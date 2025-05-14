open Cfg

let dce_changed = ref false
let last_result = ref ""
let living_map = Hashtbl.create 32

(* Dead Code Elimination *)
let rec dead_code_elimination (method_graph : Cfg.cfg) =
  (* Printf.fprintf debug_file "#RUNNING\n"; *)
  Hashtbl.reset living_map;
  dce_changed := false;
  let parse_dce (method_cfg : Cfg.cfg_elem list) =
    let rec parse_dead_code (node : cfg_elem) =
      match node with
      | Normal_Node tacs -> parse_dead tacs
      | If_Statement (cond_stmt, then_stmt, else_stmt, join_stmt) ->
          parse_dead_code cond_stmt;
          parse_dead_code then_stmt;
          parse_dead_code else_stmt;
          parse_dead_code join_stmt
      | Loop (loop_cond, loop_body, join_body) ->
          parse_dead_code loop_cond;
          parse_dead_code loop_body;
          parse_dead_code join_body
      | Cases (case_cond, case_options, case_join) ->
          parse_dead_code case_cond;
          List.iter parse_dead_code case_options;
          parse_dead_code case_join
    in
    List.iter parse_dead_code method_cfg
  in
  parse_dce method_graph.cfg;
  method_graph.cfg <- filter_dead method_graph;
  if !dce_changed then dead_code_elimination method_graph

(* Get Dead Code *)
and parse_dead (tacs : Tac.tac_elem list) : unit =
  let set_values (tac : string) =
    match Hashtbl.find_opt living_map tac with
    | Some _ ->
        (* Printf.fprintf debug_file "#\tSetting %s to true\n" tac; *)
        Hashtbl.replace living_map tac true
    | None -> (* Printf.printf "\tValue %s not found\n" tac*) ()
  in
  let modify_table (tac : Tac.tac_elem) =
    if Tac.operand_to_string tac.operand <> "comment" then (
      (* Printf.fprintf debug_file
        "# Parsing the following line: %s <- (%s) (%s) (%s)\n" tac.result
        (Tac.operand_to_string tac.operand)
        tac.arg1 tac.arg2;*)
      set_values (Tac.operand_to_string tac.operand);
      (match tac.operand with
      | Call | StaticCall _ ->
          set_values !last_result;
          List.iter set_values (String.split_on_char ' ' tac.arg2)
      | Ident_Expr v -> (
          (* Printf.fprintf debug_file "#\t IDENT\n"; *)
          set_values v;
          match Hashtbl.find_opt living_map tac.result with
          | Some _ ->
              (* Printf.fprintf debug_file "#\t VALUE %s already in map\n" tac.result*)
              ()
          | None ->
              (* Printf.fprintf debug_file "#\t ADDING VALUE %s to map as false\n"
                tac.result; *)
              Hashtbl.add living_map tac.result false)
      | _ -> (
          set_values tac.arg1;
          set_values tac.arg2;
          match Hashtbl.find_opt living_map tac.result with
          | Some _ ->
              (* Printf.fprintf debug_file "#\t VALUE %s already in map\n" tac.result *)
              ()
          | None ->
              (* Printf.fprintf debug_file "#\t ADDING VALUE %s to map as false\n"
                tac.result; *)
              Hashtbl.add living_map tac.result false));
      last_result := tac.result)
    else ()
  in
  List.iter modify_table tacs

and filter_dead cfg =
  let dead_code =
    Hashtbl.fold (fun k v acc -> (k, v) :: acc) living_map []
    |> List.filter (fun (_, v) -> not v)
    |> List.map (fun (k, _) -> k)
  in
  let alive_operand (op : Tac.tac_operand) =
    match op with
    | Bt | Call | StaticCall _ | Comment | Label | Jmp | VoidCase | EmptyCase
    | Case_Header | ClassId | Case _ | Return ->
        true
    | _ -> false
  in
  let is_alive (tac : Tac.tac_elem) : bool =
    (not (List.mem tac.result dead_code))
    || alive_operand tac.operand
    || not (String.contains tac.result '$')
  in
  (* List.iter (Printf.printf "\t -%s\n") dead_code; *)
  let rec get_filtered_node (node : cfg_elem) =
    match node with
    | Normal_Node tacs ->
        (* Printf.fprintf debug_file "PRE-FILTER: ---------------\n"; 
        Tac.print_tac_elems_file tacs debug_file; *)
        let filtered_tac =
          List.filter
            (*
            (fun f ->
              let r = is_alive f in
              if not r then
                Printf.fprintf debug_file "REMOVING LINE %s <- %s %s %s \n"
                  f.result
                  (Tac.operand_to_string f.operand)
                  f.arg1 f.arg2;
              r) *)
            is_alive tacs
        in
        (* Printf.fprintf debug_file "POST-FILTER: ---------------\n";  
        Tac.print_tac_elems_file filtered_tac debug_file;
        Printf.fprintf debug_file "\n\n"; *)
        if List.length filtered_tac != List.length tacs then dce_changed := true;
        Normal_Node filtered_tac
    | If_Statement (cond_stmt, then_stmt, else_stmt, join_stmt) ->
        let a = get_filtered_node cond_stmt in
        let b = get_filtered_node then_stmt in
        let c = get_filtered_node else_stmt in
        let d = get_filtered_node join_stmt in
        If_Statement (a, b, c, d)
    | Loop (loop_cond, loop_body, join_body) ->
        let l_cond = get_filtered_node loop_cond in
        let l_body = get_filtered_node loop_body in
        let l_join = get_filtered_node join_body in
        Loop (l_cond, l_body, l_join)
    | Cases (case_cond, case_options, case_join) ->
        let cond = get_filtered_node case_cond in
        let case_ops = List.map get_filtered_node case_options in
        let case_join = get_filtered_node case_join in
        Cases (cond, case_ops, case_join)
  in
  List.map get_filtered_node cfg.cfg

(* NOTE: Single Static Assignment *)

(* Key = Original Name; Value = SSA Name *)
let ssa_names = ref (Hashtbl.create 32)

let singe_static_assignment (method_graph : Cfg.cfg) =
  let create_new_tac (tac : Tac.tac_elem) : Tac.tac_elem =
    let result =
      match Hashtbl.find_opt !ssa_names tac.result with
      | Some v when String.contains tac.result '$' ->
          Printf.fprintf Print.debug_file "# UPDATED SSA: (%s) : (%d%s)\n"
            tac.result (v + 1) tac.result;
          Hashtbl.replace !ssa_names tac.result (v + 1);
          string_of_int (v + 1) ^ tac.result
      | None when String.contains tac.result '$' ->
          Printf.fprintf Print.debug_file "# NEW SSA: (%s) : (%d%s)\n"
            tac.result 0 tac.result;
          Hashtbl.add !ssa_names tac.result 0;
          string_of_int 0 ^ tac.result
      | _ -> tac.result
    in
    let arg1 =
      match tac.operand with
      | _ -> (
          match Hashtbl.find_opt !ssa_names tac.arg1 with
          | Some v when String.contains tac.arg1 '$' ->
              let arg1 = string_of_int v ^ tac.arg1 in
              Printf.fprintf Print.debug_file
                "# Retrieved SSA for value %s (%s)\n" tac.arg1 arg1;
              arg1
          | _ ->
              Printf.fprintf Print.debug_file
                "# Failed to find SSA for value %s\n" tac.arg1;
              tac.arg1)
    in
    let arg2 =
      match tac.operand with
      | Call | StaticCall _ ->
          String.concat " "
            (List.map
               (fun arg ->
                 match Hashtbl.find_opt !ssa_names arg with
                 | Some v when String.contains arg '$' -> string_of_int v ^ arg
                 | _ -> arg)
               (String.split_on_char ' ' tac.arg2))
      | _ -> (
          match Hashtbl.find_opt !ssa_names tac.arg2 with
          | Some v when String.contains tac.arg2 '$' ->
              let arg2 = string_of_int v ^ tac.arg2 in
              Printf.fprintf Print.debug_file
                "# Retrieved SSA for value %s (%s)\n" tac.arg2 arg2;
              arg2
          | _ ->
              Printf.fprintf Print.debug_file
                "# Failed to find SSA for value %s\n" tac.arg2;
              tac.arg2)
    in
    let (operand : Tac.tac_operand) =
      match tac.operand with
      | Ident_Expr identVal -> (
          match Hashtbl.find_opt !ssa_names identVal with
          | Some v when String.contains identVal '$' ->
              let newVal = string_of_int v ^ identVal in
              Printf.fprintf Print.debug_file
                "# Retrieved SSA for value %s (%s)\n" identVal newVal;
              Ident_Expr newVal
          | _ ->
              Printf.fprintf Print.debug_file
                "# Failed to find SSA for value %s\n" identVal;
              Ident_Expr identVal)
      | _ ->
          Printf.fprintf Print.debug_file
            "# Failed to find SSA for tac with operand %s\n"
            (Tac.operand_to_string tac.operand);
          tac.operand
    in
    {
      operand;
      arg1;
      arg2;
      result;
      line = tac.line;
      static_type = tac.static_type;
    }
  in

  (* Converts a basic block into SSA form and return the variables modified *)
  let ssaify_tac (tacs : Tac.tac_elem list) = List.map create_new_tac tacs in
  (* let get_hash_values () =
    Hashtbl.fold (fun k v acc -> (k, v) :: acc) !ssa_names []
  in*)
  (* let phi changes_one changes_two merging_hash :
      Tac.tac_elem list * Tac.tac_elem list =
    (*

[Ocaml Wiki](https://ocaml.org/manual/5.3/api/List.html)
partition f l returns a pair of lists (l1, l2), where l1 is the list of all the elements of l that satisfy the predicate f, and l2 is the list of all the elements of l that do not satisfy f. The order of the elements in the input list is preserved.
*)
    List.iter
      (fun (k, v) -> Printf.fprintf Print.debug_file "\tONE: Value %s (%d)\n" k v)
      changes_one;
    List.iter
      (fun (k, v) -> Printf.fprintf Print.debug_file "\tTWO: Value %s (%d)\n" k v)
      changes_two;
    let pred1 f = List.mem f changes_two in
    let pred2 f = List.mem f changes_one in
    (* A list of all key-value pairs of list_one not also in list_two *)
    let samesies, unmatched_one = List.partition pred1 changes_one in

    List.iter
      (fun (k, v) -> Printf.fprintf Print.debug_file "\tSAMESIES: Value %s (%d)\n" k v)
      samesies;
    List.iter
      (fun (k, v) -> Printf.fprintf Print.debug_file "\tUNMATCHED: Value %s (%d)\n" k v)
      unmatched_one;
    (* A list of all key-value pairs of list_two not also in list_one *)
    let samesies, unmatched_two = List.partition pred2 changes_two in
    List.iter
      (fun (k, v) -> Printf.fprintf Print.debug_file "\tSAMESIES: Value %s (%d)\n" k v)
      samesies;
    List.iter
      (fun (k, v) -> Printf.fprintf Print.debug_file "\tUNMATCHED: Value %s (%d)\n" k v)
      unmatched_two;

    let changed_values = unmatched_one @ unmatched_two in
    Printf.fprintf Print.debug_file "JOINING %d VALUES: ---------------\n"
      (List.length changed_values);
    List.iter
      (fun (k, v) -> Printf.fprintf Print.debug_file "\tJOINING: Value %s (%d)\n" k v)
      changed_values;
    let original_spots_one =
      List.map
        (fun ((f : string), _) ->
          let keys, values = List.split unmatched_one in
          if List.mem f keys then (f, List.find (fun k -> k = f) keys)
          else ("", ""))
        changed_values
    in
    List.iter
      (fun (k, v) ->
        match Hashtbl.find_opt merging_hash k with
        | Some currVal when currVal < v -> Hashtbl.add merging_hash k (v + 1)
        | None -> Hashtbl.add merging_hash k (v + 1)
        | _ -> ())
      changed_values;
    let new_spots =
      List.map (fun (f, _) -> (f, Hashtbl.find merging_hash f)) changed_values
    in
    ssa_names := merging_hash;
    let merge_one =
      List.map
        (fun ((k1, v1), (k2, v2)) : Tac.tac_elem ->
          let result = string_of_int v1 ^ k1 in
          let arg1 = string_of_int v2 ^ k2 in
          {
            operand = Assignment;
            result;
            arg1;
            arg2 = "";
            line = 0;
            static_type = None;
          })
        (List.combine original_spots_one new_spots)
    in
    let merge_two =
      List.map
        (fun ((k1, v1), (k2, v2)) : Tac.tac_elem ->
          let result = string_of_int v1 ^ k1 in
          let arg1 = string_of_int v2 ^ k2 in
          {
            operand = Assignment;
            result;
            arg1;
            arg2 = "";
            line = 0;
            static_type = None;
          })
        (List.combine original_spots_two new_spots)
    in
    (merge_one, merge_two)
  in *)
  let rec ssaify_cfg_node (elem : cfg_elem) : cfg_elem =
    match elem with
    | Normal_Node tacs ->
        let new_tac = ssaify_tac tacs in
        Normal_Node new_tac
    | If_Statement (cond_stmt, then_stmt, else_stmt, join_stmt) ->
        let new_cond = ssaify_cfg_node cond_stmt in
        (* let cond_hash = Hashtbl.copy !ssa_names in *)
        let new_then = ssaify_cfg_node then_stmt in
        (*  let then_hash = get_hash_values () in *)
        let new_else = ssaify_cfg_node else_stmt in
        (* let else_hash = get_hash_values () in *)
        (* PHI:  Get diff of ssa_then and ssa_else, create list of changed values *)
        (* phi then_hash else_hash cond_hash;*)
        let new_join = ssaify_cfg_node join_stmt in
        If_Statement (new_cond, new_then, new_else, new_join)
    | Loop (cond_stmt, body_stmt, join_stmt) ->
        let new_cond = ssaify_cfg_node cond_stmt in
        let new_body = ssaify_cfg_node body_stmt in
        let new_join = ssaify_cfg_node join_stmt in
        Loop (new_cond, new_body, new_join)
    | Cases (cond_stmt, case_options, join_stmt) ->
        let new_cond = ssaify_cfg_node cond_stmt in
        let new_options = List.map ssaify_cfg_node case_options in
        let new_join = ssaify_cfg_node join_stmt in
        Cases (new_cond, new_options, new_join)
  in
  method_graph.cfg <- List.map ssaify_cfg_node method_graph.cfg

let print_optimization_comparison () =
  let opt_file = open_out "./optimized.cl-tac" in
  let unopt_file = open_out "./unoptimized.cl-tac" in
  let node = !Cfg.cfg_list |> List.hd in

  let tac = Cfg.get_method_tac node.cfg in

  List.iter
    (fun elem -> Printf.fprintf unopt_file "%s\n" (Tac.get_tac_elem elem))
    tac;

  dead_code_elimination node;

  List.iter
    (fun elem -> Printf.fprintf opt_file "%s\n" (Tac.get_tac_elem elem))
    (Cfg.get_method_tac node.cfg)
