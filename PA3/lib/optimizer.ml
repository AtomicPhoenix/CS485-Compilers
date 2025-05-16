open Cfg

let dce_changed = ref false
let last_result = ref ""
let living_map = Hashtbl.create 32

let simplify_cfg (method_graph : Cfg.cfg) =
  let simplify_tac (tacs : Tac.tac_elem list) =
    let value_map = Hashtbl.create 32 in
    let get_ident str =
      match Hashtbl.find_opt value_map str with
      | Some v when v.[0] = 't' && v.[0] = '$' -> v
      | _ -> str
    in
    let get_tac_value (tac : Tac.tac_elem) : Tac.tac_elem =
      match tac.operand with
      | Plus | Minus | Times | Divide | LessThan | LessEqual | Equal | Isvoid
      | Negate | Not | Int_Constant | Boolean_Constant | String_Constant -> (
          let arg1 = get_ident tac.arg1 in
          let arg2 = get_ident tac.arg2 in
          let rhs =
            Tac.operand_to_string tac.operand ^ " " ^ tac.arg1 ^ " " ^ tac.arg2
          in
          match Hashtbl.find_opt value_map rhs with
          | None ->
              Hashtbl.add value_map rhs tac.result;
              {
                operand = tac.operand;
                arg1;
                arg2;
                result = tac.result;
                line = tac.line;
                static_type = tac.static_type;
              }
          | Some v ->
              {
                operand = Ident_Expr v;
                arg1 = "";
                arg2 = "";
                result = tac.result;
                line = tac.line;
                static_type = tac.static_type;
              })
      | Call | StaticCall _ ->
          let arg_list = String.split_on_char ' ' tac.arg2 in
          let new_arg_list = List.map get_ident arg_list in
          let arg2 = String.concat " " new_arg_list in
          {
            operand = tac.operand;
            arg1 = tac.arg1;
            arg2;
            result = tac.result;
            line = tac.line;
            static_type = tac.static_type;
          }
      | Ident_Expr v ->
          (match Hashtbl.find_opt value_map tac.result with
          | None when (tac.result).[0] = 't' && (tac.result).[1] = '$'  && v.[0] = 't' && v.[1] = '$'->
              Hashtbl.add value_map tac.result v
          | _ -> ());
          tac
      | _ -> tac
    in
    List.map get_tac_value tacs
  in
  let rec simplify_cfg_elem (node : cfg_elem) =
    match node with
    | Normal_Node tacs -> Normal_Node (simplify_tac tacs)
    | If_Statement (cond_stmt, then_stmt, else_stmt, join_stmt, phi) ->
        let new_cond = simplify_cfg_elem cond_stmt in
        let new_then = simplify_cfg_elem then_stmt in
        let new_else = simplify_cfg_elem else_stmt in
        let new_join = simplify_cfg_elem join_stmt in
        If_Statement (new_cond, new_then, new_else, new_join, phi)
    | Loop (cond_stmt, body_stmt, join_stmt) ->
        let new_cond = simplify_cfg_elem cond_stmt in
        let new_body = simplify_cfg_elem body_stmt in
        let new_join = simplify_cfg_elem join_stmt in
        Loop (new_cond, new_body, new_join)
    | Cases (cond_stmt, case_options, join_stmt) ->
        let new_cond = simplify_cfg_elem cond_stmt in
        let new_options = List.map simplify_cfg_elem case_options in
        let new_join = simplify_cfg_elem join_stmt in
        Cases (new_cond, new_options, new_join)
  in
  method_graph.cfg <- List.map simplify_cfg_elem method_graph.cfg

(* Dead Code Elimination *)
let rec dead_code_elimination (method_graph : Cfg.cfg) =
  Hashtbl.reset living_map;
  dce_changed := false;
  let parse_dce (method_cfg : Cfg.cfg_elem list) =
    let rec parse_dead_code (node : cfg_elem) =
      match node with
      | Normal_Node tacs -> parse_dead tacs
      | If_Statement (cond_stmt, then_stmt, else_stmt, join_stmt, _) ->
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

and dead_code_elimination_2_electic_boogaloo (method_graph : Cfg.cfg) =
  Hashtbl.reset living_map;
  dce_changed := false;
  let parse_dce (method_cfg : Cfg.cfg_elem list) =
    let rec parse_dead_code (node : cfg_elem) =
      match node with
      | Normal_Node tacs -> parse_dead tacs
      | If_Statement (cond_stmt, then_stmt, else_stmt, join_stmt, _) ->
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
  method_graph.cfg <- filter_dead_2_electric_boogaloo method_graph;
  if !dce_changed then dead_code_elimination_2_electic_boogaloo method_graph

(* Get Dead Code *)
and parse_dead (tacs : Tac.tac_elem list) : unit =
  let set_values (tac : string) =
    match Hashtbl.find_opt living_map tac with
    | Some _ ->
        Hashtbl.replace living_map tac true
    | None ->  ()
  in
  let modify_table (tac : Tac.tac_elem) =
    if Tac.operand_to_string tac.operand <> "comment" then (
      set_values (Tac.operand_to_string tac.operand);
      (match tac.operand with
      | Call | StaticCall _ ->
          set_values !last_result;
          List.iter set_values (String.split_on_char ' ' tac.arg2)
      | Ident_Expr v -> (
          set_values v;
          match Hashtbl.find_opt living_map tac.result with
          | Some _ ->
              ()
          | None ->
              Hashtbl.add living_map tac.result false)
      | _ -> (
          set_values tac.arg1;
          set_values tac.arg2;
          match Hashtbl.find_opt living_map tac.result with
          | Some _ ->
              ()
          | None ->
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
    || not ((tac.result).[0] = 't' && (tac.result).[1] = '$')
  in
  
  let rec get_filtered_node (node : cfg_elem) =
    match node with
    | Normal_Node tacs ->
        let filtered_tac =
          List.filter
            is_alive tacs
        in
        if List.length filtered_tac != List.length tacs then dce_changed := true;
        Normal_Node filtered_tac
    | If_Statement (cond_stmt, then_stmt, else_stmt, join_stmt, phi) ->
        let cond_stmt = get_filtered_node cond_stmt in
        let then_stmt = get_filtered_node then_stmt in
        let else_stmt = get_filtered_node else_stmt in
        let join_stmt = get_filtered_node join_stmt in
        If_Statement (cond_stmt, then_stmt, else_stmt, join_stmt, phi)
    | Loop (l_cond, l_body, l_join) ->
        let l_cond = get_filtered_node l_cond in
        let l_body = get_filtered_node l_body in
        let l_join = get_filtered_node l_join in
        Loop (l_cond, l_body, l_join)
    | Cases (cond, case_options, case_join) ->
        let cond = get_filtered_node cond in
        let case_ops = List.map get_filtered_node case_options in
        let case_join = get_filtered_node case_join in
        Cases (cond, case_ops, case_join)
  in
  List.map get_filtered_node cfg.cfg

and filter_dead_2_electric_boogaloo cfg =
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
    || not ((*String.contains tac.result '$'*)(tac.result).[0] = 't' && (tac.result).[1] = '$')
  in
  (* List.iter (Printf.printf "\t -%s\n") dead_code; *)
  let get_filtered_node (node : cfg_elem) =
    match node with
    | Normal_Node tacs ->
        let filtered_tac =
          List.filter
            is_alive tacs
        in
        if List.length filtered_tac != List.length tacs then dce_changed := true;
        Normal_Node filtered_tac
    | If_Statement (cond_stmt, then_stmt, else_stmt, join_stmt, phi) ->
        If_Statement (cond_stmt, then_stmt, else_stmt, join_stmt, phi)
    | Loop (l_cond, loop_body, l_join) ->
        Loop (l_cond, loop_body, l_join)
    | Cases (cond, case_options, case_join) ->
        Cases (cond, case_options, case_join)
  in
  List.map get_filtered_node cfg.cfg

(* NOTE: Single Static Assignment *)

(* Key = Original Name; Value = SSA Name *)
let ssa_names = ref (Hashtbl.create 32)

let revert_string s =
  let contains_t = String.contains s 't' && String.contains s '$' in
  if not contains_t then s
  else
    let len = String.length s in
    let rec find_first_non_digit i =
      if i >= len then i
      else if s.[i] >= '0' && s.[i] <= '9' then find_first_non_digit (i + 1)
      else i
    in
    let start = find_first_non_digit 0 in
    String.sub s start (len - start)

let undo_singe_static_assignment (method_graph : Cfg.cfg) =
  let unssaify (tac : Tac.tac_elem) : Tac.tac_elem =
    let (operand : Tac.tac_operand) =
      match tac.operand with
      | Ident_Expr identVal -> Ident_Expr (revert_string identVal)
      | _ -> tac.operand
    in
    let arg1 =
      match tac.operand with
      | Label | Comment | Jmp | Bt -> tac.arg1
      | _ -> revert_string tac.arg1
    in
    let arg2 =
      match tac.operand with
      | Label | Comment | Jmp | Bt -> tac.arg2
      | Call | StaticCall _ ->
          String.concat " "
            (List.map revert_string (String.split_on_char ' ' tac.arg2))
      | _ -> revert_string tac.arg2
    in
    let result =
      match tac.operand with
          | Comment | Label | Jmp | Bt -> tac.result
          | _ -> revert_string tac.result
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
  let unssaify_tac (tacs : Tac.tac_elem list) = List.map unssaify tacs in
  let rec unssaify_cfg_node (elem : cfg_elem) : cfg_elem =
    match elem with
    | Normal_Node tacs ->
        let new_tac = unssaify_tac tacs in
        Normal_Node new_tac
    | If_Statement (cond_stmt, then_stmt, else_stmt, join_stmt, phi) ->
        let new_cond = unssaify_cfg_node cond_stmt in
        (* let cond_hash = Hashtbl.copy !ssa_names in *)
        let new_then = unssaify_cfg_node then_stmt in
        let new_else = unssaify_cfg_node else_stmt in
        let new_join = unssaify_cfg_node join_stmt in
        If_Statement (new_cond, new_then, new_else, new_join, phi)
    | Loop (cond_stmt, body_stmt, join_stmt) ->
        let new_cond = unssaify_cfg_node cond_stmt in
        let new_body = unssaify_cfg_node body_stmt in
        let new_join = unssaify_cfg_node join_stmt in
        Loop (new_cond, new_body, new_join)
    | Cases (cond_stmt, case_options, join_stmt) ->
        let new_cond = unssaify_cfg_node cond_stmt in
        let new_options = List.map unssaify_cfg_node case_options in
        let new_join = unssaify_cfg_node join_stmt in
        Cases (new_cond, new_options, new_join)
  in
  method_graph.cfg <- List.map unssaify_cfg_node method_graph.cfg

let singe_static_assignment (method_graph : Cfg.cfg) =
  let create_new_tac (tac : Tac.tac_elem) : Tac.tac_elem =
    let arg1 =
      match tac.operand with
      | Label | Comment | Jmp | Bt -> tac.arg1
      | _ -> (
          match Hashtbl.find_opt !ssa_names tac.arg1 with
          | Some v when String.contains tac.arg1 '$' && String.contains tac.arg1 't' ->
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
      | Label | Comment | Jmp | Bt -> tac.arg2
      | Call | StaticCall _ ->
          String.concat " "
            (List.map
               (fun arg ->
                 match Hashtbl.find_opt !ssa_names arg with
                 | Some v when String.contains arg '$' && String.contains arg 't' -> string_of_int v ^ arg
                 | _ -> arg)
               (String.split_on_char ' ' tac.arg2))
      | _ -> (
          match Hashtbl.find_opt !ssa_names tac.arg2 with
          | Some v when (*String.contains tac.arg2 '$'*)String.length (tac.arg2) > 1 && (tac.arg2).[1] = '$' ->
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
      | Label | Comment | Jmp | Bt -> tac.operand
      | Ident_Expr identVal -> (
          match Hashtbl.find_opt !ssa_names identVal with
          | Some v when String.contains identVal '$' && String.contains identVal 't' ->
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
    let result =
      match tac.operand with
      | Comment | Label | Jmp | Bt -> tac.result
      | _ ->
      match Hashtbl.find_opt !ssa_names tac.result with
      | Some v when String.contains tac.result '$' && String.contains tac.result 't'  -> (
              Printf.fprintf Print.debug_file
                "# UPDATED SSA: (%s) : (%d%s) FOR TAC: %s <- %s %s %s \n"
                tac.result (v + 1) tac.result tac.result
                (Tac.operand_to_string tac.operand)
                tac.arg1 tac.arg2;
              Hashtbl.replace !ssa_names tac.result (v + 1);
              string_of_int (v + 1) ^ tac.result)
      | None when String.contains tac.result '$' && String.contains tac.result 't'->
          Printf.fprintf Print.debug_file
            "# NEW SSA: (%s) : (%d%s) FOR TAC: %s <- %s %s %s\n" tac.result 0
            tac.result tac.result
            (Tac.operand_to_string tac.operand)
            tac.arg1 tac.arg2;
          Hashtbl.add !ssa_names tac.result 0;
          string_of_int 0 ^ tac.result
      | _ -> tac.result
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
  let rec insert_tac (node : cfg_elem) (tacs : Tac.tac_elem list) =
    match node with
    | Normal_Node old_tacs -> (
        let rev_tac = List.rev old_tacs in
        match rev_tac with
        | jmp_tac :: revList ->
            Normal_Node (List.rev revList @ tacs @ [ jmp_tac ])
        | [] -> Normal_Node tacs)
    | If_Statement (i, t, e, join_stmt, p) -> let ret = If_Statement(insert_tac i tacs, insert_tac t tacs, insert_tac e tacs, insert_tac join_stmt tacs, p) in ret
        
    | Loop (_, _, join_stmt) -> insert_tac join_stmt tacs
    | Cases (_, _, join_stmt) -> insert_tac join_stmt tacs
  in
  let get_hash_values () =
    Hashtbl.fold (fun k v acc -> (k, v) :: acc) !ssa_names []
  in
  let get_changed (original_values : (string * int) list) =
    let new_values =
      Hashtbl.fold (fun k v acc -> (k, v) :: acc) !ssa_names []
    in
    let pred1 f = List.mem f original_values in
    let pred2 f = List.mem f new_values in

    let _, unmatched_one = List.partition pred2 original_values in
    let _, unmatched_two = List.partition pred1 new_values in
    let changed_values = unmatched_one @ unmatched_two in
    Printf.fprintf Print.debug_file "# JOINING %d VALUES: ---------------\n"
      (List.length changed_values);
    List.iter
      (fun (k, v) ->
        Printf.fprintf Print.debug_file
          "# \tJOINING from Branch 1: Value %s (%d)\n" k v)
      unmatched_one;
    List.iter
      (fun (k, v) ->
        Printf.fprintf Print.debug_file
          "# \tJOINING from Branch 2: Value %s (%d)\n" k v)
      unmatched_two;
    (* Get names of all varibales whose values changed *)
    let unique_changes =
      List.map (fun (k, _) -> k) changed_values |> List.sort_uniq compare
      (* Get maximum SSA identifer of all varibales whose values changed *)
    in
    let get_max (var : string) : int =
      List.filter_map
        (fun (k, v) -> if k = var then Some v else None)
        changed_values
      |> List.fold_left max 0
    in
    let join_values = List.map (fun k -> (k, get_max k + 1)) unique_changes in
    let get_join_tac (var : string) (binding_list : (string * int) list) :
        Tac.tac_elem =
      let result =
        string_of_int (List.find (fun (k, _) -> k = var) join_values |> snd)
        ^ var
      in
      match List.find_opt (fun (k, _) -> k = var) binding_list with
      | Some (_, v) ->
          let arg1 = string_of_int v ^ var in
          {
            operand = Assignment;
            result;
            arg1;
            arg2 = "";
            line = 0;
            static_type = None;
          }
      | None ->
          {
            operand = Assignment;
            result;
            arg1 = "0";
            arg2 = "";
            line = 0;
            static_type = None;
          }
    in
    let hashes = [ unmatched_one; unmatched_two ] in
    List.map (fun key -> List.map (get_join_tac key) hashes) unique_changes
  in

  let rec ssaify_cfg_node (elem : cfg_elem) : cfg_elem =
    match elem with
    | Normal_Node tacs ->
        let new_tac = ssaify_tac tacs in
        Normal_Node new_tac
    | If_Statement (cond_stmt, then_stmt, else_stmt, join_stmt, phi) ->
        let new_cond = ssaify_cfg_node cond_stmt in
        (* let cond_hash = Hashtbl.copy !ssa_names in *)
        let new_then = ssaify_cfg_node then_stmt in
        let then_hash = get_hash_values () in
        let new_else = ssaify_cfg_node else_stmt in
        let changed_values =
          get_changed then_hash
          |> List.map (fun lst -> (List.hd lst, List.nth lst 1))
        in
        let b1, b2 = List.split changed_values in
        let new_then = insert_tac new_then b1 in
        let new_else = insert_tac new_else b2 in
        let new_join = ssaify_cfg_node join_stmt in
        If_Statement (new_cond, new_then, new_else, new_join, phi)
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

let is_true (elem : cfg_elem) =
  let rec parse_true (elem : cfg_elem) =
    match elem with
    | Normal_Node tacs -> (
        if List.length tacs < 2 then ""
        else
          let second_to_last_tac = List.nth (List.rev tacs) 2 in
          match second_to_last_tac.operand with
          | Boolean_Constant ->
              second_to_last_tac.arg1
          | _ ->
              "")
    | If_Statement (_, _, _, join_stmt, _) -> parse_true join_stmt
    | Loop (_, _, join_stmt) -> parse_true join_stmt
    | Cases (_, _, join_stmt) -> parse_true join_stmt
  in
  parse_true elem

let rec remove_jump (node : cfg_elem) =
  match node with
  | Normal_Node old_tacs ->
      let length = List.length old_tacs in
      Normal_Node (List.filteri (fun i _ -> i < length - 2) old_tacs)
  | If_Statement (_, _, _, join_stmt, _) -> 
    remove_jump join_stmt
  | Loop (_, _, join_stmt) -> remove_jump join_stmt
  | Cases (_, _, join_stmt) -> remove_jump join_stmt

let remove_self_assigns (method_graph : Cfg.cfg) =
  let rec clean_elem (elem : cfg_elem) =
    match elem with
    | Normal_Node tacs ->
        let new_tac =
          List.filter_map
            (fun (tac : Tac.tac_elem) ->
              match tac.operand with
              | Assignment ->
                  if tac.arg1 = tac.result && tac.arg2 = "" then None
                  else Some tac
              | _ -> Some tac)
            tacs
        in
        Normal_Node new_tac
    | If_Statement (cond_stmt, then_stmt, else_stmt, join_stmt, phi) -> (
        let new_cond = clean_elem cond_stmt in
        let constant_branch = is_true new_cond in
        match constant_branch with
        | "true" ->
            let new_then = clean_elem then_stmt in
            let new_join = clean_elem join_stmt in
            If_Statement
              (remove_jump new_cond, new_then, Normal_Node [], new_join, phi)
        | "false" ->
            let new_else = clean_elem else_stmt in
            let new_join = clean_elem join_stmt in
            If_Statement
              (remove_jump new_cond, Normal_Node [], new_else, new_join, phi)
        | _ ->

            let new_then = clean_elem then_stmt in
            let new_else = clean_elem else_stmt in
            let new_join = clean_elem join_stmt in
            If_Statement (new_cond, new_then, new_else, new_join, phi))
    | Loop (cond_stmt, body_stmt, join_stmt) -> (
        let new_cond = clean_elem cond_stmt in
        let constant_branch = is_true new_cond in
        match constant_branch with
        | "false" ->
            let new_join = clean_elem join_stmt in
            Loop (remove_jump new_cond, Normal_Node [], new_join)
        | _ ->
            let new_body = clean_elem body_stmt in
            let new_join = clean_elem join_stmt in
            Loop (new_cond, new_body, new_join))
    | Cases (cond_stmt, case_options, join_stmt) ->
        let new_cond = clean_elem cond_stmt in
        let new_options = List.map clean_elem case_options in
        let new_join = clean_elem join_stmt in
        Cases (new_cond, new_options, new_join)
  in
  method_graph.cfg <- List.map clean_elem method_graph.cfg

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
