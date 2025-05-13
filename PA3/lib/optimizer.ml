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
    let rec parse_cfg (node : cfg_elem) =
      match node with
      | Normal_Node tacs -> parse_dead tacs
      | If_Statement (cond_stmt, then_stmt, else_stmt, join_stmt) ->
          (*    let a = parse_cfg cond_stmt in
          let b = parse_cfg then_stmt in
          let c = parse_cfg else_stmt in
          let d = parse_cfg join_stmt in
          If_Statement (a, b, c, d) *)
          parse_cfg cond_stmt;
          parse_cfg then_stmt;
          parse_cfg else_stmt;
          parse_cfg join_stmt
      | Loop (loop_cond, loop_body, join_body) ->
          (* let l_cond = parse_cfg loop_cond in
          let l_body = parse_cfg loop_body in
          let l_join = parse_cfg join_body in
          Loop (l_cond, l_body, l_join) *)
          parse_cfg loop_cond;
          parse_cfg loop_body;
          parse_cfg join_body
      | Cases (case_cond, case_options, case_join) ->
          (* let cond = parse_cfg case_cond in
          let case_ops = List.map parse_cfg case_options in
          let case_join = parse_cfg case_join in
          Cases (cond, case_ops, case_join) *)
          parse_cfg case_cond;
          List.iter parse_cfg case_options;
          parse_cfg case_join
    in
    List.iter parse_cfg method_cfg
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
    | Bt | Call | StaticCall _ | Label | Jmp | VoidCase | EmptyCase | ClassId
    | Comment | Case_Header | Return ->
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

let print_optimization_comparison () =
  let opt_file = open_out "./optimized.cl-tac" in
  let unopt_file = open_out "./unoptimized.cl-tac" in
  let node = !Cfg.cfg_list |> List.hd in

  List.iter
    (fun elem -> Printf.fprintf unopt_file "%s\n" (Tac.get_tac_elem elem))
    (Cfg.get_method_tac node.cfg);

  List.iter
    (fun elem -> Printf.fprintf opt_file "%s\n" (Tac.get_tac_elem elem))
    (Cfg.get_method_tac node.cfg)
