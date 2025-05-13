open Cfg

(* 
type cfg_elem =
  (* Basic Block *)
  | Normal_Node of tac_elem list
  (* Condition * Then Stmt * Else Stmt * Join  *)
  | If_Statement of cfg_elem * cfg_elem * cfg_elem * cfg_elem
  (* Condition * Body * Join *)
  | Loop of cfg_elem * cfg_elem * cfg_elem
  (* Case Expr * Cases * Join *)
  | Cases of cfg_elem * cfg_elem list * cfg_elem

and cfg_node = { data : cfg_elem; next : cfg_node option }
and cfg = {
  cfg_root : cfg_node;
  class_name : string;
  method_name : string;
  arguments : Parser.ast_formal list;
  temp_count : int;
}
*)

let dce_worked = ref false

(* Dead Code Elimination *)
let rec dead_code_elimination (method_cfg : Cfg.cfg_elem list) =
  let rec parse_cfg (node : cfg_elem) : cfg_elem =
    match node with
    | Normal_Node tacs -> localDCE tacs
    | If_Statement (cond_stmt, then_stmt, else_stmt, join_stmt) ->
        let a = parse_cfg cond_stmt in
        let b = parse_cfg then_stmt in
        let c = parse_cfg else_stmt in
        let d = parse_cfg join_stmt in
        If_Statement (a, b, c, d)
    | Loop (loop_cond, loop_body, join_body) ->
        let l_cond = parse_cfg loop_cond in
        let l_body = parse_cfg loop_body in
        let l_join = parse_cfg join_body in
        Loop (l_cond, l_body, l_join)
    | Cases (case_cond, case_options, case_join) ->
        let cond = parse_cfg case_cond in
        let case_ops = List.map parse_cfg case_options in
        let case_join = parse_cfg case_join in
        Cases (cond, case_ops, case_join)
  in
  List.map parse_cfg method_cfg

(* Get Dead Code *)
and localDCE (tacs : Tac.tac_elem list) : cfg_elem =
  let living_map = Hashtbl.create 32 in
  let set_values (tac : string) =
    match Hashtbl.find_opt living_map tac with
    | Some _ ->
        (* Printf.printf "\tSetting %s to true\n" tac; *)
        Hashtbl.replace living_map tac true
    | None -> ()
  in
  let modify_table (tac : Tac.tac_elem) =
    if Tac.operand_to_string tac.operand <> "comment" then (
      (* Printf.printf "Parsing the following line: %s <- 1.%s 2.%s 3.%s\n"
        tac.result
        (Tac.operand_to_string tac.operand)
        tac.arg1 tac.arg2; *)
      set_values (Tac.operand_to_string tac.operand);
      set_values tac.arg1;
      set_values tac.arg2;
      match Hashtbl.find_opt living_map tac.result with
      | Some _ -> ()
      | None -> Hashtbl.add living_map tac.result false)
    else ()
    (* | Some _ ->
        (* Printf.printf "\tSetting %s to true\n" tac.result; *)
        Hashtbl.add living_map tac.result true
    | None ->
        (* Printf.printf "\tSetting %s to false\n" tac.result; *)
        Hashtbl.add living_map tac.result false *)
  in
  List.iter modify_table tacs;
  let dead_code =
    Hashtbl.fold (fun k v acc -> (k, v) :: acc) living_map []
    |> List.filter (fun (_, v) -> not v)
    |> List.map (fun (k, _) -> k)
  in
  let is_alive (tac : Tac.tac_elem) : bool =
    ((not (List.mem tac.result dead_code))
    || List.mem tac.operand
         [ Bt; Call; Label; Jmp; VoidCase; EmptyCase; Return ])
    || not (String.contains tac.result '$')
  in
  let filtered_tac = List.filter is_alive tacs in
  if List.length filtered_tac == List.length tacs then Normal_Node filtered_tac
  else localDCE filtered_tac

(* let rec parse_elem node =
    match node with
    | Normal_Node tacs -> List.iter modify_table tacs
    | If_Statement (cond_stmt, then_stmt, else_stmt, join_stmt) ->
        parse_elem cond_stmt;
        parse_elem then_stmt;
        parse_elem else_stmt;
        parse_elem join_stmt
    | Loop (loop_cond, loop_body, join_body) ->
        parse_elem loop_cond;
        parse_elem loop_body;
        parse_elem join_body
    | Cases (case_cond, case_options, case_join) ->
        parse_elem case_cond;
        List.iter parse_elem case_options;
        parse_elem case_join
  in
  List.iter parse_elem cfg_nodes; 
  Hashtbl.fold (fun k v acc -> (k, v) :: acc) living_map []
  |> List.filter (fun (_, v) -> not v)
  |> List.map (fun (k, _) -> k)*)

let print_optimization_comparison () =
  let opt_file = open_out "./optimized.cl-tac" in
  let unopt_file = open_out "./unoptimized.cl-tac" in
  let node = Cfg.cfg_list |> List.hd in

  List.iter
    (fun elem -> Printf.fprintf unopt_file "%s\n" (Tac.get_tac_elem elem))
    (Cfg.get_method_tac node.cfg);

  node.cfg <- dead_code_elimination node.cfg;

  List.iter
    (fun elem -> Printf.fprintf opt_file "%s\n" (Tac.get_tac_elem elem))
    (Cfg.get_method_tac node.cfg)

(* 
  let getDead cfg_node : string list =
  let living_map = Hashtbl.create 32 in
  let modify_table (tac : Tac.tac_elem) =
    match Hashtbl.find_opt living_map tac.result with
    | Some _ -> Hashtbl.add living_map tac.result true
    | None -> Hashtbl.add living_map tac.result false
  in
  let parse_line node acc =
    match cfg_node.next with
    | Some data ->
        modify_table node.data;
        getDead data acc
    | None -> acc
  in
  parse_line cfg_node
*)
