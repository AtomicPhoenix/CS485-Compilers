open Tac

(* Traverse AST to find parts where the control flow changes *)
(* 
    - Conditional Statements
    - Loops
    - Function Calls
    - Start and end of a method call
*)

(* 
  - Sequential statements stored normally in a basic_block
  - If statement stored as: Cond -> if   -> end
                                 -> else ->
  - Loops stored as: Cond -> LoopBody
                      |   <- 
                      v 
                     Exit
*)

type cfg_elem =
  (* Basic Block *)
  | Normal_Node of tac_elem list
  (* Condition * Then Stmt * Else Stmt * Join * phi_elem Functions *)
  | If_Statement of cfg_elem * cfg_elem * cfg_elem * cfg_elem * phi_elem list
  (* Condition * Body * Join *)
  | Loop of cfg_elem * cfg_elem * cfg_elem
  (* Case Expr * Cases * Join *)
  | Cases of cfg_elem * cfg_elem list * cfg_elem

and basic_block_label =
  | If_Cond
  | If_Then
  | If_Else
  | If_Join
  | Case_Expr
  | Case_Stmt
  | Case_Result
  | Case_Join
  | While_Cond
  | While_Body
  | While_Join
  | Normal

and labelled_basic_block = { label : basic_block_label; block : tac_elem list }
(* and cfg_node = { data : cfg_elem; next : cfg_node option } *)

and phi_elem = string * string * string list

and cfg = {
  (*  cfg_root : cfg_node; *)
  mutable cfg : cfg_elem list;
  class_name : string;
  method_name : string;
  arguments : Parser.ast_formal list;
  temp_count : int;
}

(* Checks if a given tac element is a break point in the CFG *)
let is_break_point (tac : tac_elem) =
  match tac.operand with
  | Bt | Jmp | Case _ | EmptyCase | VoidCase -> true
  | _ -> false

let get_next_cfg_elem cfg_base =
  let tacs = cfg_base |> List.flatten in
  let rec get_next cfg_base =
    match cfg_base with
    | [] -> 0
    | hd :: _ when hd.arg1 = "If-Cond" -> 1
    | hd :: _ when hd.arg1 = "While-Pred" -> 2
    | hd :: _ when hd.arg1 = "Case-Start" -> 3
    | _ :: tail -> get_next tail
  in
  get_next tacs

let print_cfg file (cfg_param : cfg) =
  let rec print_cfg_elem elem =
    match elem with
    | Loop (cond, while_body, join_body) ->
        Printf.fprintf file "#-----------While Stmt Condition:-----------\n";
        print_cfg_elem cond;
        Printf.fprintf file "#-----------While Stmt Body:-----------\n";
        print_cfg_elem while_body;
        Printf.fprintf file "#-----------While Stmt Join:-----------\n";
        print_cfg_elem join_body
    | If_Statement (cond, then_body, else_body, join_body, _) ->
        Printf.fprintf file
          "#---------------If Stmt Condition:-----------------------\n";
        print_cfg_elem cond;
        Printf.fprintf file
          "#---------------If Stmt Then:-----------------------\n";
        print_cfg_elem then_body;
        Printf.fprintf file
          "#---------------If Stmt Else:-----------------------\n";
        print_cfg_elem else_body;
        Printf.fprintf file
          "#---------------If Stmt Join:-----------------------\n";
        print_cfg_elem join_body
    | Cases (exp, cases, join) ->
        Printf.fprintf file "#-----------Cases Start:-----------\n";
        print_cfg_elem exp;
        List.iteri
          (fun i case ->
            Printf.fprintf file "#--------------Case %d:---------------\n" i;
            print_cfg_elem case)
          cases;
        Printf.fprintf file "#-----------Cases Join:-----------\n";
        print_cfg_elem join
    | Normal_Node elems -> print_tac_elems_commented elems
  in
  List.iter print_cfg_elem cfg_param.cfg

let get_label tacs =
  let find_label lbl =
    match List.find_opt (fun f -> f.arg1 = lbl) tacs with
    | Some _ -> true
    | None -> false
  in
  let find_op lbl =
    match List.find_opt (fun f -> f.operand = lbl) tacs with
    | Some _ -> true
    | None -> false
  in
  if find_label "If-Cond" then If_Cond
  else if find_label "If-Then" then If_Then
  else if find_label "If-Else" then If_Else
  else if find_label "If-Join" then If_Join
  else if find_label "While-Cond" then While_Cond
  else if find_label "While-Body" then While_Body
  else if find_label "While-Join" then While_Join
  else if find_label "Case-Expr" then Case_Expr
  else if find_label "Case-Stmt" then Case_Stmt
  else if find_label "Case-Result" then Case_Result
  else if find_op ClassId then Case_Stmt
  else if find_op VoidCase then Case_Stmt
  else if find_op EmptyCase then Case_Stmt
  else if find_label "Case-Join" then Case_Join
  else Normal

let label_blocks cfg_param =
  let label_block cfg_elem = { label = get_label cfg_elem; block = cfg_elem } in
  List.map label_block cfg_param

let print_labelled_block block =
  let get_label label =
    match label with
    | If_Cond -> "If-Cond"
    | If_Join -> "If_Join"
    | If_Then -> "If-Then"
    | If_Else -> "If-Else"
    | Case_Expr -> "Case_Expr"
    | Case_Stmt -> "Case_Stmt"
    | Case_Join -> "Case_Join"
    | Case_Result -> "Case_Result"
    | While_Cond -> "While_Cond"
    | While_Body -> "While_Body"
    | While_Join -> "While_Join"
    | Normal -> "Normal"
  in
  Printf.fprintf Print.out_file "-------------%s-----------\n"
    (get_label block.label);
  print_tac_elems block.block

let create_proper_cfg (labelled_blocks : labelled_basic_block list) =
  let global_blocks = ref labelled_blocks in
  let pop () =
    let ret = List.hd !global_blocks in
    global_blocks := List.filteri (fun i _ -> i > 0) !global_blocks;
    ret
  in
  let get_case_stmts () =
    let rec get_next acc =
      match (List.hd !global_blocks).label with
      | Case_Stmt ->
          let hd = pop () in
          get_next (Normal_Node hd.block :: acc)
      | _ -> acc
    in
    get_next []
  in
  let rec get_next () : cfg_elem =
    let hd = pop () in
    match hd.label with
    | If_Cond ->
        let cond = Normal_Node hd.block in
        let then_stmt = get_next () in
        let else_stmt = get_next () in
        let join_stmt = get_next () in
        If_Statement (cond, then_stmt, else_stmt, join_stmt, [])
    | Case_Expr ->
        let case_expr = Normal_Node hd.block in
        let case_stmts = get_case_stmts () in
        let case_join = get_next () in
        Cases (case_expr, case_stmts, case_join)
    | While_Cond ->
        let while_cond = Normal_Node hd.block in
        let while_body = get_next () in
        let while_join = get_next () in
        Loop (while_cond, while_body, while_join)
    | _ -> Normal_Node hd.block
  in
  let rec build_lst acc =
    match List.length !global_blocks > 0 with
    | true -> build_lst (acc @ [ get_next () ])
    | false -> acc
  in
  build_lst []

let tac_to_cfg (tacs, class_name, method_name, arguments, temp_count) =
  let create_cfg tac_list =
    let rec create_cfg_inner (tac_list : tac_elem list) acc cfg =
      match tac_list with
      | tac :: tail -> (
          match is_break_point tac with
          | true -> create_cfg_inner tail [] ([ List.rev (tac :: acc) ] @ cfg)
          | false -> create_cfg_inner tail (tac :: acc) cfg)
      | [] -> [ List.rev acc ] @ cfg
    in
    List.rev (create_cfg_inner tac_list [] [])
  in
  let cfg = tacs |> create_cfg |> label_blocks |> create_proper_cfg in
  { cfg; class_name; method_name; arguments; temp_count }

let rec block_tac (node : cfg_elem) =
  let rec parse acc (node : cfg_elem) =
    match node with
    | Normal_Node a -> a :: acc
    | If_Statement (c, t, e, j, _) ->
        parse acc c @ parse acc t @ parse acc e @ parse acc j
    | Loop (a, b, c) -> parse acc a @ parse acc b @ parse acc c
    | Cases (node1, node_list, node2) ->
        let start = parse acc node1 in
        let middle = List.map (parse acc) node_list |> List.flatten in
        let e = parse acc node2 in
        start @ middle @ e
  in
  parse [] node

and get_method_tac (nodes : cfg_elem list) =
  nodes |> List.map block_tac |> List.flatten |> List.flatten
(* 
  (* Basic Block *)
  | Normal_Node of basic_block
  (* Condition * Then Stmt * Else Stmt * Join  *)
  | If_Statement of cfg_elem * cfg_elem * cfg_elem * cfg_elem
  (* Condition * Body * Join *)
  | Loop of cfg_elem * cfg_elem * cfg_elem
  (* Case Expr * Cases * Join *)
  | Cases of cfg_elem * cfg_elem list * cfg_elem

       *)

(* Convert Tac to cfg *)
let cfg_list = ref (List.map tac_to_cfg Tac.tacs)
