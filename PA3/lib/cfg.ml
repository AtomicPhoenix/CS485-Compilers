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

type basic_block = tac_elem list
and cfg = basic_block list

and cfg_elem =
  (* Basic Block *)
  | Normal_Node of basic_block
  (* Condition * Then Stmt * Else Stmt * Join  *)
  | If_Statement of cfg_elem * cfg_elem * cfg_elem * cfg_elem
  (* Condition * Body * Join *)
  | Loop of cfg_elem * cfg_elem * cfg_elem
  (* Case Expr * Cases * Join *)
  | Cases of cfg_elem * cfg_elem list * cfg_elem

and control_flow_graph = cfg_elem list

and graph_elem = {
  cfg : cfg;
  class_name : string;
  method_name : string;
  arguments : Parser.ast_formal list;
  temp_count : int;
}

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

and labelled_basic_block = { label : basic_block_label; block : basic_block }

(* Checks if a given tac element is a break point in the CFG *)
let is_break_point (tac : tac_elem) =
  match tac.operand with
  | Bt | Jmp | Case _ | EmptyCase | VoidCase -> true
  | _ -> false

let tac_to_cfg (tacs, class_name, method_name, arguments, temp_count) :
    graph_elem =
  let rec create_cfg (tac_list : tac_elem list) acc cfg =
    match tac_list with
    | tac :: tail -> (
        match is_break_point tac with
        | true -> create_cfg tail [] ([ List.rev (tac :: acc) ] @ cfg)
        | false -> create_cfg tail (tac :: acc) cfg)
    | [] -> [ List.rev acc ] @ cfg
  in
  {
    cfg = List.rev (create_cfg tacs [] []);
    class_name;
    method_name;
    arguments;
    temp_count;
  }

let get_next_cfg_elem (cfg_base : cfg) =
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

(* let generate_graph cfg_list : cfg_elem list =
  let rec get_cfg_elem (cfg_list : cfg) =
    match get_next_cfg_elem cfg_list with
    | 1 ->
        let cond, cfg_list = get_cfg_elem cfg_list in
        let then_stmt, cfg_list = get_cfg_elem cfg_list in
        let else_stmt, cfg_list = get_cfg_elem cfg_list in
        let if_join, cfg_list = get_cfg_elem cfg_list in
        (If_Statement (cond, then_stmt, else_stmt, if_join), cfg_list)
    | 2 ->
        let while_pred, cfg_list = get_cfg_elem cfg_list in
        let while_body, cfg_list = get_cfg_elem cfg_list in
        let while_join, cfg_list = get_cfg_elem cfg_list in
        (Loop (while_pred, while_body, while_join), cfg_list)
    | 3 ->
        let case_expr, cfg_list = get_cfg_elem cfg_list in
        let cases, cfg_list = get_cfg_elem cfg_list in
        let case_join, cfg_list = get_cfg_elem cfg_list in
        (Cases (case_expr, [ cases ], case_join), cfg_list)
    | _ -> (Basic_Node (cfg_list |> List.flatten), [])
  in
  let rec build_lst lst acc =
    match lst with
    | [] -> assert false
    | _ ->
        let elem, remaining = get_cfg_elem cfg_list in
        build_lst remaining (elem :: acc)
  in
  build_lst cfg_list []
*)
(* let create_case_elem (cfg_base : cfg) =
  let first_elem = List.hd cfg_base |> List.rev |> List.hd in
  if first_elem.operand = Bt then (* Its either a while loop of a for loop *)
    let second_elem = List.nth cfg_base 1 |> List.hd in
    if second_elem.arg1 = "while-body" then
      ( Loop (List.nth cfg_base 0, List.nth cfg_base 1, List.nth cfg_base 2),
        List.filteri (fun i _ -> i > 2) cfg_base )
    else
      ( If_Statement
          ( List.nth cfg_base 0,
            List.nth cfg_base 1,
            List.nth cfg_base 2,
            List.nth cfg_base 3 ),
        List.filteri (fun i _ -> i > 3) cfg_base )
  else assert false *)
(*
    if (if_stmt) {

    }
    elif (case) {

    }
    elif (loop) {

    }
    else {

    }
  *)

(*
let create_case_cfg_elem (cfg_list : cfg list) =
  let rec build_case_cfg_elem cfg_list case_acc remaining =
    match cfg_list with hd :: tail -> true | _ -> false
  in
  assert false
*)
and print_graph (graph : graph_elem) =
  let print_cfg cfg_param =
    List.iter
      (fun elem ->
        let tac = get_tac_elem elem in
        if elem.operand = Label && elem.arg2 = "join" then
          Printf.fprintf Print.out_file "JOINING\n";
        Printf.fprintf Print.out_file "%s\n" tac)
      cfg_param;
    Printf.fprintf Print.out_file "-------------------\n"
    (* Printf.fprintf Print.out_file "-------------------\n" *)
  in
  (*Printf.fprintf Print.out_file "CFG for %s.%s:\n" graph.class_name
    graph.method_name;
  Printf.fprintf Print.out_file "Arguments: ";
  List.iter
    (fun (arg : Parser.ast_formal) ->
      Printf.fprintf Print.out_file "(%s : %s) " arg.name.name
        arg.formal_type.name)
    graph.arguments;
  Printf.fprintf Print.out_file "\n"; *)
  List.iter print_cfg graph.cfg

let rec print_cfg elem =
  match elem with
  | Loop (cond, while_body, join_body) ->
      Printf.fprintf Print.out_file
        "-----------While Stmt Condition:-----------\n";
      print_cfg cond;
      Printf.fprintf Print.out_file "-----------While Stmt Body:-----------\n";
      print_cfg while_body;
      Printf.fprintf Print.out_file "-----------While Stmt Join:-----------\n";
      print_cfg join_body
  | If_Statement (cond, then_body, else_body, join_body) ->
      Printf.fprintf Print.out_file
        "---------------If Stmt Condition:-----------------------\n";
      print_cfg cond;
      Printf.fprintf Print.out_file
        "---------------If Stmt Then:-----------------------\n";
      print_cfg then_body;
      Printf.fprintf Print.out_file
        "---------------If Stmt Else:-----------------------\n";
      print_cfg else_body;
      Printf.fprintf Print.out_file
        "---------------If Stmt Join:-----------------------\n";
      print_cfg join_body
  | Cases (exp, cases, join) ->
      Printf.fprintf Print.out_file "-----------Cases Start:-----------\n";
      print_cfg exp;
      List.iteri
        (fun i case ->
          Printf.fprintf Print.out_file
            "--------------Case %d:---------------\n" i;
          print_cfg case)
        cases;
      Printf.fprintf Print.out_file "-----------Cases Join:-----------\n";
      print_cfg join
  | Normal_Node elems -> print_tac_elems elems

let get_label (tacs : basic_block) =
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

let label_blocks (cfg_param : cfg) =
  let label_block (cfg_elem : basic_block) =
    { label = get_label cfg_elem; block = cfg_elem }
  in
  List.map label_block cfg_param

let cfg_list : graph_elem list = List.map tac_to_cfg Tac.tacs

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
        If_Statement (cond, then_stmt, else_stmt, join_stmt)
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

let real_cfg =
  let cfgs = List.map (fun cfg -> cfg.cfg) cfg_list in
  let labelled_blocks = List.map label_blocks cfgs in
  List.map create_proper_cfg labelled_blocks
