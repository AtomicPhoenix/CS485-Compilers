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
  - If statement stored as: Cond -> if       -> end
                                 -> elseifs  ->
                                 -> else     ->
  - Loops stored as: Cond -> LoopBody
                      |   <- 
                      v 
                     Exit
*)

type basic_block = tac_elem list
and cfg = basic_block list

(*
and control_flow_graph_elem =
  (* Basic Block followed by next CFG elem *)
  | Sequence of basic_block
  (* Basic Block (Condition) followed by Basic Block List (If bodies) followed by next CFG elem *)
  | If_Statement of basic_block * basic_block list
  (* Basic Block (Condition) followed by Basic Block(Loop Body) followed by next CFG elem *)
  | Loop of basic_block * basic_block
  | Function_Call of basic_block

and cfg = control_flow_graph_elem list *)

let is_break_point (tac : tac_elem) =
  match tac.operand with
  | Bt | Call | Jmp | Case _ | Default | Return -> true
  | _ -> false

let tac_to_cfg (tacs, class_name, method_name, temp_count) :
    cfg * string * string * int =
  let rec create_cfg (tac_list : tac_elem list) acc cfg =
    match tac_list with
    | tac :: tail -> (
        match is_break_point tac with
        | true -> create_cfg tail [] ([ List.rev (tac :: acc) ] @ cfg)
        | false -> create_cfg tail (tac :: acc) cfg)
    | [] -> [ List.rev acc ] @ cfg
  in
  (List.rev (create_cfg tacs [] []), class_name, method_name, temp_count)

and print_cfg bbl = List.iter (List.iter print_tac_elem) bbl

let cfg_list = List.map tac_to_cfg Tac.tacs
