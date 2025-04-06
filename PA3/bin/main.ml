module Print = PA3.Print
module Parser = PA3.Parser
module Tac = PA3.Tac
module Asm = PA3.Asm
module Cfg = PA3.Cfg

let default_classes : Parser.annotated_ast_elem list =
  [
    {
      class_name = { line_num = 0; name = "Object" };
      inherits = None;
      features = [];
    };
    {
      class_name = { line_num = 0; name = "Bool" };
      inherits = Some { line_num = 0; name = "Object" };
      features = [];
    };
    {
      class_name = { line_num = 0; name = "String" };
      inherits = Some { line_num = 0; name = "Object" };
      features = [];
    };
    {
      class_name = { line_num = 0; name = "Int" };
      inherits = Some { line_num = 0; name = "Object" };
      features = [];
    };
    {
      class_name = { line_num = 0; name = "IO" };
      inherits = Some { line_num = 0; name = "Object" };
      features = [];
    };
  ]

let () =
  (* let class_map = Parser.parse_class_map () in
  let implementation_map = Parser.parse_implementation_map () in
  let parent_map = Parser.parse_parent_map () in  *)
  let annotated_ast = Parser.parse_annotated_ast () in
  List.iter Tac.add_class default_classes;
  List.iter Tac.add_class annotated_ast;

  (* A List of basic blocks *)
  (* A list of list of tac elems *)
  let tacs = Tac.parse_tac_expressions annotated_ast in

  (* A cfg list *)
  (* A list of list of basic blocks *)
  (* A tac_elem list list list *)
  let cfg_list = List.map Cfg.tac_to_cfg tacs in

  (* A list of (the assembly code for) methods *)
  let method_asm =
    List.map
      (fun (cfg, _, method_name) ->
        let method_tac = cfg |> List.flatten in
        List.map (fun tac -> Asm.tac_to_as tac method_name) method_tac
        |> List.flatten)
      cfg_list
  in
  List.iter (List.iter Asm.print_asm) method_asm

(* basic_block_to_ast *)

(* 
  
let cfg_to_asm = ()
  let class_name = "TODO" in
  let method_name = "TODO" in


*)
