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

let basic_block_to_asm (bb : Cfg.basic_block) = Asm.tac_list_to_asm bb
let cfg_to_asm (cfg : Cfg.basic_block list) = List.map basic_block_to_asm cfg

let () =
  (* let class_map = Parser.parse_class_map () in
  let implementation_map = Parser.parse_implementation_map () in
  let parent_map = Parser.parse_parent_map () in  *)
  let annotated_ast = Parser.parse_annotated_ast () in
  List.iter Tac.add_class default_classes;
  List.iter Tac.add_class annotated_ast;
  let tacs = Tac.parse_tac_expressions annotated_ast in
  let cfg_list = List.map Cfg.tac_to_cfg tacs in
  let asm_commands =
    List.map cfg_to_asm cfg_list |> List.flatten |> List.flatten |> List.flatten
  in
  List.iter Asm.print_asm asm_commands

(* basic_block_to_ast *)
