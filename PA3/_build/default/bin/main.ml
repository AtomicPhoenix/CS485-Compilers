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
  let _ = Parser.parse_class_map () in
  let _ = Parser.parse_implementation_map () in
  let _ = Parser.parse_parent_map () in
  (* let class_map = Parser.parse_class_map () in
  let implementation_map = Parser.parse_implementation_map () in
  let parent_map = Parser.parse_parent_map () in  *)
  let annotated_ast = Parser.parse_annotated_ast () in
  List.iter Tac.add_class default_classes;
  List.iter Tac.add_class annotated_ast;
  let tacs = Tac.parse_tac_expressions annotated_ast in
  let cfg = List.map Cfg.tac_to_cfg tacs in
  List.iter Cfg.print_cfg cfg
(* List.iter Asm.tac_to_as (snd (List.hd tacs)) *)
