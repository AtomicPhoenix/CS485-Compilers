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
  let vtables = Asm.create_default_vtables () @ !Asm.vtable_list in
  let print_vtable (table : Asm.vtable) =
    let name = table.name_id in
    let strid = table.name_string_id in
    Printf.fprintf Print.out_file ".globl %s..vtable\n" name;
    Printf.fprintf Print.out_file "%s..vtable:\n" name;
    Printf.fprintf Print.out_file "\t.quad string%d\n" strid;
    List.iter
      (fun (func : Asm.vtable_func) ->
        Printf.fprintf Print.out_file "\t.quad %s.%s\n" func.type_name
          func.method_name)
      table.methods;
    Printf.fprintf Print.out_file
      "\t#;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;\n"
  in
  List.iter print_vtable vtables;
  let print_new_funcs funcs =
    let print_new_func func =
      let name, lines = func in
      Printf.fprintf Print.out_file "\t.p2align 4\n";
      Printf.fprintf Print.out_file "\t.globl\t%s..new\n" name;
      Printf.fprintf Print.out_file "\t.type\t%s..new, @function\n" name;
      List.iter (fun ln -> Asm.print_asm ln) lines;
      Printf.fprintf Print.out_file "\t.size\t%s, .-%s\n" name name;
      Printf.fprintf Print.out_file
        "\t#;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;\n"
    in
    List.iter print_new_func funcs
  in
  print_new_funcs Asm.new_funcs;
  List.iter
    (fun func ->
      List.iter (fun f -> Asm.print_asm f) func;
      Printf.fprintf Print.out_file
        "\t#;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;\n")
    Asm.intrinsic_funcs;

  (* let class_map = Parser.parse_class_map () in
  let implementation_map = Parser.parse_implementation_map () in
  let parent_map = Parser.parse_parent_map () in  *)
  let annotated_ast = Parser.parse_annotated_ast () in
  List.iter Tac.add_class default_classes;
  List.iter Tac.add_class annotated_ast;

  (*let tacs = Tac.parse_tac_expressions annotated_ast in*)
  (*let cfg_list = List.map Cfg.tac_to_cfg tacs in*)
  (*let asm_commands =*)
  (*List.map cfg_to_asm cfg_list |> List.flatten |> List.flatten *)
  (*in*)

  (*List.iter Asm.print_asm asm_commands*)

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
