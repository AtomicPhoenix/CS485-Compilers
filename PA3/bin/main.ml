module Print = PA3.Print
module Parser = PA3.Parser
module Tac = PA3.Tac
module Asm = PA3.Asm
module Cfg = PA3.Cfg
module Intrinsics = PA3.Intrinsics

let () =
  (* Print all tacs *)
  (*
  List.iter (fun (f, _, _, _, _) -> Tac.print_tac_elems f) Tac.tacs;

  (* Default vtables *)
  List.iter Asm.print_vtable Asm.vtables;

  (* New functions (class initializers) *)
  Asm.print_new_funcs Asm.new_funcs;

  (* Important default functions *)
  let print_instrinsic_func ifunc =
    List.iter Asm.print_asm ifunc;
    Printf.fprintf Print.out_file
      "\t#;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;\n"
  in
  List.iter print_instrinsic_func Intrinsics.intrinsic_funcs;

  (* Assembly for methods *)
  List.iter (List.iter Asm.print_asm) Asm.method_asm;
  Printf.fprintf Print.out_file "\t.section\t.rodata\n";

  (* Assembly Strings *)
  Asm.print_string_map ();

  (* Value comparison handlers *)
  List.iter Asm.print_asm Intrinsics.handlers;

  (* Print start *)
  Asm.print_start ()
  *)
  (* List.iter (fun (f, _, _, _, _) -> Tac.print_tac_elems f) Tac.tacs; *)
  (* List.iter Cfg.print_graph Cfg.cfg_list *)
  (* Cfg.print_graph (List.hd Cfg.cfg_list)*)
  List.iter (List.iter Cfg.print_cfg) Cfg.real_cfg
