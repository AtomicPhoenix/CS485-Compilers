module Print = PA3.Print
module Parser = PA3.Parser
module Tac = PA3.Tac
module Asm = PA3.Asm
module Cfg = PA3.Cfg
module Optimizer = PA3.Optimizer
module Intrinsics = PA3.Intrinsics

let () =
  List.iter
    (fun (node : Cfg.cfg) ->
      node.cfg <- Optimizer.dead_code_elimination node.cfg)
    !Cfg.cfg_list;
  let method_asm = Asm.get_method_asm !Cfg.cfg_list in

  (* Print Tac for First Method *)
  if Print.debug then
    ((!Cfg.cfg_list |> List.hd).cfg |> Cfg.get_method_tac
   |> Tac.print_tac_elems_file)
      (open_out "./outputs/our-tac.cl-tac");

  Optimizer.print_optimization_comparison ();
  List.iter Asm.print_vtable Asm.vtables;

  Asm.print_new_funcs Asm.new_funcs;

  (* Important default functions *)
  let print_instrinsic_func ifunc = List.iter Asm.print_asm ifunc in
  List.iter print_instrinsic_func Intrinsics.intrinsic_funcs;

  (* Assembly for methods *)
  List.iter (List.iter Asm.print_asm) method_asm;
  Printf.fprintf Print.out_file "\t.section\t.rodata\n";

  (* Assembly Strings *)
  Asm.print_string_map ();

  (* Value comparison handlers *)
  List.iter Asm.print_asm Intrinsics.handlers;

  (* Print start *)
  Asm.print_start ()
