module Print = PA3.Print
module Parser = PA3.Parser
module Tac = PA3.Tac
module Asm = PA3.Asm
module Cfg = PA3.Cfg
module Optimizer = PA3.Optimizer
module Intrinsics = PA3.Intrinsics

let optimize () =
  let out_file = open_out (Print.base_file_name ^ ".pre-cl-tac") in
  ((!Cfg.cfg_list |> List.hd).cfg |> Cfg.get_method_tac
 |> Tac.print_tac_elems_file)
    out_file;

  List.iter
    (fun (node : Cfg.cfg) -> Optimizer.dead_code_elimination node)
    !Cfg.cfg_list
(* Optimizer.print_optimization_comparison () *)

let pa4c1 () =
  (* Print Tac for First Method *)
  let out_file = open_out (Print.base_file_name ^ ".cl-tac") in
  (* List.iter
    (fun (node : Cfg.cfg) -> Optimizer.dead_code_elimination node)
    !Cfg.cfg_list; *)
  ((!Cfg.cfg_list |> List.hd).cfg |> Cfg.get_method_tac
 |> Tac.print_tac_elems_file)
    out_file

let pa4full () =
  (* Print Tac for First Method *)
  (* if Print.debug then (
    let out_file = open_out (Print.base_file_name ^ ".cl-tac") in
    ((!Cfg.cfg_list |> List.hd).cfg |> Cfg.get_method_tac
   |> Tac.print_tac_elems_file)
      out_file;

    Optimizer.print_optimization_comparison ());*)
  List.iter Asm.print_vtable Asm.vtables;

  Asm.print_new_funcs Asm.new_funcs;

  (* Important default functions *)
  let print_instrinsic_func ifunc = List.iter Asm.print_asm ifunc in
  List.iter print_instrinsic_func Intrinsics.intrinsic_funcs;

  (* Assembly for methods *)
  List.iter (List.iter Asm.print_asm) (Asm.get_method_asm !Cfg.cfg_list);
  Printf.fprintf Print.out_file "\t.section\t.rodata\n";

  (* Assembly Strings *)
  Asm.print_string_map ();

  (* Value comparison handlers *)
  List.iter Asm.print_asm Intrinsics.handlers;

  (* Print start *)
  Asm.print_start ()

let () =
  optimize ();
  pa4c1 ();
  pa4full ()
