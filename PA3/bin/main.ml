module Print = PA3.Print
module Parser = PA3.Parser
module Tac = PA3.Tac
module Asm = PA3.Asm
module Cfg = PA3.Cfg
module Optimizer = PA3.Optimizer
module Intrinsics = PA3.Intrinsics

let do_ssa () =
  List.iter
    (fun (node : Cfg.cfg) -> Optimizer.singe_static_assignment node)
    !Cfg.cfg_list

let undo_ssa () =
  List.iter
    (fun (node : Cfg.cfg) -> Optimizer.undo_singe_static_assignment node)
    !Cfg.cfg_list

let clean () =
  List.iter
    (fun (node : Cfg.cfg) -> Optimizer.remove_self_assigns node)
    !Cfg.cfg_list

let do_dce () =
  clean ();
  List.iter
    (fun (node : Cfg.cfg) -> Optimizer.dead_code_elimination node)
    !Cfg.cfg_list;
  clean ()

let print_all_tac file_name =
  let out_file = open_out (Print.base_file_name ^ file_name ^ ".cl-tac") in
  List.iter
    (fun (f : Cfg.cfg) ->
      (Cfg.get_method_tac f.cfg |> Tac.print_tac_elems_file) out_file)
    !Cfg.cfg_list

let do_dce_2_eb () =
  clean ();
  do_ssa ();
  List.iter (fun (node : Cfg.cfg) -> Optimizer.simplify_cfg node) !Cfg.cfg_list;
  List.iter
    (fun (node : Cfg.cfg) ->
      Optimizer.dead_code_elimination_2_electic_boogaloo node)
    !Cfg.cfg_list;
  List.iter (fun (node : Cfg.cfg) -> Optimizer.simplify_cfg node) !Cfg.cfg_list;
  undo_ssa ();
  clean ()

let optimize () =
  do_dce ();
  do_dce_2_eb ();
  do_dce ();
  do_dce_2_eb ();
  do_dce ()

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
  print_all_tac "unoptimized";
  optimize ();
  print_all_tac "optimized";
  pa4c1 ();
  pa4full ()
