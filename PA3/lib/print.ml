let file_name = Sys.argv.(1)
let file = open_in file_name
let base_file_name = String.sub file_name 0 (String.length file_name - 8)
let out_file = open_out (base_file_name ^ ".s")

(* let debug_file = open_out "/dev/null"*)
let debug = true
let debug_file = if debug then out_file else open_out "/dev/null"

(*let tac_file = open_out (base_file_name ^ ".cl-tac")*)
(*let out_file = stdout*)

(* let out_file = stdout*)
let read () = input_line file

let read_int () : int =
  let r = read () in
  try int_of_string r
  with c ->
    Printf.printf "%s\n" r;
    Printf.printf "---------------------ERRROR-------------------------\n";
    raise c
