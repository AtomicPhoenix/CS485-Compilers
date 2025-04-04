open Print
open Parser
(* -------------------------------------------------------------------CODE GEN CODE----------------------------------------------------------------------- *)

(*NOTE: Write a program that generates assembly instructions for certain simple Cool programs.
    - Write a correct code generator for a subset of Cool. 
    - This involves implementing the operational semantics specification of Cool. 
    - You do not have to track information to generate run-time errors (e.g., division by zero, dispatch on void) for this checkpoint. 
    - You do not have to worry about “malformed input” because the semantic analyzer (from PA2) will rule out bad programs.
    - No Error reporting for this checkpoint BUT KEEP LINE INFO FOR LATER
    - The input program will introduce only one class, Main, with only one method, main. 
    - The following AST or language elements will NOT appear:
        - attribute_*
        - self_dispatch — except IO (e.g., in_out, out_int)
        - static_dispatch
        - dynamic_dispatch
        - internal functions
        - while
        - isvoid
        - case
        - new
        - let_binding_init
        - any novel class/method — except “Main.main”
        - run-time errors (e.g., division by zero)
        - strings — programs will largely involve integers and booleans
    - This results in single-class, single-method Cool programs focusing on integer and boolean manipulation.
    - Need to convert internal functions that are defined in the CRM. Look at the assembly code that the reference compiler emits for them.
    - Highly recommended to convert to control-flow graph where basic blocks are TAC, then convert TAC to assembly
    - "Whitespace and newlines do not matter in your file.s assembly code. However, whitespace and newlines do matter for the output of your generated assembly! This is because you are specifically being asked to implement IO (and, later, substring) functions." -- I don't know what this means 
    - Implment all relevant operational semantics rules in the Reference Manual including all of the relevant built-in functions on the IO class.*)

(* We store the memory addresses of each variable as we parse them *)

(*type asm_instruction = {*)
(*instruction: string;*)
(*arg1: string option;*)
(*arg2: string option;*)
(*arg3: string option;*)
(*}*)
type asm_instruction = string * string * string * string
(** instruction, arg1, arg2, arg3 *)

and asm_line = Instruction of asm_instruction | Line of string
and asm = asm_line list
and vtable = { name_id : string; methods : string list }

and attribute = {
  field_name : string;
  index : int;
  size : int;
  type_name : string;
}

and asm_class = {
  class_tag : int;
  object_size : int;
  vtable : vtable;
  attributes : attribute;
}

let class_map = Parser.parse_class_map ()
let implementation_map = Parser.parse_implementation_map ()
let parent_map = Parser.parse_parent_map ()

(*TODO: make this make and return an int object*)
let in_int =
  "\n\
   in_int:\n\
   \tpushq\t%rbx\n\
   \tmovl\t$4096, %esi\n\
   \tsubq\t$4112, %rsp\n\
   \tmovq\tstdin(%rip), %rdx\n\
   \tleaq\t16(%rsp), %rbx\n\
   \tmovq\t%rbx, %rdi\n\
   \tcall\tfgets\n\
   \tleaq\t8(%rsp), %rdx\n\
   \tmovq\t%rbx, %rdi\n\
   \txorl\t%eax, %eax\n\
   \tmovq\t$percent.ld, %rsi\n\
   \tcall\tsscanf\n\
   \tmovq\t8(%rsp), %rax\n\
   \tmovl\t$2147483648, %edx\n\
   \tmovl\t$4294967295, %ecx\n\
   \taddq\t%rax, %rdx\n\
   \tcmpq\t%rdx, %rcx\n\
   \tmovl\t$0, %edx\n\
   \tcmovb\t%rdx, %rax\n\
   \taddq\t$4112, %rsp\n\
   \tpopq\t%rbx\n\
   \tret\n"

let var_locations = Hashtbl.create 32
let string_map = Hashtbl.create 32

let add_var_addr (var_name : string) =
  let fp_offset = 8 * Hashtbl.length var_locations in
  Hashtbl.add var_locations var_name fp_offset

let get_var_addr (var_name : string) : string =
  match Hashtbl.find_opt var_locations var_name with
  | Some addr -> Printf.sprintf "%d(%%rbp)" addr
  | None ->
      Printf.fprintf out_file "; Is this supposed to happen?\n";
      var_name

let string_counter = ref 0

(** Method to convert a TAC element to assembly code *)
let tac_to_as (tac : tac_elem) elems =
  match tac.operand with
  | Assignment ->
      let result = get_var_addr tac.result in
      let arg1 = get_var_addr tac.arg1 in
      elems @ [
        Instruction ("movq", arg1, "%rax", "");
        Instruction ("movq", "%rax", result, "")
      ]
    
  | Bt ->
      let arg1 = get_var_addr tac.arg1 in
      elems
      @ [
          Instruction ("movq", arg1, "%rax", "");
          Instruction ("testq", "%rax", "%rax", "");
          Instruction ("jne", tac.arg2, "", "");
        ]
  | Call ->
      add_var_addr tac.result;
      let result = get_var_addr tac.result in
      if tac.arg2 = "" then
        elems
        @ [
            Instruction ("pusha", "", "", "");
            Instruction ("andq", "$0xFFFFFFFFFFFFFFF0", "%rsp", "");
            Instruction ("call", tac.arg1, "", "");
            Instruction ("movq", "%rax", result, "");
            Instruction ("popa", "", "", "");
          ]
      else
        let args = String.split_on_char ' ' tac.arg2 in
        let arglist =
          List.fold_left
            (fun acc itm -> acc @ [ Instruction ("pushq", itm, "", "") ])
            [] args
        in
        elems
        @ [ Instruction ("pusha", "", "", "") ]
        @ arglist
        @ [
            Instruction ("andq", "$0xFFFFFFFFFFFFFFF0", "%rsp", "");
            Instruction ("call", tac.arg1, "", "");
            Instruction ("movq", "%rax", result, "");
            Instruction ("popa", "", "", "");
          ]
      (* Push all variables onto stack *)
      (* Push all onto stack *)
      (*elems @ [Instruction{instruction = "callq"; arg1 = Some tac.arg1; arg2 = ""; arg3 = ""}]*)
      (*Printf.fprintf out_file "\tcallq %s\n" tac.arg1*)
  | Comment ->
      elems @ [ Line ("#" ^ tac.arg1) ]
      (*Printf.sprintf out_file "\t%s\n" ("#" ^ tac.arg1)*)
  | Label -> elems @ [ Line (Printf.sprintf "%s:" tac.arg1) ]
  | Jmp -> elems @ [ Instruction ("jmp", tac.arg1, "", "") ]
  (* | Case *)
  (* | Default *)
  | Return ->
      elems
      @ [
          Instruction ("movq", "%rbp", "%rsp", "");
          Instruction ("popq", "%rbp", "%rsp", "");
          Instruction ("ret", "", "", "");
        ]
  | LetNoInit ->
      add_var_addr tac.result;
      let result = get_var_addr tac.result in
      elems @ [
        Instruction ("movq", "$0", result, "");
      ]
    
  | Ident_Expr s ->
      add_var_addr tac.result;
      let result = get_var_addr tac.result in
      elems @ [
        Instruction ("movq", get_var_addr s, result, "");
      ]
  (*| New *)
  (* | Isvoid *)
  | Plus ->
      let arg1 = get_var_addr tac.arg1 in
      let arg2 = get_var_addr tac.arg2 in
      add_var_addr tac.result;
      let result = get_var_addr tac.result in
      elems
      @ [
          Instruction ("movq", arg1, "%rax", "");
          Instruction ("addl", arg2, "%eax", "");
          Instruction ("movq", "%rax", result, "");
        ]
  | Minus ->
      let arg1 = get_var_addr tac.arg1 in
      let arg2 = get_var_addr tac.arg2 in
      add_var_addr tac.result;
      let result = get_var_addr tac.result in
      elems
      @ [
          Instruction ("movq", arg1, "%rax", "");
          Instruction ("subl", arg2, "%eax", "");
          Instruction ("movq", "%rax", result, "");
        ]
  | Divide ->
      let arg1 = get_var_addr tac.arg1 in
      let arg2 = get_var_addr tac.arg2 in
      add_var_addr tac.result;
      let result = get_var_addr tac.result in
      elems
      @ [
          Instruction ("movq", arg1, "%rax", "");
          Instruction ("cltd", "", "", "");
          Instruction ("idivl", arg2, "", "");
          Instruction ("movq", "%rax", result, "");
        ]
  | Times ->
      let arg1 = get_var_addr tac.arg1 in
      let arg2 = get_var_addr tac.arg2 in
      add_var_addr tac.result;
      let result = get_var_addr tac.result in
      elems
      @ [
          Instruction ("movq", arg1, "%rax", "");
          Instruction ("imull", arg2, "%eax", "");
          Instruction ("movq", "%rax", result, "");
        ]
  | LessThan ->
      let arg1 = get_var_addr tac.arg1 in
      let arg2 = get_var_addr tac.arg2 in
      add_var_addr tac.result;
      let result = get_var_addr tac.result in
      elems
      @ [
          Instruction ("movq", arg1, "%rax", "");
          Instruction ("cmpq", "%rax", arg2, "");
          Instruction ("movq", "$0", "%rdx", "");
          Instruction ("cmovlq", "$1", "%rdx", "");
          Instruction ("movq", "%rdx", result, "");
        ]
  | LessEqual ->
      let arg1 = get_var_addr tac.arg1 in
      let arg2 = get_var_addr tac.arg2 in
      add_var_addr tac.result;
      let result = get_var_addr tac.result in
      elems
      @ [
          Instruction ("movq", arg1, "%rax", "");
          Instruction ("cmpq", "%rax", arg2, "");
          Instruction ("movq", "$0", "%rdx", "");
          Instruction ("cmovleq", "$1", "%rdx", "");
          Instruction ("movq", "%rdx", result, "");
        ]
  | Equal ->
      let arg1 = get_var_addr tac.arg1 in
      let arg2 = get_var_addr tac.arg2 in
      add_var_addr tac.result;
      let result = get_var_addr tac.result in
      elems
      @ [
          Instruction ("movq", arg1, "%rax", "");
          Instruction ("cmpq", "%rax", arg2, "");
          Instruction ("movq", "$0", "%rdx", "");
          Instruction ("cmoveq", "$1", "%rdx", "");
          Instruction ("movq", "%rdx", result, "");
        ]
  | Not ->
      let arg1 = get_var_addr tac.arg1 in
      add_var_addr tac.result;
      let result = get_var_addr tac.result in
      elems
      @ [
          Instruction ("movq", arg1, "%rax", "");
          Instruction ("testq", "%rax", "%rax", "");
          Instruction ("movq", "$0", "%rdx", "");
          Instruction ("cmoveq", "$1", "%rdx", "");
          Instruction ("movq", "%rdx", result, "");
        ]
  | Negate ->
      let arg1 = get_var_addr tac.arg1 in
      add_var_addr tac.result;
      let result = get_var_addr tac.result in
      elems
      @ [
          Instruction ("movq", arg1, "%rax", "");
          Instruction ("notq", "%rax", "", "");
          Instruction ("movq", "%rax", result, "");
        ]
  | Int_Constant ->
      add_var_addr tac.result;
      let result = get_var_addr tac.result in
      let arg1 = get_var_addr tac.arg1 in
      elems @ [
        Instruction ("movq", arg1, "%rax", "");
        Instruction ("movq", "%rax", result, "")
      ]
  | String_Constant -> (
      add_var_addr tac.result;
      let result = get_var_addr tac.result in
      match Hashtbl.find_opt string_map tac.arg1 with
      | Some str_id ->
          elems
          @ [
              Instruction ("movq", ".string" ^ string_of_int str_id, result, "");
            ]
      | None ->
          string_counter := !string_counter + 1;
          Hashtbl.add string_map tac.arg1 !string_counter;
          elems
          @ [
              Instruction
                ("movq", ".string" ^ string_of_int !string_counter, result, "");
            ]
      (* This is all we do because they're emitted later :) *))
  (* This is the later but that's another stage; needs to be NOT just in an expression lm ao*)
  (*Printf.fprintf out_file "\t%s\n" (".secton\t.rodata");*)
  (*Printf.fprintf out_file "%s\n" ("string" ^ string_of_int(!string_counter) ^ ":");*)
  (*Printf.fprintf out_file "\t%s\n" (".string \"" ^ string_of_int(!string_counter) ^ "\"");*)
  (*Printf.fprintf out_file "\t%s\n" (".string \"" ^ string_of_int(!string_counter) ^ "\"")*)

   | Boolean_Constant ->
      add_var_addr tac.result;
      let result = get_var_addr tac.result in
      if tac.arg1 = "true" then
      elems @ [
        Instruction ("movq", "$1", "%rax", "");
        Instruction ("movq", "%rax", result, "")
      ]
      else
      elems @ [
        Instruction ("movq", "$0", "%rax", "");
        Instruction ("movq", "%rax", result, "")
      ]
  | _ -> assert false
