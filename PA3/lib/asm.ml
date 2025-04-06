open Print
open Parser
(* ------------------------------------------------------------------CODE GEN CODE----------------------------------------------------------------------- *)

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
and vtable_func = { type_name : string; method_name : string }
and vtable = { name_id : string; name_string_id : int; methods : vtable_func list }

and attribute = {
  field_name : string;
  index : int;
  type_name : string;
  expression : expr option;
}

and asm_class = {
  class_tag : int;
  object_size : int;
  vtable : vtable;
  attributes : attribute list;
}
and new_func = string * asm

let print_asm (asm : asm_line) =
  match asm with
  | Instruction (s1, s2, s3, s4) ->
      if s4 != "" then Printf.fprintf out_file "\t%s %s, %s %s\n" s1 s2 s3 s4
      else if s3 != "" then Printf.fprintf out_file "\t%s %s, %s\n" s1 s2 s3
      else if s2 != "" then Printf.fprintf out_file "\t%s %s\n" s1 s2
      else if s1 != "" then Printf.fprintf out_file "\t%s\n" s1
  | Line s1 -> Printf.fprintf out_file "%s\n" s1

let var_locations = Hashtbl.create 32

(* let string_map = Hashtbl.create 32*)
let string_map = Hashtbl.create 32
let class_id_map = Hashtbl.create 32
let class_vtable_map = Hashtbl.create 32
let string_counter = ref 5
let class_tag_ctr = ref 9
let class_map = Parser.parse_class_map ()
let implementation_map = Parser.parse_implementation_map ()
let parent_map = Parser.parse_parent_map ()
let vtable_list : vtable list ref = ref []

let create_vtable (itm : implementation_map_elem) : vtable =
  let name = itm.name in
  let funcs =
    List.map
      (fun (meth : imp_method) ->
        { type_name = meth.type_name; method_name = meth.name })
      itm.methods
  in
  string_counter := !string_counter + 1;
  { name_id = name; name_string_id = !string_counter; methods = funcs }

let create_vtables () =
  let tables = List.map create_vtable implementation_map in
  List.iter (fun i -> Hashtbl.add class_vtable_map i.name_id i; vtable_list := !vtable_list @ [i]) tables

let create_default_vtables () =
  [
    { name_id = "Bool"; name_string_id = 0; methods = [{ type_name = "Bool"; method_name = ".new"}; {type_name = "Object"; method_name = "abort"}; {type_name = "Object"; method_name = "copy"}; {type_name = "Object"; method_name = "type_name"}]};
    { name_id = "IO"; name_string_id = 1; methods = [{ type_name = "IO"; method_name = ".new"}; {type_name = "Object"; method_name = "abort"}; {type_name = "Object"; method_name = "copy"}; {type_name = "Object"; method_name = "type_name"}; {type_name = "IO"; method_name = "in_int"}; {type_name = "IO"; method_name = "in_string"}; {type_name = "IO"; method_name = "out_int"}; {type_name = "IO"; method_name = "out_string"}]};
    { name_id = "Int"; name_string_id = 2; methods = [{ type_name = "Int"; method_name = ".new"}; {type_name = "Object"; method_name = "abort"}; {type_name = "Object"; method_name = "copy"}; {type_name = "Object"; method_name = "type_name"}]};
    { name_id = "Object"; name_string_id = 3; methods = [{ type_name = "Object"; method_name = ".new"}; {type_name = "Object"; method_name = "abort"}; {type_name = "Object"; method_name = "copy"}; {type_name = "Object"; method_name = "type_name"}]};
    { name_id = "String"; name_string_id = 4; methods = [{ type_name = "String"; method_name = ".new"}; {type_name = "Object"; method_name = "abort"}; {type_name = "Object"; method_name = "copy"}; {type_name = "Object"; method_name = "type_name"}; {type_name = "String"; method_name = "concat"}; {type_name = "String"; method_name = "length"}; {type_name = "String"; method_name = "substr"}]};
  ]

let get_class_attributes attrs =
  let get_attribute i (attr : ast_attribute) =
    let name = attr.name in
    let index = i in
    let typename = attr.type_name in
    let expr = attr.attr_expr in
    { field_name = name; index; type_name = typename; expression = expr }
  in
  List.mapi get_attribute attrs

let make_asm_class (c : class_map_elem) =
  let tag = !class_tag_ctr in
  class_tag_ctr := !class_tag_ctr + 1;
  Hashtbl.add class_id_map c.name tag;
  (* 8 bytes/64 bits since every attribute is a pointer *)
  let siz = List.length c.attrs in
  let class_vtable = Hashtbl.find class_vtable_map c.name in
  let attrs = get_class_attributes c.attrs in
  {
    class_tag = tag;
    object_size = siz;
    vtable = class_vtable;
    attributes = attrs;
  }
  


let in_int =
  [
    Line "\t.p2align 4";
    Line "\t.globl\tIO.in_int";
    Line "IO.in_int:";
    Instruction ("pushq", "%rbp", "", "");
    Instruction ("pushq", "%rbx", "", "");
    Instruction ("subq", "$4120", "%rsp", "");
    Instruction ("call", "Int..new", "", "");
    Instruction ("leaq", "16(%rsp)", "%rbp", "");
    Instruction ("movl", "$4096", "%esi", "");
    Instruction ("movq", "stdin(%rip)", "%rdx", "");
    Instruction ("movq", "%rbp", "%rdi", "");
    Instruction ("movq", "%rax", "%rbx", "");
    Instruction ("call", "fgets", "", "");
    Instruction ("leaq", "8(%rsp)", "%rdx", "");
    Instruction ("movq", "%rbp", "%rdi", "");
    Instruction ("xorl", "%eax", "%eax", "");
    Instruction ("movq", "$percent.ld", "%rsi", "");
    Instruction ("call", "sscanf", "", "");
    Instruction ("movq", "8(%rsp)", "%rax", "");
    Instruction ("movl", "$2147483648", "%edx", "");
    Instruction ("movl", "$4294967295", "%ecx", "");
    Instruction ("addq", "%rax", "%rdx", "");
    Instruction ("cmpq", "%rdx", "%rcx", "");
    Instruction ("movl", "$0", "%edx", "");
    Instruction ("cmovb", "%rdx", "%rax", "");
    Instruction ("movq", "%rax", "24(%rbx)", "");
    Instruction ("addq", "$4120", "%rsp", "");
    Instruction ("movq", "%rbx", "%rax", "");
    Instruction ("popq", "%rbx", "", "");
    Instruction ("popq", "%rbp", "", "");
    Instruction ("ret", "", "", "");
    (*Instruction (".size", "IO.in_int", ".-IO.in_int", "");*)
  ]

let out_int =
  [
    Line "\t.p2align 4";
    Line "\t.globl\tIO.out_int";
    Line "IO.out_int:";
    Instruction ("pushq", "%rbx", "", "");
    Instruction ("movq", "24(%rsi)", "%rsi", "");
    Instruction ("movq", "24(%rdi)", "%rbx", "");
    Instruction ("xorl", "%eax", "%eax", "");
    Instruction ("movq", "$percent.d", "%rdi", "");
    Instruction ("call", "printf", "", "");
    Instruction ("movq", "rbx", "%rax", "");
    Instruction ("popq", "rbx", "", "");
    (*Instruction (".size", "IO.out_int", ".-IO.out_int", "");*)
  ]
let intrinsic_funcs =
  [
    in_int;
    out_int;
  ]

(* Bool is class tag 0 *)
let () = Hashtbl.add class_id_map "Bool" 0

let bool_new =
  [
    Line "Bool..new:";
    Instruction ("subq", "$8", "%rsp", "");
    Instruction ("movl", "$4", "%esi", "");
    Instruction ("movl", "$8", "%edi", "");
    Instruction ("call", "calloc", "", "");
    Line "\t#Set class tag, object size, vtable pointer";
    Instruction ("movq", "$0", "(%rax)", "");
    Instruction ("movq", "$4", "8(%rax)", "");
    Instruction ("movq", "$Bool..vtable", "%r10", "");
    Instruction ("movq", "%r10", "16(%rax)", "");
    Instruction ("movq", "$0", "24(%rax)", "");
    Instruction ("addq", "$8", "%rsp", "");
    Instruction ("ret", "", "", "");
    Instruction (".size", "Bool..new", ".-Bool..new", "");
  ]

let () = Hashtbl.add class_id_map "IO" 1

let io_new =
  [
    Line "IO..new:";
    Instruction ("subq", "$8", "%rsp", "");
    Instruction ("movl", "$3", "%esi", "");
    Instruction ("movl", "$8", "%edi", "");
    Instruction ("call", "calloc", "", "");
    Line "\t#Set class tag, object size, vtable pointer";
    Instruction ("movq", "$8", "(%rax)", "");
    Instruction ("movq", "$3", "8(%rax)", "");
    Instruction ("movq", "$IO..vtable", "%r10", "");
    Instruction ("movq", "%r10", "16(%rax)", "");
    Instruction ("addq", "$8", "%rsp", "");
    Instruction ("ret", "", "", "");
    Instruction (".size", "IO..new", ".-IO..new", "");
  ]

let () = Hashtbl.add class_id_map "Int" 2

let int_new =
  [
    Line "Int..new:";
    Instruction ("subq", "$8", "%rsp", "");
    Instruction ("movl", "$4", "%esi", "");
    Instruction ("movl", "$8", "%edi", "");
    Instruction ("call", "calloc", "", "");
    Line "\t#Set class tag, object size, vtable pointer";
    Instruction ("movq", "$1", "(%rax)", "");
    Instruction ("movq", "$4", "8(%rax)", "");
    Instruction ("movq", "$Int..vtable", "%r10", "");
    Instruction ("movq", "%r10", "16(%rax)", "");
    Instruction ("movq", "$0", "24(%rax)", "");
    Instruction ("addq", "$8", "%rsp", "");
    Instruction ("ret", "", "", "");
    Instruction (".size", "IO..new", ".-IO..new", "");
  ]

let () = Hashtbl.add class_id_map "Object" 3

let object_new =
  [
    Line "Object..new:";
    Instruction ("subq", "$8", "%rsp", "");
    Instruction ("movl", "$3", "%esi", "");
    Instruction ("movl", "$8", "%edi", "");
    Instruction ("call", "calloc", "", "");
    Line "\t#Set class tag, object size, vtable pointer";
    Instruction ("movq", "$7", "(%rax)", "");
    Instruction ("movq", "$3", "8(%rax)", "");
    Instruction ("movq", "$Object..vtable", "%r10", "");
    Instruction ("movq", "%r10", "16(%rax)", "");
    Instruction ("addq", "$8", "%rsp", "");
    Instruction ("ret", "", "", "");
    (*Instruction (".size", "Object..new", ".-Object..new", "");*)
  ]

let () = Hashtbl.add class_id_map "String" 4

let string_new =
  [
    Line "String..new:";
    Instruction ("subq", "$8", "%rsp", "");
    Instruction ("movl", "$4", "%esi", "");
    Instruction ("movl", "$8", "%edi", "");
    Instruction ("call", "calloc", "", "");
    Line "\t#Set class tag, object size, vtable pointer";
    Instruction ("movq", "$3", "(%rax)", "");
    Instruction ("movq", "$4", "8(%rax)", "");
    Instruction ("movq", "$String..vtable", "%r10", "");
    Instruction ("movq", "%r10", "16(%rax)", "");
    Instruction ("movq", "$empty.string", "%r10", "");
    Instruction ("movq", "%r10", "24(%rax)", "");
    Instruction ("addq", "$8", "%rsp", "");
    Instruction ("ret", "", "", "");
    (*Instruction (".size", "String..new", ".-String..new", "");*)
  ]

let new_funcs =
  [
    ("Bool", bool_new);
    ("IO", io_new);
    ("Int", int_new);
    ("Object", object_new);
    ("String", string_new);
  ]
let handlers =
  [
    Line "lt_handler:";
    Instruction ("pushq", "%r12", "", "");
    Instruction ("movq", "%rsi", "%r12", "");
    Instruction ("pushq", "%rbp", "", "");
    Instruction ("pushq", "%rbx", "", "");
    Instruction ("movq", "%rdi", "%rbx", "");
    Instruction ("call", "Bool..new", "", "");
    Instruction ("movq", "%rax", "%rbp", "");
    Instruction ("testq", "%rbx", "%rbx", "");
    Instruction ("je", ".lt_false", "", "");
    Instruction ("testq", "%r12", "%r12", "");
    Instruction ("je", ".lt_false", "", "");
    Instruction ("movq", "(%r12)", "%rdx", "");
    Instruction ("addq", "(%rbx)", "%rdx", "");
    Instruction ("testq", "$-3", "%rdx", "");
    Instruction ("je", ".lt_num", "", "");
    Instruction ("xorl", "%eax", "%eax", "");
    Instruction ("cmpq", "$6", "%rdx", "");
    Instruction ("je", ".lt_string", "", "");
    Line ".lt_cleanup:";
    Instruction ("movq", "%rax", "24(%rbp)", "");
    Instruction ("movq", "%rbp", "%rax", "");
    Instruction ("popq", "%rbx", "", "");
    Instruction ("popq", "%rbp", "", "");
    Instruction ("popq", "%r12", "", "");
    Instruction ("ret", "", "", "");
    Instruction ("\t.p2align 4,,10", "", "", "");
    Instruction ("\t.p2align 3", "", "", "");
    Line ".lt_num:";
    Instruction ("movq", "24(%r12)", "%rax", "");
    Instruction ("cmpq", "%rax", "24(%rbx)", "");
    Instruction ("setge", "%al", "", "");
    Instruction ("movzbl", "%al", "%eax", "");
    Instruction ("movq", "%rax", "24(%rbp)", "");
    Instruction ("movq", "%rbp", "%rax", "");
    Instruction ("popq", "%rbx", "", "");
    Instruction ("popq", "%rbp", "", "");
    Instruction ("popq", "%r12", "", "");
    Instruction ("ret", "", "", "");
    Instruction ("\t.p2align 4,,10", "", "", "");
    Instruction ("\t.p2align 3", "", "", "");
    Line ".lt_false:";
    Instruction ("xorl", "%eax", "%eax", "");
    Instruction ("movq", "%rax", "24(%rbp)", "");
    Instruction ("movq", "%rbp", "%rax", "");
    Instruction ("popq", "%rbx", "", "");
    Instruction ("popq", "%rbp", "", "");
    Instruction ("popq", "%r12", "", "");
    Instruction ("ret", "", "", "");
    Instruction ("\t.p2align 4,,10", "", "", "");
    Instruction ("\t.p2align 3", "", "", "");
    Line ".lt_string:";
    Instruction ("movq", "24(%r12)", "%rsi", "");
    Instruction ("movq", "24(%rbx)", "%rdi", "");
    Instruction ("call", "strcmp", "", "");
    Instruction ("shrl", "$31", "%eax", "");
    Instruction ("jmp", ".lt_cleanup", "", "");
    Instruction ("\t.size", "lt_handler", ".-lt_handler", "");
    Instruction ("\t.p2align 4", "", "", "");
    Instruction ("\t.globl", "le_handler", "", "");
    Instruction ("\t.type", "le_handler", "@function", "");
    Line "le_handler:";
    Instruction ("pushq", "%r12", "", "");
    Instruction ("pushq", "%rbp", "", "");
    Instruction ("movq", "%rsi", "%rbp", "");
    Instruction ("pushq", "%rbx", "", "");
    Instruction ("movq", "%rdi", "%rbx", "");
    Instruction ("call", "Bool..new", "", "");
    Instruction ("movq", "%rax", "%r12", "");
    Instruction ("testq", "%rbx", "%rbx", "");
    Instruction ("je", ".le_false", "", "");
    Instruction ("testq", "%rbp", "%rbp", "");
    Instruction ("je", ".le_false", "", "");
    Instruction ("movq", "0(%rbp)", "%rax", "");
    Instruction ("addq", "(%rbx)", "%rax", "");
    Instruction ("testq", "$-3", "%rax", "");
    Instruction ("je", ".le_num", "", "");
    Instruction ("xorl", "%edx", "%edx", "");
    Instruction ("cmpq", "%rbp", "%rbx", "");
    Instruction ("sete", "%dl", "", "");
    Instruction ("cmpq", "$6", "%rax", "");
    Instruction ("je", ".le_string", "", "");
    Line ".le_cleanup:";
    Instruction ("movq", "%rdx", "24(%r12)", "");
    Instruction ("movq", "%r12", "%rax", "");
    Instruction ("popq", "%rbx", "", "");
    Instruction ("popq", "%rbp", "", "");
    Instruction ("popq", "%r12", "", "");
    Instruction ("ret", "", "", "");
    Instruction ("\t.p2align 4,,10", "", "", "");
    Instruction ("\t.p2align 3", "", "", "");
    Line ".le_num:";
    Instruction ("movq", "24(%rbp)", "%rax", "");
    Instruction ("xorl", "%edx", "%edx", "");
    Instruction ("cmpq", "%rax", "24(%rbx)", "");
    Instruction ("movq", "%r12", "%rax", "");
    Instruction ("setg", "%dl", "", "");
    Instruction ("movq", "%rdx", "24(%r12)", "");
    Instruction ("popq", "%rbx", "", "");
    Instruction ("popq", "%rbp", "", "");
    Instruction ("popq", "%r12", "", "");
    Instruction ("ret", "", "", "");
    Instruction ("\t.p2align 4,,10", "", "", "");
    Instruction ("\t.p2align 3", "", "", "");
    Line ".le_false:";
    Instruction ("xorl", "%edx", "%edx", "");
    Instruction ("movq", "%r12", "%rax", "");
    Instruction ("movq", "%rdx", "24(%r12)", "");
    Instruction ("popq", "%rbx", "", "");
    Instruction ("popq", "%rbp", "", "");
    Instruction ("popq", "%r12", "", "");
    Instruction ("ret", "", "", "");
    Instruction ("\t.p2align 4,,10", "", "", "");
    Instruction ("\t.p2align 3", "", "", "");
    Line ".le_string:";
    Instruction ("movq", "24(%rbp)", "%rsi", "");
    Instruction ("movq", "24(%rbx)", "%rdi", "");
    Instruction ("call", "strcmp", "", "");
    Instruction ("xorl", "%edx", "%edx", "");
    Instruction ("testl", "%eax", "%eax", "");
    Instruction ("setle", "%dl", "", "");
    Instruction ("jmp", ".le_cleanup", "", "");
    Instruction ("\t.size", "le_handler", ".-le_handler", "");
    Instruction ("\t.p2align 4", "", "", "");
    Instruction ("\t.globl", "eq_handler", "", "");
    Instruction ("\t.type", "eq_handler", "@function", "");
    Line "eq_handler:";
    Instruction ("pushq", "%r12", "", "");
    Instruction ("pushq", "%rbp", "", "");
    Instruction ("movq", "%rsi", "%rbp", "");
    Instruction ("pushq", "%rbx", "", "");
    Instruction ("movq", "%rdi", "%rbx", "");
    Instruction ("call", "Bool..new", "", "");
    Instruction ("movq", "%rax", "%r12", "");
    Instruction ("testq", "%rbx", "%rbx", "");
    Instruction ("je", ".eq_false", "", "");
    Instruction ("testq", "%rbp", "%rbp", "");
    Instruction ("je", ".eq_false", "", "");
    Instruction ("movq", "0(%rbp)", "%rax", "");
    Instruction ("addq", "(%rbx)", "%rax", "");
    Instruction ("testq", "$-3", "%rax", "");
    Instruction ("je", ".eq_num", "", "");
    Instruction ("xorl", "%edx", "%edx", "");
    Instruction ("cmpq", "%rbp", "%rbx", "");
    Instruction ("sete", "%dl", "", "");
    Instruction ("cmpq", "$6", "%rax", "");
    Instruction ("je", ".eq_string", "", "");
    Line ".eq_cleanup:";
    Instruction ("movq", "%rdx", "24(%r12)", "");
    Instruction ("movq", "%r12", "%rax", "");
    Instruction ("popq", "%rbx", "", "");
    Instruction ("popq", "%rbp", "", "");
    Instruction ("popq", "%r12", "", "");
    Instruction ("ret", "", "", "");
    Instruction ("\t.p2align 4,,10", "", "", "");
    Instruction ("\t.p2align 3", "", "", "");
    Line ".eq_num:";
    Instruction ("movq", "24(%rbp)", "%rax", "");
    Instruction ("xorl", "%edx", "%edx", "");
    Instruction ("cmpq", "%rax", "24(%rbx)", "");
    Instruction ("movq", "%r12", "%rax", "");
    Instruction ("setne", "%dl", "", "");
    Instruction ("movq", "%rdx", "24(%r12)", "");
    Instruction ("popq", "%rbx", "", "");
    Instruction ("popq", "%rbp", "", "");
    Instruction ("popq", "%r12", "", "");
    Instruction ("ret", "", "", "");
    Instruction ("\t.p2align 4,,10", "", "", "");
    Instruction ("\t.p2align 3", "", "", "");
    Line ".eq_false:";
    Instruction ("xorl", "%edx", "%edx", "");
    Instruction ("movq", "%r12", "%rax", "");
    Instruction ("movq", "%rdx", "24(%r12)", "");
    Instruction ("popq", "%rbx", "", "");
    Instruction ("popq", "%rbp", "", "");
    Instruction ("popq", "%r12", "", "");
    Instruction ("ret", "", "", "");
    Instruction ("\t.p2align 4,,10", "", "", "");
    Instruction ("\t.p2align 3", "", "", "");
    Line ".eq_string:";
    Instruction ("movq", "24(%rbp)", "%rsi", "");
    Instruction ("movq", "24(%rbx)", "%rdi", "");
    Instruction ("call", "strcmp", "", "");
    Instruction ("xorl", "%edx", "%edx", "");
    Instruction ("testl", "%eax", "%eax", "");
    Instruction ("sete", "%dl", "", "");
    Instruction ("jmp", ".eq_cleanup", "", "");
    Instruction ("\t.size", "eq_handler", ".-eq_handler", "");
  ]

let add_var_addr (var_name : string) =
  let fp_offset = 8 * Hashtbl.length var_locations in
  Hashtbl.add var_locations var_name fp_offset

let get_var_addr (var_name : string) : string =
  match Hashtbl.find_opt var_locations var_name with
  | Some addr -> Printf.sprintf "%d(%%rbp)" addr
  | None -> var_name

let pusha =
  [
    Instruction ("pushq", "%rax", "", "");
    Instruction ("pushq", "%rbx", "", "");
    Instruction ("pushq", "%rbp", "", "");
    Instruction ("pushq", "%rdi", "", "");
    Instruction ("pushq", "%rsi", "", "");
    Instruction ("pushq", "%rcx", "", "");
    Instruction ("pushq", "%rdx", "", "");
    Instruction ("pushq", "%r8", "", "");
    Instruction ("pushq", "%r9", "", "");
    Instruction ("pushq", "%r10", "", "");
    Instruction ("pushq", "%r11", "", "");
    Instruction ("pushq", "%r12", "", "");
    Instruction ("pushq", "%r13", "", "");
    Instruction ("pushq", "%r14", "", "");
    Instruction ("pushq", "%r15", "", "");
  ]

let popa =
  [
    Instruction ("popq", "%rax", "", "");
    Instruction ("popq", "%rbx", "", "");
    Instruction ("popq", "%rbp", "", "");
    Instruction ("popq", "%rdi", "", "");
    Instruction ("popq", "%rsi", "", "");
    Instruction ("popq", "%rcx", "", "");
    Instruction ("popq", "%rdx", "", "");
    Instruction ("popq", "%r8", "", "");
    Instruction ("popq", "%r9", "", "");
    Instruction ("popq", "%r10", "", "");
    Instruction ("popq", "%r11", "", "");
    Instruction ("popq", "%r12", "", "");
    Instruction ("popq", "%r13", "", "");
    Instruction ("popq", "%r14", "", "");
    Instruction ("popq", "%r15", "", "");
  ]

(** Method to convert a TAC element to assembly code *)
let tac_to_as (tac : tac_elem) cur_method =
  match tac.operand with
  (****************** TODO ******************)
  | Assignment ->
      let result = get_var_addr tac.result in
      let arg1 = get_var_addr tac.arg1 in
      [
        Instruction ("movq", arg1, "%rax", "");
        Instruction ("movq", "%rax", result, "");
      ]
  | Bt ->
      let arg1 = get_var_addr tac.arg1 in
      [
        Instruction ("movq", arg1, "%rax", "");
        Instruction ("testq", "%rax", "%rax", "");
        Instruction ("jne", tac.arg2, "", "");
      ]
  | Call ->
      add_var_addr tac.result;
      let result = get_var_addr tac.result in
      if tac.arg2 = "" then
        pusha
        @ [
            Instruction ("andq", "$0xFFFFFFFFFFFFFFF0", "%rsp", "");
            Instruction ("call", tac.arg1, "", "");
            Instruction ("movq", "%rax", result, "");
          ]
        @ popa
      else
        (*let args = String.split_on_char ' ' tac.arg2 in*)
        (*let arglist =*)
        (*List.fold_left*)
        (*(fun acc itm -> acc @ [ Instruction ("pushq", itm, "", "") ])*)
        (*[] args*)
        let arglist =
          [ Instruction ("movq", get_var_addr tac.arg2, "%rsi", "") ]
        in
        pusha @ arglist
        @ [
            Instruction ("andq", "$0xFFFFFFFFFFFFFFF0", "%rsp", "");
            Instruction ("call", tac.arg1, "", "");
            Instruction ("movq", "%rax", result, "");
          ]
        @ popa
      (* Push all variables onto stack *)
      (* Push all onto stack *)
      (*[Instruction{instruction = "callq"; arg1 = Some tac.arg1; arg2 = ""; arg3 = ""}]*)
      (*Printf.fprintf out_file "\tcallq %s\n" tac.arg1*)
  | Comment ->
      [ Line ("#" ^ tac.arg1) ]
      (*Printf.sprintf out_file "\t%s\n" ("#" ^ tac.arg1)*)
  | Label -> [ Line (Printf.sprintf "%s:" tac.arg1) ]
  | Jmp -> [ Instruction ("jmp", tac.arg1, "", "") ]
  (* | Case *)
  (* | Default *)
  (****************** TODO ******************)
  | Return ->
      [
        (*Instruction ("movq", "%rbp", "%rsp", "");*)
        (*Instruction ("popq", "%rbp", "%rsp", "");*)
        (*Instruction ("ret", "", "", "");*)
        Instruction ("jmp", "." ^ cur_method ^ ".end", "", "");
      ]
  | LetNoInit ->
      add_var_addr tac.result;
      let result = get_var_addr tac.result in
      [
        (*Instruction ("movq", "$0", result, "");*)
        Instruction ("pushq", "%rbp", "", "");
        Instruction ("pushq", "%rax", "", "");
        Instruction ("call", "$" ^ tac.arg2 ^ "..new", "", "");
        Instruction ("movq", "%rax", "%r10", "");
        Instruction ("popq", "%rax", "", "");
        Instruction ("movq", "%r10", result, "");
      ]
  (****************** TODO ******************)
  | Ident_Expr s ->
      add_var_addr tac.result;
      let result = get_var_addr tac.result in
      [
        Instruction ("movq", get_var_addr s, "%rax", "");
        Instruction ("movq", "%rax", result, "");
      ]
  (*| New *)
  (* | Isvoid *)
  | Plus ->
      let arg1 = get_var_addr tac.arg1 in
      let arg2 = get_var_addr tac.arg2 in
      add_var_addr tac.result;
      let result = get_var_addr tac.result in

      [
        Instruction ("movq", arg1, "%rax", "");
        Instruction ("movq", "24(%rax)", "%rax", "");
        Instruction ("movq", arg2, "%rdx", "");
        Instruction ("movq", "24(%rdx)", "%rdx", "");
        Instruction ("addl", "%edx", "%eax", "");
        Instruction ("pushq", "%rbp", "", "");
        Instruction ("pushq", "%rax", "", "");
        Instruction ("call", "$Int..new", "", "");
        Instruction ("movq", "%rax", "%r10", "");
        Instruction ("popq", "%rax", "", "");
        Instruction ("popq", "%rbp", "", "");
        Instruction ("movq", "%rax", "24(%r10)", "");
        Instruction ("movq", "%r10", result, "");
      ]
  | Minus ->
      let arg1 = get_var_addr tac.arg1 in
      let arg2 = get_var_addr tac.arg2 in
      add_var_addr tac.result;
      let result = get_var_addr tac.result in

      [
        Instruction ("movq", arg1, "%rax", "");
        Instruction ("movq", "24(%rax)", "%rax", "");
        Instruction ("movq", arg2, "%rdx", "");
        Instruction ("movq", "24(%rdx)", "%rdx", "");
        Instruction ("subl", "%edx", "%eax", "");
        Instruction ("pushq", "%rbp", "", "");
        Instruction ("pushq", "%rax", "", "");
        Instruction ("call", "$Int..new", "", "");
        Instruction ("movq", "%rax", "%r10", "");
        Instruction ("popq", "%rax", "", "");
        Instruction ("popq", "%rbp", "", "");
        Instruction ("movq", "%rax", "24(%r10)", "");
        Instruction ("movq", "%r10", result, "");
      ]
  | Divide ->
      let arg1 = get_var_addr tac.arg1 in
      let arg2 = get_var_addr tac.arg2 in
      add_var_addr tac.result;
      let result = get_var_addr tac.result in

      [
        Instruction ("movq", arg2, "%rcx", "");
        Instruction ("movq", "24(%rcx)", "%rcx", "");
        Instruction ("movq", arg1, "%rax", "");
        Instruction ("movq", "24(%rax)", "%rax", "");
        Instruction ("cltd", "", "", "");
        Instruction ("idivl", "%rcx", "", "");
        Instruction ("pushq", "%rbp", "", "");
        Instruction ("pushq", "%rax", "", "");
        Instruction ("call", "$Int..new", "", "");
        Instruction ("movq", "%rax", "%r10", "");
        Instruction ("popq", "%rax", "", "");
        Instruction ("popq", "%rbp", "", "");
        Instruction ("movq", "%rax", "24(%r10)", "");
        Instruction ("movq", "%r10", result, "");
      ]
  | Times ->
      let arg1 = get_var_addr tac.arg1 in
      let arg2 = get_var_addr tac.arg2 in
      add_var_addr tac.result;
      let result = get_var_addr tac.result in
      [
        Instruction ("movq", arg1, "%rax", "");
        Instruction ("movq", "24(%rax)", "%rax", "");
        Instruction ("movq", arg2, "%rdx", "");
        Instruction ("movq", "24(%rdx)", "%rdx", "");
        Instruction ("imull", "%edx", "%eax", "");
        Instruction ("pushq", "%rbp", "", "");
        Instruction ("pushq", "%rax", "", "");
        Instruction ("call", "$Int..new", "", "");
        Instruction ("movq", "%rax", "%r10", "");
        Instruction ("popq", "%rax", "", "");
        Instruction ("popq", "%rbp", "", "");
        Instruction ("movq", "%rax", "24(%r10)", "");
        Instruction ("movq", "%r10", result, "");
      ]
  | LessThan ->
      let arg1 = get_var_addr tac.arg1 in
      let arg2 = get_var_addr tac.arg2 in
      add_var_addr tac.result;
      let result = get_var_addr tac.result in

      [
        Instruction ("pushq", "%rdi", "", "");
        Instruction ("pushq", "%rsi", "", "");
        Instruction ("movq", arg1, "%rdi", "");
        Instruction ("movq", arg2, "%rsi", "");
        Instruction ("call", "lt_handler", "", "");
        Instruction ("popq", "%rsi", "", "");
        Instruction ("popq", "%rdi", "", "");
        Instruction ("movq", "%rax", result, "");
      ]
  | LessEqual ->
      let arg1 = get_var_addr tac.arg1 in
      let arg2 = get_var_addr tac.arg2 in
      add_var_addr tac.result;
      let result = get_var_addr tac.result in

      [
        Instruction ("pushq", "%rdi", "", "");
        Instruction ("pushq", "%rsi", "", "");
        Instruction ("movq", arg1, "%rdi", "");
        Instruction ("movq", arg2, "%rsi", "");
        Instruction ("call", "le_handler", "", "");
        Instruction ("popq", "%rsi", "", "");
        Instruction ("popq", "%rdi", "", "");
        Instruction ("movq", "%rax", result, "");
      ]
  | Equal ->
      let arg1 = get_var_addr tac.arg1 in
      let arg2 = get_var_addr tac.arg2 in
      add_var_addr tac.result;
      let result = get_var_addr tac.result in

      [
        Instruction ("pushq", "%rdi", "", "");
        Instruction ("pushq", "%rsi", "", "");
        Instruction ("movq", arg1, "%rdi", "");
        Instruction ("movq", arg2, "%rsi", "");
        Instruction ("call", "eq_handler", "", "");
        Instruction ("popq", "%rsi", "", "");
        Instruction ("popq", "%rdi", "", "");
        Instruction ("movq", "%rax", result, "");
      ]
  | Not ->
      let arg1 = get_var_addr tac.arg1 in
      add_var_addr tac.result;
      let result = get_var_addr tac.result in

      [
        Instruction ("movq", arg1, "%rax", "");
        Instruction ("movq", "24(%rax)", "%rax", "");
        Instruction ("testq", "%rax", "%rax", "");
        Instruction ("movl", "$1", "%eax", "");
        Instruction ("movl", "$0", "%edx", "");
        Instruction ("cmovel", "%eax", "%edx", "");
        Instruction ("pushq", "%rbp", "", "");
        Instruction ("pushq", "%rdx", "", "");
        Instruction ("call", "Bool..new", "", "");
        Instruction ("popq", "%rdx", "", "");
        Instruction ("popq", "%rbp", "", "");
        Instruction ("movq", "%rdx", "24(%rax)", "");
        Instruction ("movq", "%rax", result, "");
      ]
  | Negate ->
      let arg1 = get_var_addr tac.arg1 in
      add_var_addr tac.result;
      let result = get_var_addr tac.result in

      [
        Instruction ("movq", arg1, "%rax", "");
        Instruction ("movq", "24(%rax)", "%rax", "");
        Instruction ("notq", "%rax", "", "");
        Instruction ("pushq", "%rbp", "", "");
        Instruction ("pushq", "%rax", "", "");
        Instruction ("call", "$Int..new", "", "");
        Instruction ("movq", "%rax", "%r10", "");
        Instruction ("popq", "%rax", "", "");
        Instruction ("popq", "%rbp", "", "");
        Instruction ("movq", "%rax", "24(%r10)", "");
        Instruction ("movq", "%r10", result, "");
      ]
  | Int_Constant ->
      add_var_addr tac.result;
      let result = get_var_addr tac.result in
      [
        Instruction ("call", "$Int..new", "", "");
        Instruction ("movq", "$" ^ tac.arg1, "24(%rax)", "");
        Instruction ("movq", "%rax", result, "");
      ]
      (****************** TODO ******************)
  | String_Constant -> (
      add_var_addr tac.result;
      let result = get_var_addr tac.result in
      match Hashtbl.find_opt string_map tac.arg1 with
      | Some str_id ->
          [
            Instruction ("call", "String..new", "", "");
            Instruction
              ("movq", "$.string" ^ string_of_int str_id, "24(%rax)", "");
            Instruction ("movq", "%rax", result, "");
            (*Instruction ("movq", "$.string" ^ string_of_int str_id, result, "");*)
          ]
      | None ->
          string_counter := !string_counter + 1;
          Hashtbl.add string_map tac.arg1 !string_counter;

          [
            Instruction ("call", "String..new", "", "");
            Instruction
              ( "movq",
                "$.string" ^ string_of_int !string_counter,
                "24(%rax)",
                "" );
            Instruction ("movq", "%rax", result, "");
            (*Instruction*)
            (*("movq", ".string" ^ string_of_int !string_counter, result, "");*)
          ]
      (* This is all we do because they're emitted later :) *)
      (* This is the later but that's another stage; needs to be NOT just in an expression lm ao*)
      (*Printf.fprintf out_file "\t%s\n" (".secton\t.rodata");*)
      (*Printf.fprintf out_file "%s\n" ("string" ^ string_of_int(!string_counter) ^ ":");*)
      (*Printf.fprintf out_file "\t%s\n" (".string \"" ^ string_of_int(!string_counter) ^ "\"");*)
      (*Printf.fprintf out_file "\t%s\n" (".string \"" ^ string_of_int(!string_counter) ^ "\"")*)
      )
  | Boolean_Constant ->
      add_var_addr tac.result;
      let result = get_var_addr tac.result in
      if tac.arg1 = "true" then
        [
          Instruction ("call", "Bool..new", "", "");
          Instruction ("movq", "$1", "24(%rax)", "");
          Instruction ("movq", "%rax", result, "");
        ]
      else
        [
          Instruction ("call", "Bool..new", "", "");
          Instruction ("movq", "$0", "24(%rax)", "");
          Instruction ("movq", "%rax", result, "");
        ]
  | _ -> assert false

let tac_list_to_asm lst = List.map tac_to_as lst
