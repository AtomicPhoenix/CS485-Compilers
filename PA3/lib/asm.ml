open Print
open Parser
open Tac
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

and vtable = {
  name_id : string;
  name_string_id : int;
  methods : vtable_func list;
}

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

let print_start () =
  Printf.fprintf Print.out_file
    "\t.globl start\n\
     start:\n\
     \t.globl main\n\
     \t.type main, @function\n\
     main:\n\
     \tpushq\t%%rbp\n\
     \tcall\tMain.main\t\n\
     andq\t$-16, %%rsp\n\
     \txorq\t%%rdi, %%rdi\n\
     \tcall\texit\n"

let print_asm (asm : asm_line) =
  match asm with
  | Instruction (s1, s2, s3, s4) ->
      if s4 <> "" then (*Printf.printf "\t%s\t%s, %s, %s\n" s1 s2 s3 s4;*)
        Printf.fprintf out_file "\t%s\t%s, %s, %s\n" s1 s2 s3 s4
      else if s3 <> "" then (*Printf.printf "\t%s\t%s, %s\n" s1 s2 s3;*)
        Printf.fprintf out_file "\t%s\t%s, %s\n" s1 s2 s3
      else if s2 <> "" then (*Printf.printf "\t%s\t%s\n" s1 s2;*)
        Printf.fprintf out_file "\t%s\t%s\n" s1 s2
      else if s1 <> "" then (*Printf.printf "\t%s\n" s1;*)
        Printf.fprintf out_file "\t%s\n" s1
  | Line s1 -> (*Printf.printf "%s\n" s1;*) Printf.fprintf out_file "%s\n" s1

let print_new_funcs funcs =
  let print_new_func func =
    let name, lines = func in
    Printf.fprintf Print.out_file "\t.p2align 4\n";
    Printf.fprintf Print.out_file "\t.globl\t%s..new\n" name;
    Printf.fprintf Print.out_file "\t.type\t%s..new, @function\n" name;
    List.iter (fun ln -> print_asm ln) lines;
    (*Printf.fprintf Print.out_file "\t.size\t%s, .-%s\n" name name;*)
    Printf.fprintf Print.out_file
      "\t#;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;\n"
  in
  List.iter print_new_func funcs

let var_locations = Hashtbl.create 32

(* let string_map = Hashtbl.create 32*)
let string_map = Hashtbl.create 32
let class_id_map = Hashtbl.create 32
let class_vtable_map = Hashtbl.create 32
let class_attribute_map = Hashtbl.create 32
let string_counter = ref 8
let class_tag_ctr = ref 9
let parser_class_map : class_map_elem list = Parser.parser_class_map
let implementation_map = Parser.implementation_map
let parent_map = Parser.parent_map
let vtable_list : vtable list ref = ref []

let print_string_map () =
  Hashtbl.iter
    (fun k v ->
      Printf.fprintf Print.out_file "string%d:\n\t.string\t\"%s\"\n" v k)
    string_map;
  Printf.fprintf Print.out_file
    "\t.globl empty.string\nempty.string:\n\t.string\t\"\"\n";
  Printf.fprintf Print.out_file
    "\t.globl percent.ld\npercent.ld:\n\t.string\t\"%%ld\"\n";
  Printf.fprintf Print.out_file
    "\t.globl percent.d\npercent.d:\n\t.string\t\"%%d\"\n";
  Printf.fprintf Print.out_file "\t.text\n"

let create_vtable (itm : implementation_map_elem) : vtable option =
  let name = itm.name in
  if
    name <> "Bool" && name <> "IO" && name <> "Int" && name <> "Object"
    && name <> "String"
  then (
    let funcs =
      List.map
        (fun (meth : imp_method) ->
          { type_name = meth.type_name; method_name = meth.name })
        itm.methods
    in
    string_counter := !string_counter + 1;
    Hashtbl.add string_map itm.name !string_counter;
    Some { name_id = name; name_string_id = !string_counter; methods = funcs })
  else None

let create_vtables () =
  let string6 = "abort" in
  let string7 = "ERROR: 0: Exception: String.substr out of range\\n" in
  Hashtbl.add string_map string6 6;
  Hashtbl.add string_map string7 7;
  let tables = List.filter_map create_vtable implementation_map in
  List.iter
    (fun i ->
      Hashtbl.add class_vtable_map i.name_id i;
      vtable_list := !vtable_list @ [ i ])
    tables

let rec print_vtable_func func =
  Printf.fprintf Print.out_file "\t.quad %s.%s\n" func.type_name
    func.method_name

and print_vtable (table : vtable) =
  let name = table.name_id in
  let strid = table.name_string_id in
  Printf.fprintf Print.out_file ".globl %s..vtable\n" name;
  Printf.fprintf Print.out_file "%s..vtable:\n" name;
  Printf.fprintf Print.out_file "\t.quad string%d\n" strid;
  List.iter print_vtable_func table.methods;
  Printf.fprintf Print.out_file
    "\t#;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;\n"

let create_default_vtables () =
  Hashtbl.add string_map "Bool" 0;
  Hashtbl.add string_map "IO" 1;
  Hashtbl.add string_map "Int" 2;
  Hashtbl.add string_map "Object" 3;
  Hashtbl.add string_map "String" 4;
  [
    {
      name_id = "Bool";
      name_string_id = 0;
      methods =
        [
          { type_name = "Bool"; method_name = ".new" };
          { type_name = "Object"; method_name = "abort" };
          { type_name = "Object"; method_name = "copy" };
          { type_name = "Object"; method_name = "type_name" };
        ];
    };
    {
      name_id = "IO";
      name_string_id = 1;
      methods =
        [
          { type_name = "IO"; method_name = ".new" };
          { type_name = "Object"; method_name = "abort" };
          { type_name = "Object"; method_name = "copy" };
          { type_name = "Object"; method_name = "type_name" };
          { type_name = "IO"; method_name = "in_int" };
          { type_name = "IO"; method_name = "in_string" };
          { type_name = "IO"; method_name = "out_int" };
          { type_name = "IO"; method_name = "out_string" };
        ];
    };
    {
      name_id = "Int";
      name_string_id = 2;
      methods =
        [
          { type_name = "Int"; method_name = ".new" };
          { type_name = "Object"; method_name = "abort" };
          { type_name = "Object"; method_name = "copy" };
          { type_name = "Object"; method_name = "type_name" };
        ];
    };
    {
      name_id = "Object";
      name_string_id = 3;
      methods =
        [
          { type_name = "Object"; method_name = ".new" };
          { type_name = "Object"; method_name = "abort" };
          { type_name = "Object"; method_name = "copy" };
          { type_name = "Object"; method_name = "type_name" };
        ];
    };
    {
      name_id = "String";
      name_string_id = 4;
      methods =
        [
          { type_name = "String"; method_name = ".new" };
          { type_name = "Object"; method_name = "abort" };
          { type_name = "Object"; method_name = "copy" };
          { type_name = "Object"; method_name = "type_name" };
          { type_name = "String"; method_name = "concat" };
          { type_name = "String"; method_name = "length" };
          { type_name = "String"; method_name = "substr" };
        ];
    };
  ]

let get_class_attributes class_name attrs =
  let get_attribute i (attr : ast_attribute) =
    let name = attr.name in
    let index = i in
    let typename = attr.type_name in
    let expr = attr.attr_expr in
    { field_name = name; index; type_name = typename; expression = expr }
  in
  let attributes = List.mapi get_attribute attrs in
  Hashtbl.add class_attribute_map class_name attributes;
  attributes

let make_asm_class (c : class_map_elem) =
  let tag = !class_tag_ctr in
  Hashtbl.add class_id_map c.name tag;
  (* 8 bytes/64 bits since every attribute is a pointer *)
  let siz = List.length c.attrs in
  let class_vtable = Hashtbl.find_opt class_vtable_map c.name in
  let attrs = get_class_attributes c.name c.attrs in
  match class_vtable with
  | Some cv ->
      Some
        { class_tag = tag; object_size = siz; vtable = cv; attributes = attrs }
  | None ->
      (* Printf.fprintf out_file "#; No vtable found for class %s\n" c.name; *)
      None

let abort =
  [
    Line "\t.globl\tObject.abort";
    Line "Object.abort:";
    Instruction ("pushq", "%rbp", "", "");
    Instruction ("movq", "%rsp", "%rbp", "");
    Instruction ("movq", "16(%rbp)", "%r12", "");
    Instruction ("movq", "$16", "%r14", "");
    Instruction ("subq", "%r14", "%rsp", "");
    Instruction ("movq", "$string6", "%r13", "");
    Instruction ("andq", "$0xFFFFFFFFFFFFFFF0", "%rsp", "");
    Instruction ("movq", "%r13", "%rdi", "");
    Instruction ("call", "cooloutstr", "", "");
    Instruction ("andq", "$0xFFFFFFFFFFFFFFF0", "%rsp", "");
    Instruction ("movl", "$0", "%edi", "");
    Instruction ("call", "exit", "", "");
    Line "Object.abort.end:";
    Instruction ("movq", "%rbp", "%rsp", "");
    Instruction ("popq", "%rbp", "", "");
    Instruction ("ret", "", "", "");
  ]

let copy =
  [
    Line "\t.p2align 4";
    Line "\t.globl\tObject.copy";
    Line "Object.copy:";
    Instruction ("pushq", "%rbp", "", "");
    Instruction ("movq", "%rsp", "%rbp", "");
    Instruction ("movq", "16(%rbp)", "%r12", "");
    Instruction ("movq", "$16", "%r14", "");
    Instruction ("subq", "%r14", "%rsp", "");
    Instruction ("movq", "8(%r12)", "%r14", "");
    Instruction ("andq", "$0xFFFFFFFFFFFFFFF0", "%rsp", "");
    Instruction ("movq", "$8", "%rsi", "");
    Instruction ("movq", "%r14", "%rdi", "");
    Instruction ("call", "calloc", "", "");
    Instruction ("movq", "%rax", "%r13", "");
    Instruction ("pushq", "%r13", "", "");
    Line "\t.globl\tObject.copy.end";
    Line "Object.copy.end:";
    Instruction ("movq", "%rbp", "%rsp", "");
    Instruction ("popq", "%rbp", "", "");
    Instruction ("ret", "", "", "");
  ]

let type_name =
  [
    Line "Object.type_name:";
    Line "\t.globl\tObject.type_name";
    Instruction ("pushq", "%rbp", "", "");
    Instruction ("movq", "%rsp", "%rbp", "");
    Instruction ("movq", "16(%rbp)", "%r12", "");
    Instruction ("movq", "$16", "%r14", "");
    Instruction ("subq", "%r14", "%rsp", "");
    Instruction ("pushq", "%rbp", "", "");
    Instruction ("pushq", "%r12", "", "");
    Instruction ("movq", "$String..new", "%r14", "");
    Instruction ("call", "*%r14", "", "");
    Instruction ("popq", "%r12", "", "");
    Instruction ("popq", "%rbp", "", "");
    Instruction ("movq", "16(%r12)", "%r14", "");
    Instruction ("movq", "0(%r14)", "%r14", "");
    Instruction ("movq", "%r14", "24(%r13)", "");
    Line "Object.type_name.end:";
    Instruction ("movq", "%rbp", "%rsp", "");
    Instruction ("popq", "%rbp", "", "");
    Instruction ("ret", "", "", "");
  ]

let in_int =
  [
    Line "\t.p2align 4";
    Line "\t.globl\tIO.in_int";
    Line "\t.type\tIO.in_int, @function";
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
    Instruction ("movq", "8(%rsp)", "%rcx", "");
    Instruction ("movl", "$2147483648", "%edx", "");
    Instruction ("addq", "%rcx", "%rdx", "");
    Instruction ("shrq", "$32", "%rdx", "");
    Instruction ("jne", ".in_int_zero", "", "");
    Instruction ("testl", "%eax", "%eax", "");
    Instruction ("jg", ".in_int_nonzero", "", "");
    Line ".in_int_zero:";
    Instruction ("xorl", "%ecx", "%ecx", "");
    Line ".in_int_nonzero:";
    Instruction ("movq", "%rcx", "24(%rbx)", "");
    (*Instruction ("movl", "$4294967295", "%ecx", "");*)
    (*Instruction ("addq", "%rcx", "%rdx", "");*)
    (*Instruction ("cmpq", "%rdx", "%rcx", "");*)
    (*Instruction ("movl", "$0", "%edx", "");*)
    (*Instruction ("cmovb", "%rdx", "%rax", "");*)
    (*Instruction ("movq", "%rax", "24(%rbx)", "");*)
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
    Line "\t.type\tIO.out_int, @function";
    Line "IO.out_int:";
    Instruction ("pushq", "%rbx", "", "");
    (*Instruction ("subq", "$8", "%rsp", "");*)
    Instruction ("movq", "24(%rsi)", "%rsi", "");
    Instruction ("movq", "%rdi", "%rbx", "");
    Instruction ("xorl", "%eax", "%eax", "");
    Instruction ("movq", "$percent.d", "%rdi", "");
    Instruction ("call", "printf", "", "");
    Instruction ("movq", "%rbx", "%rax", "");
    (*Instruction ("addq", "$8", "%rsp", "");*)
    Instruction ("popq", "%rbx", "", "");
    Instruction ("ret", "", "", "");
    (*Instruction (".size", "IO.out_int", ".-IO.out_int", "");*)
  ]

let in_string =
  [
    Line ".globl\tIO.in_string";
    Line "IO.in_string:";
    Line "\t## method definition";
    Instruction ("pushq", "%rbp", "", "");
    Instruction ("movq", "%rsp", "%rbp", "");
    Instruction ("movq", "16(%rbp)", "%r12", "");
    Line "\t## stack room for temporaries: 2";
    Instruction ("movq", "$16", "%r14", "");
    Instruction ("subq", "%r14", "%rsp", "");
    Line "\t## return address handling";
    Line "\t## method body begins";
    Line "\t## new String";
    Instruction ("pushq", "%rbp", "", "");
    Instruction ("pushq", "%r12", "", "");
    Instruction ("movq", "$String..new", "%r14", "");
    Instruction ("call", "*%r14", "", "");
    Instruction ("popq", "%r12", "", "");
    Instruction ("popq", "%rbp", "", "");
    Instruction ("movq", "%r13", "%r14", "");
    Line "\t## guarantee 16-byte alignment before call";
    Instruction ("andq", "$0xFFFFFFFFFFFFFFF0", "%rsp", "");
    Instruction ("call", "coolgetstr", "", "");
    Instruction ("movq", "%rax", "%r13", "");
    Instruction ("movq", "%r13", "24(%r14)", "");
    Instruction ("movq", "%r14", "%r13", "");
    Line ".globl\tIO.in_string.end";
    Line "IO.in_string.end:";
    Line "\t## method body ends";
    Line "\t## return address handling";
    Instruction ("movq", "%rbp", "%rsp", "");
    Instruction ("popq", "%rbp", "", "");
    Instruction ("ret", "", "", "");
    Line "\t## ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;";
  ]

let out_string =
  [
    Line "\t.p2align 4";
    Line "\t.globl\tIO.out_string";
    Line "IO.out_string:";
    Instruction ("pushq", "%rbp", "", "");
    Instruction ("movq", "%rsp", "%rbp", "");
    Instruction ("movq", "16(%rbp)", "%r12", "");
    Instruction ("movq $16, %r14", "", "", "");
    Instruction ("subq %r14, %rsp", "", "", "");
    Instruction ("movq 24(%rbp), %r14", "", "", "");
    Instruction ("movq 24(%r14), %r13", "", "", "");
    Instruction ("andq $0xFFFFFFFFFFFFFFF0, %rsp", "", "", "");
    Instruction ("movq %r13, %rdi", "", "", "");
    Instruction ("call cooloutstr", "", "", "");
    Instruction ("movq %r12, %r13", "", "", "");
    Instruction ("movq %rbp, %rsp", "", "", "");
    Instruction ("popq %rbp", "", "", "");
    Instruction ("ret", "", "", "");
  ]

let cooloutstr =
  [
    Instruction (".globl\tcooloutstr", "", "", "");
    Instruction (".type\tcooloutstr, @function", "", "", "");
    Instruction ("cooloutstr:", "", "", "");
    Instruction (".LFB6:", "", "", "");
    Instruction (".cfi_startproc", "", "", "");
    Instruction ("endbr64", "", "", "");
    Instruction ("pushq\t%rbp", "", "", "");
    Instruction (".cfi_def_cfa_offset 16", "", "", "");
    Instruction (".cfi_offset 6, -16", "", "", "");
    Instruction ("movq\t%rsp, %rbp", "", "", "");
    Instruction (".cfi_def_cfa_register 6", "", "", "");
    Instruction ("subq\t$32, %rsp", "", "", "");
    Instruction ("movq\t%rdi, -24(%rbp)", "", "", "");
    Instruction ("movl\t$0, -4(%rbp)", "", "", "");
    Instruction ("jmp\t.L2", "", "", "");
    Instruction (".L5:", "", "", "");
    Instruction ("movl\t-4(%rbp), %eax", "", "", "");
    Instruction ("movslq\t%eax, %rdx", "", "", "");
    Instruction ("movq\t-24(%rbp), %rax", "", "", "");
    Instruction ("addq\t%rdx, %rax", "", "", "");
    Instruction ("movzbl\t(%rax), %eax", "", "", "");
    Instruction ("cmpb\t$92, %al", "", "", "");
    Instruction ("jne\t.L3", "", "", "");
    Instruction ("movl\t-4(%rbp), %eax", "", "", "");
    Instruction ("cltq", "", "", "");
    Instruction ("leaq\t1(%rax), %rdx", "", "", "");
    Instruction ("movq\t-24(%rbp), %rax", "", "", "");
    Instruction ("addq\t%rdx, %rax", "", "", "");
    Instruction ("movzbl\t(%rax), %eax", "", "", "");
    Instruction ("cmpb\t$110, %al", "", "", "");
    Instruction ("jne\t.L3", "", "", "");
    Instruction ("movq\tstdout(%rip), %rax", "", "", "");
    Instruction ("movq\t%rax, %rsi", "", "", "");
    Instruction ("movl\t$10, %edi", "", "", "");
    Instruction ("call\tfputc@PLT", "", "", "");
    Instruction ("addl\t$2, -4(%rbp)", "", "", "");
    Instruction ("jmp\t.L2", "", "", "");
    Instruction (".L3:", "", "", "");
    Instruction ("movl\t-4(%rbp), %eax", "", "", "");
    Instruction ("movslq\t%eax, %rdx", "", "", "");
    Instruction ("movq\t-24(%rbp), %rax", "", "", "");
    Instruction ("addq\t%rdx, %rax", "", "", "");
    Instruction ("movzbl\t(%rax), %eax", "", "", "");
    Instruction ("cmpb\t$92, %al", "", "", "");
    Instruction ("jne\t.L4", "", "", "");
    Instruction ("movl\t-4(%rbp), %eax", "", "", "");
    Instruction ("cltq", "", "", "");
    Instruction ("leaq\t1(%rax), %rdx", "", "", "");
    Instruction ("movq\t-24(%rbp), %rax", "", "", "");
    Instruction ("addq\t%rdx, %rax", "", "", "");
    Instruction ("movzbl\t(%rax), %eax", "", "", "");
    Instruction ("cmpb\t$116, %al", "", "", "");
    Instruction ("jne\t.L4", "", "", "");
    Instruction ("movq\tstdout(%rip), %rax", "", "", "");
    Instruction ("movq\t%rax, %rsi", "", "", "");
    Instruction ("movl\t$9, %edi", "", "", "");
    Instruction ("call\tfputc@PLT", "", "", "");
    Instruction ("addl\t$2, -4(%rbp)", "", "", "");
    Instruction ("jmp\t.L2", "", "", "");
    Instruction (".L4:", "", "", "");
    Instruction ("movq\tstdout(%rip), %rdx", "", "", "");
    Instruction ("movl\t-4(%rbp), %eax", "", "", "");
    Instruction ("movslq\t%eax, %rcx", "", "", "");
    Instruction ("movq\t-24(%rbp), %rax", "", "", "");
    Instruction ("addq\t%rcx, %rax", "", "", "");
    Instruction ("movzbl\t(%rax), %eax", "", "", "");
    Instruction ("movsbl\t%al, %eax", "", "", "");
    Instruction ("movq\t%rdx, %rsi", "", "", "");
    Instruction ("movl\t%eax, %edi", "", "", "");
    Instruction ("call\tfputc@PLT", "", "", "");
    Instruction ("addl\t$1, -4(%rbp)", "", "", "");
    Instruction (".L2:", "", "", "");
    Instruction ("movl\t-4(%rbp), %eax", "", "", "");
    Instruction ("movslq\t%eax, %rdx", "", "", "");
    Instruction ("movq\t-24(%rbp), %rax", "", "", "");
    Instruction ("addq\t%rdx, %rax", "", "", "");
    Instruction ("movzbl\t(%rax), %eax", "", "", "");
    Instruction ("testb\t%al, %al", "", "", "");
    Instruction ("jne\t.L5", "", "", "");
    Instruction ("movq\tstdout(%rip), %rax", "", "", "");
    Instruction ("movq\t%rax, %rdi", "", "", "");
    Instruction ("call\tfflush@PLT", "", "", "");
    Instruction ("nop", "", "", "");
    Instruction ("leave", "", "", "");
    Instruction (".cfi_def_cfa 7, 8", "", "", "");
    Instruction ("ret", "", "", "");
    Instruction (".cfi_endproc", "", "", "");
    Instruction (".LFE6:", "", "", "");
    Instruction (".size\tcooloutstr, .-cooloutstr", "", "", "");
    Instruction (".globl\tcoolstrlen", "", "", "");
    Instruction (".type\tcoolstrlen, @function", "", "", "");
    Instruction ("", "", "", "");
  ]

let coolstrlen =
  [
    Line "\t.globl\tcoolstrlen";
    Line "\t.type\tcoolstrlen, @function";
    Line "coolstrlen:";
    Line ".LFB7:";
    Line "\t.cfi_startproc";
    Instruction ("endbr64", "", "", "");
    Instruction ("pushq", "%rbp", "", "");
    Line "\t.cfi_def_cfa_offset 16";
    Line "\t.cfi_offset 6, -16";
    Instruction ("movq", "%rsp", "%rbp", "");
    Line "\t.cfi_def_cfa_register 6";
    Instruction ("movq", "%rdi", "-24(%rbp)", "");
    Instruction ("movl", "$0", "-4(%rbp)", "");
    Instruction ("jmp", ".L7", "", "");
    Line ".L8:";
    Instruction ("movl", "-4(%rbp)", "%eax", "");
    Instruction ("addl", "$1", "%eax", "");
    Instruction ("movl", "%eax", "-4(%rbp)", "");
    Line ".L7:";
    Instruction ("movl", "-4(%rbp)", "%eax", "");
    Instruction ("movl", "%eax", "%edx", "");
    Instruction ("movq", "-24(%rbp)", "%rax", "");
    Instruction ("addq", "%rdx", "%rax", "");
    Instruction ("movzbl", "(%rax)", "%eax", "");
    Instruction ("testb", "%al", "%al", "");
    Instruction ("jne", ".L8", "", "");
    Instruction ("movl", "-4(%rbp)", "%eax", "");
    Instruction ("popq", "%rbp", "", "");
    Line "\t.cfi_def_cfa 7, 8";
    Instruction ("ret", "", "", "");
    Line "\t.cfi_endproc";
    Line ".LFE7:";
    Line "\t.size\tcoolstrlen, .-coolstrlen";
  ]

let coolstrcat =
  [
    Line "\t.section\t.rodata";
    Line ".LC0:";
    Line "\t.string\t\"%s%s\"";
    Line "\t.text";
    Line "\t.globl\tcoolstrcat";
    Line "\t.type\tcoolstrcat, @function";
    Line "coolstrcat:";
    Line ".LFB8:";
    Line "\t.cfi_startproc";
    Instruction ("endbr64", "", "", "");
    Instruction ("pushq", "%rbp", "", "");
    Line "\t.cfi_def_cfa_offset 16";
    Line "\t.cfi_offset 6, -16";
    Instruction ("movq", "%rsp", "%rbp", "");
    Line "\t.cfi_def_cfa_register 6";
    Instruction ("pushq", "%rbx", "", "");
    Instruction ("subq", "$40", "%rsp", "");
    Line "\t.cfi_offset 3, -24";
    Instruction ("movq", "%rdi", "-40(%rbp)", "");
    Instruction ("movq", "%rsi", "-48(%rbp)", "");
    Instruction ("cmpq", "$0", "-40(%rbp)", "");
    Instruction ("jne", ".L11", "", "");
    Instruction ("movq", "-48(%rbp)", "%rax", "");
    Instruction ("jmp", ".L12", "", "");
    Line ".L11:";
    Instruction ("cmpq", "$0", "-48(%rbp)", "");
    Instruction ("jne", ".L13", "", "");
    Instruction ("movq", "-40(%rbp)", "%rax", "");
    Instruction ("jmp", ".L12", "", "");
    Line ".L13:";
    Instruction ("movq", "-40(%rbp)", "%rax", "");
    Instruction ("movq", "%rax", "%rdi", "");
    Instruction ("call", "coolstrlen", "", "");
    Instruction ("movl", "%eax", "%ebx", "");
    Instruction ("movq", "-48(%rbp)", "%rax", "");
    Instruction ("movq", "%rax", "%rdi", "");
    Instruction ("call", "coolstrlen", "", "");
    Instruction ("addl", "%ebx", "%eax", "");
    Instruction ("addl", "$1", "%eax", "");
    Instruction ("movl", "%eax", "-28(%rbp)", "");
    Instruction ("movl", "-28(%rbp)", "%eax", "");
    Instruction ("cltq", "", "", "");
    Instruction ("movl", "$1", "%esi", "");
    Instruction ("movq", "%rax", "%rdi", "");
    Instruction ("call", "calloc@PLT", "", "");
    Instruction ("movq", "%rax", "-24(%rbp)", "");
    Instruction ("movl", "-28(%rbp)", "%eax", "");
    Instruction ("movslq", "%eax", "%rsi", "");
    Instruction ("movq", "-48(%rbp)", "%rcx", "");
    Instruction ("movq", "-40(%rbp)", "%rdx", "");
    Instruction ("movq", "-24(%rbp)", "%rax", "");
    Instruction ("movq", "%rcx", "%r8", "");
    Instruction ("movq", "%rdx", "%rcx", "");
    Instruction ("leaq", ".LC0(%rip)", "%rdx", "");
    Instruction ("movq", "%rax", "%rdi", "");
    Instruction ("movl", "$0", "%eax", "");
    Instruction ("call", "snprintf@PLT", "", "");
    Instruction ("movq", "-24(%rbp)", "%rax", "");
    Line ".L12:";
    Instruction ("movq", "-8(%rbp)", "%rbx", "");
    Instruction ("leave", "", "", "");
    Line "\t.cfi_def_cfa 7, 8";
    Instruction ("ret", "", "", "");
    Line "\t.cfi_endproc";
    Line ".LFE8:";
    Line "\t.size\tcoolstrcat, .-coolstrcat";
  ]

let coolgetstr =
  [
    Line "\t.section\t.rodata";
    Line ".LC1:";
    Line "\t.string\t\"\"";
    Line "\t.text";
    Line "\t.globl\tcoolgetstr";
    Line "\t.type\tcoolgetstr, @function";
    Line "coolgetstr:";
    Line ".LFB9:";
    Line "\t.cfi_startproc";
    Instruction ("endbr64", "", "", "");
    Instruction ("pushq", "%rbp", "", "");
    Line "\t.cfi_def_cfa_offset 16";
    Line "\t.cfi_offset 6, -16";
    Instruction ("movq", "%rsp", "%rbp", "");
    Line "\t.cfi_def_cfa_register 6";
    Instruction ("subq", "$16", "%rsp", "");
    Instruction ("movl", "$1", "%esi", "");
    Instruction ("movl", "$40960", "%edi", "");
    Instruction ("call", "calloc@PLT", "", "");
    Instruction ("movq", "%rax", "-8(%rbp)", "");
    Instruction ("movl", "$0", "-16(%rbp)", "");
    Line ".L21:";
    Instruction ("movq", "stdin(%rip)", "%rax", "");
    Instruction ("movq", "%rax", "%rdi", "");
    Instruction ("call", "fgetc@PLT", "", "");
    Instruction ("movl", "%eax", "-12(%rbp)", "");
    Instruction ("cmpl", "$-1", "-12(%rbp)", "");
    Instruction ("je", ".L15", "", "");
    Instruction ("cmpl", "$10", "-12(%rbp)", "");
    Instruction ("jne", ".L16", "", "");
    Line ".L15:";
    Instruction ("cmpl", "$0", "-16(%rbp)", "");
    Instruction ("je", ".L17", "", "");
    Instruction ("leaq", ".LC1(%rip)", "%rax", "");
    Instruction ("jmp", ".L18", "", "");
    Line ".L17:";
    Instruction ("movq", "-8(%rbp)", "%rax", "");
    Instruction ("jmp", ".L18", "", "");
    Line ".L16:";
    Instruction ("cmpl", "$0", "-12(%rbp)", "");
    Instruction ("jne", ".L19", "", "");
    Instruction ("movl", "$1", "-16(%rbp)", "");
    Instruction ("jmp", ".L21", "", "");
    Line ".L19:";
    Instruction ("movq", "-8(%rbp)", "%rax", "");
    Instruction ("movq", "%rax", "%rdi", "");
    Instruction ("call", "coolstrlen", "", "");
    Instruction ("movl", "%eax", "%edx", "");
    Instruction ("movq", "-8(%rbp)", "%rax", "");
    Instruction ("addq", "%rdx", "%rax", "");
    Instruction ("movl", "-12(%rbp)", "%edx", "");
    Instruction ("movb", "%dl", "(%rax)", "");
    Instruction ("jmp", ".L21", "", "");
    Line ".L18:";
    Instruction ("leave", "", "", "");
    Line "\t.cfi_def_cfa 7, 8";
    Instruction ("ret", "", "", "");
    Line "\t.cfi_endproc";
    Line ".LFE9:";
    Line "\t.size\tcoolgetstr, .-coolgetstr";
  ]

let coolsubstr =
  [
    Line "coolsubstr:";
    Line ".LFB10:";
    Line "\t.cfi_startproc";
    Instruction ("endbr64", "", "", "");
    Instruction ("pushq", "%rbp", "", "");
    Line "\t.cfi_def_cfa_offset 16";
    Line "\t.cfi_offset 6, -16";
    Instruction ("movq", "%rsp", "%rbp", "");
    Line "\t.cfi_def_cfa_register 6";
    Instruction ("subq", "$48", "%rsp", "");
    Instruction ("movq", "%rdi", "-24(%rbp)", "");
    Instruction ("movq", "%rsi", "-32(%rbp)", "");
    Instruction ("movq", "%rdx", "-40(%rbp)", "");
    Instruction ("movq", "-24(%rbp)", "%rax", "");
    Instruction ("movq", "%rax", "%rdi", "");
    Instruction ("call", "coolstrlen", "", "");
    Instruction ("movl", "%eax", "-4(%rbp)", "");
    Instruction ("cmpq", "$0", "-32(%rbp)", "");
    Instruction ("js", ".L23", "", "");
    Instruction ("cmpq", "$0", "-40(%rbp)", "");
    Instruction ("js", ".L23", "", "");
    Instruction ("movq", "-32(%rbp)", "%rdx", "");
    Instruction ("movq", "-40(%rbp)", "%rax", "");
    Instruction ("addq", "%rax", "%rdx", "");
    Instruction ("movl", "-4(%rbp)", "%eax", "");
    Instruction ("cltq", "", "", "");
    Instruction ("cmpq", "%rax", "%rdx", "");
    Instruction ("jle", ".L24", "", "");
    Line ".L23:";
    Instruction ("movl", "$0", "%eax", "");
    Instruction ("jmp", ".L25", "", "");
    Line ".L24:";
    Instruction ("movq", "-40(%rbp)", "%rax", "");
    Instruction ("movq", "-32(%rbp)", "%rcx", "");
    Instruction ("movq", "-24(%rbp)", "%rdx", "");
    Instruction ("addq", "%rcx", "%rdx", "");
    Instruction ("movq", "%rax", "%rsi", "");
    Instruction ("movq", "%rdx", "%rdi", "");
    Instruction ("call", "strndup@PLT", "", "");
    Line ".L25:";
    Instruction ("leave", "", "", "");
    Line "\t.cfi_def_cfa 7, 8";
    Instruction ("ret", "", "", "");
    Line "\t.cfi_endproc";
    Line ".LFE10:";
    Line "\t.size\tcoolsubstr, .-coolsubstr";
  ]

let concat =
  [
    Line ".globl String.concat";
    Line "String.concat:";
    Line "## method definition";
    Instruction ("pushq", "%rbp", "", "");
    Instruction ("movq", "%rsp", "%rbp", "");
    Instruction ("movq", "16(%rbp)", "%r12", "");
    Line "## stack room for temporaries: 2";
    Instruction ("movq", "$16", "%r14", "");
    Instruction ("subq", "%r14", "%rsp", "");
    Line "## return address handling";
    Line "## fp[3] holds argument s (String)";
    Line "## method body begins";
    Line "## new String";
    Instruction ("pushq", "%rbp", "", "");
    Instruction ("pushq", "%r12", "", "");
    Instruction ("movq", "$String..new", "%r14", "");
    Instruction ("call", "*%r14", "", "");
    Instruction ("popq", "%r12", "", "");
    Instruction ("popq", "%rbp", "", "");
    Instruction ("movq", "%r13", "%r15", "");
    Instruction ("movq", "24(%rbp)", "%r14", "");
    Instruction ("movq", "24(%r14)", "%r14", "");
    Instruction ("movq", "24(%r12)", "%r13", "");
    Line "## guarantee 16-byte alignment before call";
    Instruction ("andq", "$0xFFFFFFFFFFFFFFF0", "%rsp", "");
    Instruction ("movq", "%r13", "%rdi", "");
    Instruction ("movq", "%r14", "%rsi", "");
    Instruction ("call", "coolstrcat", "", "");
    Instruction ("movq", "%rax", "%r13", "");
    Instruction ("movq", "%r13", "24(%r15)", "");
    Instruction ("movq", "%r15", "%r13", "");
    Line ".globl String.concat.end";
    Line "String.concat.end:";
    Line "## method body ends";
    Line "## return address handling";
    Instruction ("movq", "%rbp", "%rsp", "");
    Instruction ("popq", "%rbp", "", "");
    Instruction ("ret", "", "", "");
    Line "## ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;";
  ]

let string_length =
  [
    Line ".globl String.length";
    Line "String.length:";
    Line "## method definition";
    Instruction ("pushq", "%rbp", "", "");
    Instruction ("movq", "%rsp", "%rbp", "");
    Instruction ("movq", "16(%rbp)", "%r12", "");
    Line "## stack room for temporaries: 2";
    Instruction ("movq", "$16", "%r14", "");
    Instruction ("subq", "%r14", "%rsp", "");
    Line "## return address handling";
    Line "## method body begins";
    Line "## new Int";
    Instruction ("pushq", "%rbp", "", "");
    Instruction ("pushq", "%r12", "", "");
    Instruction ("movq", "$Int..new", "%r14", "");
    Instruction ("call", "*%r14", "", "");
    Instruction ("popq", "%r12", "", "");
    Instruction ("popq", "%rbp", "", "");
    Instruction ("movq", "%r13", "%r14", "");
    Instruction ("movq", "24(%r12)", "%r13", "");
    Line "## guarantee 16-byte alignment before call";
    Instruction ("andq", "$0xFFFFFFFFFFFFFFF0", "%rsp", "");
    Instruction ("movq", "%r13", "%rdi", "");
    Instruction ("movl", "$0", "%eax", "");
    Instruction ("call", "coolstrlen", "", "");
    Instruction ("movq", "%rax", "%r13", "");
    Instruction ("movq", "%r13", "24(%r14)", "");
    Instruction ("movq", "%r14", "%r13", "");
    Line ".globl String.length.end";
    Line "String.length.end:";
    Line "## method body ends";
    Line "## return address handling";
    Instruction ("movq", "%rbp", "%rsp", "");
    Instruction ("popq", "%rbp", "", "");
    Instruction ("ret", "", "", "");
    Line "## ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;";
  ]

let string_substr =
  [
    Line ".globl String.substr";
    Line "String.substr:";
    Line "## method definition";
    Instruction ("pushq", "%rbp", "", "");
    Instruction ("movq", "%rsp", "%rbp", "");
    Instruction ("movq", "16(%rbp)", "%r12", "");
    Line "## stack room for temporaries: 2";
    Instruction ("movq", "$16", "%r14", "");
    Instruction ("subq", "%r14", "%rsp", "");
    Line "## return address handling";
    Line "## fp[4] holds argument i (Int)";
    Line "## fp[3] holds argument l (Int)";
    Line "## method body begins";
    Line "## new String";
    Instruction ("pushq", "%rbp", "", "");
    Instruction ("pushq", "%r12", "", "");
    Instruction ("movq", "$String..new", "%r14", "");
    Instruction ("call", "*%r14", "", "");
    Instruction ("popq", "%r12", "", "");
    Instruction ("popq", "%rbp", "", "");
    Instruction ("movq", "%r13", "%r15", "");
    Instruction ("movq", "24(%rbp)", "%r14", "");
    Instruction ("movq", "24(%r14)", "%r14", "");
    Instruction ("movq", "32(%rbp)", "%r13", "");
    Instruction ("movq", "24(%r13)", "%r13", "");
    Instruction ("movq", "24(%r12)", "%r12", "");
    Line "## guarantee 16-byte alignment before call";
    Instruction ("andq", "$0xFFFFFFFFFFFFFFF0", "%rsp", "");
    Instruction ("movq", "%r12", "%rdi", "");
    Instruction ("movq", "%r13", "%rsi", "");
    Instruction ("movq", "%r14", "%rdx", "");
    Instruction ("call", "coolsubstr", "", "");
    Instruction ("movq", "%rax", "%r13", "");
    Instruction ("cmpq", "$0", "%r13", "");
    Instruction ("jne", "l6", "", "");
    Instruction ("movq", "$string7", "%r13", "");
    Line "## guarantee 16-byte alignment before call";
    Instruction ("andq", "$0xFFFFFFFFFFFFFFF0", "%rsp", "");
    Instruction ("movq", "%r13", "%rdi", "");
    Instruction ("call", "cooloutstr", "", "");
    Line "## guarantee 16-byte alignment before call";
    Instruction ("andq", "$0xFFFFFFFFFFFFFFF0", "%rsp", "");
    Instruction ("movl", "$0", "%edi", "");
    Instruction ("call", "exit", "", "");
    Line ".globl l6";
    Line "l6:";
    Instruction ("movq", "%r13", "24(%r15)", "");
    Instruction ("movq", "%r15", "%r13", "");
    Line ".globl String.substr.end";
    Line "String.substr.end:";
    Line "## method body ends";
    Line "## return address handling";
    Instruction ("movq", "%rbp", "%rsp", "");
    Instruction ("popq", "%rbp", "", "");
    Instruction ("ret", "", "", "");
    Line "## ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;";
    Line "## global string constants";
  ]

let intrinsic_funcs =
  [
    in_int;
    out_int;
    in_string;
    out_string;
    cooloutstr;
    coolgetstr;
    coolstrcat;
    coolstrlen;
    coolsubstr;
    abort;
    copy;
    type_name;
    string_length;
    string_substr;
    concat;
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
    (*Instruction (".size", "Bool..new", ".-Bool..new", "");*)
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
    (*Instruction (".size", "IO..new", ".-IO..new", "");*)
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
    (*Instruction (".size", "Int..new", ".-Int..new", "");*)
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

let generate_class_new_asm asm_class_var =
  let func_name = Printf.sprintf "%s..new:" asm_class_var.vtable.name_id in
  let class_tag = Printf.sprintf "$%d" asm_class_var.class_tag in
  let object_size = Printf.sprintf "$%d" (3 + asm_class_var.object_size) in
  let vtable_name = Printf.sprintf "$%s..vtable" asm_class_var.vtable.name_id in
  let stack_room = Printf.sprintf "$%d" 16 in
  let attr_init_lines =
    List.map
      (fun (attr : attribute) : asm_line list ->
        let var_index = 3 + attr.index in
        let type_new = Printf.sprintf "$%s..new" attr.type_name in
        let stack_location = Printf.sprintf "%d(%%rax)" (8 * var_index) in
        [
          Line
            (Printf.sprintf "\t## self[%d] holds field x (%s)" var_index
               attr.type_name);
          Line (Printf.sprintf "\t## new %s" attr.type_name);
          Instruction ("pushq", "%rax", "", "");
          Instruction ("pushq", "%rbp", "", "");
          Instruction ("pushq", "%r12", "", "");
          Instruction ("movq", type_new, "%r14", "");
          Instruction ("call", "*%r14", "", "");
          Instruction ("movq", "%rax", "%r13", "");
          Instruction ("popq", "%r12", "", "");
          Instruction ("popq", "%rbp", "", "");
          Instruction ("pushq", "%rax", "", "");
          Instruction ("movq", "%r13", stack_location, "");
        ])
      asm_class_var.attributes
    |> List.flatten
  in
  let return_lines =
    [
      Instruction ("movq", "%rbp", "%rsp", "");
      Instruction ("popq", "%rbp", "", "");
      Instruction ("ret", "", "", "");
    ]
  in
  ( asm_class_var.vtable.name_id,
    [
      (* Name *)
      Line func_name;
      Line (Printf.sprintf "## constructor for %s" asm_class_var.vtable.name_id);
      (* Set stack pointer (make room for temporaries *)
      Instruction ("pushq", "%rbp", "", "");
      Instruction ("movq", "%rsp", "%rbp", "");
      Line "\t## stack room for temporaries: ?";
      Instruction ("subq", stack_room, "%rsp", "");
      (*  Return address handling *)
      Line "\t## return address handling";
      (* Will be used later for calloc? *)
      Instruction ("movq", object_size, "%rax", "");
      (*  16-byte alignment *)
      Line "\t## guarantee 16-byte alignment before call";
      Instruction ("andq", "$0xFFFFFFFFFFFFFFF0", "%rsp", "");
      (*  Allocate space for the variable (rsi*rdi)=num*bytes *)
      Instruction ("movq", "$8", "%rsi", "");
      Instruction ("movq", "%rax", "%rdi", "");
      Instruction ("call", "calloc", "", "");
      (* Instruction ("movq", "%rax", "%rax", ""); *)
      (* NOTE: Alot of these can be simplified to one line operations *)
      Line "\t## store class tag, object size and vtable pointer";
      Instruction ("movq", class_tag, "0(%rax)", "");
      Instruction ("movq", object_size, "%r14", "");
      Instruction ("movq", "%r14", "8(%rax)", "");
      Instruction ("movq", vtable_name, "%r14", "");
      Instruction ("movq", "%r14", "16(%rax)", "");
      (* Instruction ("movq", "%rax", "%r13", ""); *)
      Line "\t## return address handling";
      (* Reset stack pointer *)
      Line "\t## ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;";
      Line "\t## initialize attributes";
    ]
    @ attr_init_lines @ return_lines )
(*
      Line func_name;
      Instruction ("subq", pointer_size, "%rsp", "");
      Instruction ("movl", object_size, "%esi", "");
      Instruction ("movl", "$8", "%edi", "");
      (* Call glibc calloc function which allocates (esi * dsi) bytes of memory ( count * size) *)
      Instruction ("call", "calloc", "", "");
      Line "\t#Set class tag, object size, vtable pointer";
      Instruction ("movq", class_tag, "(%rax)", "");
      Instruction ("movq", object_size, "8(%rax)", "");
      Instruction ("movq", vtable_name, "16(%rax)", "");
      Instruction ("addq", pointer_size, "%rsp", "");
      Instruction ("ret", "", "", "");
     *)

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
    Instruction ("setl", "%al", "", "");
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
    Instruction ("setle", "%dl", "", "");
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
    Instruction ("sete", "%dl", "", "");
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
  match Hashtbl.find_opt var_locations var_name with
  | None ->
      (* Printf.fprintf out_file "# Adding var %s at position -%d(%%rbp)\n"
        var_name fp_offset; *)
      Hashtbl.add var_locations var_name fp_offset
  | Some _ -> ()

let get_var_addr (var_name : string) : string =
  match Hashtbl.find_opt var_locations var_name with
  | Some addr -> Printf.sprintf "-%d(%%rbp)" addr
  | None -> var_name

let pusha =
  [
    Instruction ("pushq", "%rax", "", "");
    (*Instruction ("pushq", "%rbx", "", "");*)
    (*Instruction ("pushq", "%rbp", "", "");*)
    Instruction ("pushq", "%rdi", "", "");
    Instruction ("pushq", "%rsi", "", "");
    Instruction ("pushq", "%rcx", "", "");
    Instruction ("pushq", "%rdx", "", "");
    Instruction ("pushq", "%r8", "", "");
    Instruction ("pushq", "%r9", "", "");
    Instruction ("pushq", "%r10", "", "");
    Instruction ("pushq", "%r11", "", "");
    Instruction ("pushq", "%r12", "", "");
    (*Instruction ("pushq", "%r13", "", "");*)
    (*Instruction ("pushq", "%r14", "", "");*)
    (*Instruction ("pushq", "%r15", "", "");*)
  ]

let popa =
  [
    (*Instruction ("popq", "%r15", "", "");*)
    (*Instruction ("popq", "%r14", "", "");*)
    (*Instruction ("popq", "%r13", "", "");*)
    Instruction ("popq", "%r12", "", "");
    Instruction ("popq", "%r11", "", "");
    Instruction ("popq", "%r10", "", "");
    Instruction ("popq", "%r9", "", "");
    Instruction ("popq", "%r8", "", "");
    Instruction ("popq", "%rdx", "", "");
    Instruction ("popq", "%rcx", "", "");
    Instruction ("popq", "%rsi", "", "");
    Instruction ("popq", "%rdi", "", "");
    (*Instruction ("popq", "%rbp", "", "");*)
    (*Instruction ("popq", "%rbx", "", "");*)
    Instruction ("popq", "%rax", "", "");
  ]

let jump_number = ref 1

let get_jump () =
  jump_number := !jump_number + 1;
  !jump_number

(** Method to convert a TAC element to assembly code *)
let tac_to_as (tac : tac_elem) cur_method class_name =
  [ Line (Tac.get_tac_elem_commented tac) ]
  @
  match tac.operand with
  | Assignment ->
      let result = get_var_addr tac.result in
      let arg1 = get_var_addr tac.arg1 in
      [
        Line "\t#Assignment start";
        Instruction ("movq", arg1, "%rax", "");
        Instruction ("movq", "%rax", result, "");
        Line "\t#Assignment end";
      ]
  | Bt ->
      let arg1 = get_var_addr tac.arg1 in
      [
        Line "\t#Branch True start";
        Instruction ("movq", arg1, "%rax", "");
        Instruction ("movq", "24(%rax)", "%rax", "");
        Instruction ("testq", "%rax", "%rax", "");
        Instruction ("jne", tac.arg2, "", "");
        Line "\t#Branch True end";
      ]
  | Call ->
      add_var_addr tac.result;
      let result = get_var_addr tac.result in
      if tac.arg2 = "" then
        [ Line "\t#Call w/ args start" ]
        @ [
            Instruction ("call", "IO." ^ tac.arg1, "", "");
            Instruction ("movq", "%rax", result, "");
          ]
        @ [ Line "\t#Call w/ args end" ]
      else
        let arglist =
          [ Instruction ("movq", get_var_addr tac.arg2, "%rsi", "") ]
        in
        [ Line "\t#Call w/ args start" ]
        @ arglist
        @ [
            Instruction ("call", "IO." ^ tac.arg1, "", "");
            Instruction ("movq", "%rax", result, "");
          ]
        @ [ Line "\t#Call w/ args end" ]
  | ClassId -> (
      add_var_addr tac.result;
      let result = get_var_addr tac.result in
      let class_name = get_var_addr tac.arg1 in
      let class_tag = Hashtbl.find_opt class_id_map class_name in
      match class_tag with
      | Some class_tag ->
          [ Instruction ("movq", Printf.sprintf "$%d" class_tag, result, "") ]
      | None ->
          [
            Instruction ("movq", class_name, "%r13", "");
            Instruction ("movq", "%r13", result, "");
          ])
  | Comment ->
      [ Line "\t#Comment start" ]
      @ [ Line ("#" ^ tac.arg1) ]
      @ [ Line "\t#Comment end" ]
  | Label -> [ Line "\t#Label" ] @ [ Line (Printf.sprintf "%s:" tac.arg1) ]
  | Jmp -> [ Line "\t#Jump" ] @ [ Instruction ("jmp", tac.arg1, "", "") ]
  | Return ->
      [
        Line "\t#Return start";
        Instruction ("jmp", "." ^ cur_method ^ ".end", "", "");
        Line "\t#Return end";
      ]
  | LetNoInit ->
      add_var_addr tac.result;
      let result = get_var_addr tac.result in
      [
        (*Instruction ("movq", "$0", result, "");*)
        Line "\t#Let No Init start";
        Instruction ("pushq", "%rbp", "", "");
        Instruction ("pushq", "%rax", "", "");
        Instruction ("call", tac.arg2 ^ "..new", "", "");
        Instruction ("movq", "%rax", "%r10", "");
        Instruction ("popq", "%rax", "", "");
        Instruction ("popq", "%rbp", "", "");
        Instruction ("movq", "%r10", result, "");
        Line "\t#Let No Init end";
      ]
  | Ident_Expr ident_name ->
      add_var_addr tac.result;
      let result = get_var_addr tac.result in
      let val_addr =
        match Hashtbl.find_opt var_locations ident_name with
        | Some addr -> Printf.sprintf "-%d(%%rbp)" addr
        | None -> (
            match
              List.find_opt
                (fun attr -> attr.field_name = ident_name)
                (Hashtbl.find class_attribute_map class_name)
            with
            | Some v -> Printf.sprintf "Attr Index: %d" ((v.index + 3) * 8)
            | None -> assert false)
      in
      [
        Line "\t#Ident Expr start";
        Instruction ("movq", val_addr, "%rax", "");
        Instruction ("movq", "%rax", result, "");
        Line "\t#Ident Expr end";
      ]
  | Plus ->
      let arg1 = get_var_addr tac.arg1 in
      let arg2 = get_var_addr tac.arg2 in
      add_var_addr tac.result;
      let result = get_var_addr tac.result in

      [
        Line "\t#Plus start";
        Instruction ("movq", arg1, "%rax", "");
        Instruction ("movq", "24(%rax)", "%rax", "");
        Instruction ("movq", arg2, "%rdx", "");
        Instruction ("movq", "24(%rdx)", "%rdx", "");
        Instruction ("addl", "%edx", "%eax", "");
        Instruction ("pushq", "%rbp", "", "");
        Instruction ("pushq", "%rax", "", "");
        Instruction ("call", "Int..new", "", "");
        Instruction ("movq", "%rax", "%r10", "");
        Instruction ("popq", "%rax", "", "");
        Instruction ("popq", "%rbp", "", "");
        Instruction ("movq", "%rax", "24(%r10)", "");
        Instruction ("movq", "%r10", result, "");
        Line "\t#Plus end";
      ]
  | Minus ->
      let arg1 = get_var_addr tac.arg1 in
      let arg2 = get_var_addr tac.arg2 in
      add_var_addr tac.result;
      let result = get_var_addr tac.result in

      [
        Line "\t#Minus start";
        Instruction ("movq", arg1, "%rax", "");
        Instruction ("movq", "24(%rax)", "%rax", "");
        Instruction ("movq", arg2, "%rdx", "");
        Instruction ("movq", "24(%rdx)", "%rdx", "");
        Instruction ("subl", "%edx", "%eax", "");
        Instruction ("pushq", "%rbp", "", "");
        Instruction ("pushq", "%rax", "", "");
        Instruction ("call", "Int..new", "", "");
        Instruction ("movq", "%rax", "%r10", "");
        Instruction ("popq", "%rax", "", "");
        Instruction ("popq", "%rbp", "", "");
        Instruction ("movq", "%rax", "24(%r10)", "");
        Instruction ("movq", "%r10", result, "");
        Line "\t#Minus end";
      ]
  | Divide ->
      let arg1 = get_var_addr tac.arg1 in
      let arg2 = get_var_addr tac.arg2 in
      add_var_addr tac.result;
      let result = get_var_addr tac.result in

      [
        Line "\t#Divide start";
        Instruction ("movq", arg2, "%rcx", "");
        Instruction ("movq", "24(%rcx)", "%rcx", "");
        Instruction ("movq", arg1, "%rax", "");
        Instruction ("movq", "24(%rax)", "%rax", "");
        Instruction ("cltd", "", "", "");
        Instruction ("idivl", "%ecx", "", "");
        Instruction ("pushq", "%rbp", "", "");
        Instruction ("pushq", "%rax", "", "");
        Instruction ("call", "Int..new", "", "");
        Instruction ("movq", "%rax", "%r10", "");
        Instruction ("popq", "%rax", "", "");
        Instruction ("popq", "%rbp", "", "");
        Instruction ("movq", "%rax", "24(%r10)", "");
        Instruction ("movq", "%r10", result, "");
        Line "\t#Divide end";
      ]
  | Times ->
      let arg1 = get_var_addr tac.arg1 in
      let arg2 = get_var_addr tac.arg2 in
      add_var_addr tac.result;
      let result = get_var_addr tac.result in
      [
        Line "\t#Times start";
        Instruction ("movq", arg1, "%rax", "");
        Instruction ("movq", "24(%rax)", "%rax", "");
        Instruction ("movq", arg2, "%rdx", "");
        Instruction ("movq", "24(%rdx)", "%rdx", "");
        Instruction ("imull", "%edx", "%eax", "");
        Instruction ("pushq", "%rbp", "", "");
        Instruction ("pushq", "%rax", "", "");
        Instruction ("call", "Int..new", "", "");
        Instruction ("movq", "%rax", "%r10", "");
        Instruction ("popq", "%rax", "", "");
        Instruction ("popq", "%rbp", "", "");
        Instruction ("movq", "%rax", "24(%r10)", "");
        Instruction ("movq", "%r10", result, "");
        Line "\t#Times end";
      ]
  | LessThan ->
      let arg1 = get_var_addr tac.arg1 in
      let arg2 = get_var_addr tac.arg2 in
      add_var_addr tac.result;
      let result = get_var_addr tac.result in

      [
        Line "\t#Less Than start";
        Instruction ("pushq", "%rdi", "", "");
        Instruction ("pushq", "%rsi", "", "");
        Instruction ("movq", arg1, "%rdi", "");
        Instruction ("movq", arg2, "%rsi", "");
        Instruction ("call", "lt_handler", "", "");
        Instruction ("popq", "%rsi", "", "");
        Instruction ("popq", "%rdi", "", "");
        Instruction ("movq", "%rax", result, "");
        Line "\t#Less Than end";
      ]
  | LessEqual ->
      let arg1 = get_var_addr tac.arg1 in
      let arg2 = get_var_addr tac.arg2 in
      add_var_addr tac.result;
      let result = get_var_addr tac.result in

      [
        Line "\t#Less Equal start";
        Instruction ("pushq", "%rdi", "", "");
        Instruction ("pushq", "%rsi", "", "");
        Instruction ("movq", arg1, "%rdi", "");
        Instruction ("movq", arg2, "%rsi", "");
        Instruction ("call", "le_handler", "", "");
        Instruction ("popq", "%rsi", "", "");
        Instruction ("popq", "%rdi", "", "");
        Instruction ("movq", "%rax", result, "");
        Line "\t#Less Equal end";
      ]
  | Equal ->
      let arg1 = get_var_addr tac.arg1 in
      let arg2 = get_var_addr tac.arg2 in
      add_var_addr tac.result;
      let result = get_var_addr tac.result in

      [
        Line "\t#Equal start";
        Instruction ("pushq", "%rdi", "", "");
        Instruction ("pushq", "%rsi", "", "");
        Instruction ("movq", arg1, "%rdi", "");
        Instruction ("movq", arg2, "%rsi", "");
        Instruction ("call", "eq_handler", "", "");
        Instruction ("popq", "%rsi", "", "");
        Instruction ("popq", "%rdi", "", "");
        Instruction ("movq", "%rax", result, "");
        Line "\t#Equal end";
      ]
  | Not ->
      add_var_addr tac.arg1;
      let arg1 = get_var_addr tac.arg1 in
      add_var_addr tac.result;
      let result = get_var_addr tac.result in

      [
        Line "\t#Not start";
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
        Line "\t#Not end";
      ]
  | Negate ->
      let arg1 = get_var_addr tac.arg1 in
      add_var_addr tac.result;
      let result = get_var_addr tac.result in

      [
        Line "\t#Negate start";
        Instruction ("movq", arg1, "%rax", "");
        Instruction ("movq", "24(%rax)", "%rax", "");
        Instruction ("negq", "%rax", "", "");
        Instruction ("pushq", "%rbp", "", "");
        Instruction ("pushq", "%rax", "", "");
        Instruction ("call", "Int..new", "", "");
        Instruction ("movq", "%rax", "%r10", "");
        Instruction ("popq", "%rax", "", "");
        Instruction ("popq", "%rbp", "", "");
        Instruction ("movq", "%rax", "24(%r10)", "");
        Instruction ("movq", "%r10", result, "");
        Line "\t#Negate end";
      ]
  | Int_Constant ->
      add_var_addr tac.result;
      let result = get_var_addr tac.result in
      [
        Line "\t#iconst start";
        Instruction ("call", "Int..new", "", "");
        Instruction ("movq", "$" ^ tac.arg1, "24(%rax)", "");
        Instruction ("movq", "%rax", result, "");
        Line "\t#iconst end";
      ]
  | String_Constant -> (
      add_var_addr tac.result;
      let result = get_var_addr tac.result in
      match Hashtbl.find_opt string_map tac.arg1 with
      | Some str_id ->
          [
            Line "\t#sconst start";
            Instruction ("call", "String..new", "", "");
            Instruction
              ("movq", "$string" ^ string_of_int str_id, "24(%rax)", "");
            Instruction ("movq", "%rax", result, "");
            Line "\t#sconst end";
            (*Instruction ("movq", "$.string" ^ string_of_int str_id, result, "");*)
          ]
      | None ->
          string_counter := !string_counter + 1;
          Hashtbl.add string_map tac.arg1 !string_counter;

          [
            Line "\t#sconst start";
            Instruction ("call", "String..new", "", "");
            Instruction
              ("movq", "$string" ^ string_of_int !string_counter, "24(%rax)", "");
            Instruction ("movq", "%rax", result, "");
            Line "\t#sconst end";
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
          Line "\t#bconst start";
          Instruction ("call", "Bool..new", "", "");
          Instruction ("movq", "$1", "24(%rax)", "");
          Instruction ("movq", "%rax", result, "");
          Line "\t#bconst end";
        ]
      else
        [
          Line "\t#bconst start";
          Instruction ("call", "Bool..new", "", "");
          Instruction ("movq", "$0", "24(%rax)", "");
          Instruction ("movq", "%rax", result, "");
          Line "\t#bconst end";
        ]
  | Case -> assert false
  (* NOTE: I'm assuming default works the same as new based on our implementation of the default class new statements. This also assumes we dont use the default keyword for any user-defined classes *)
  | Default | New ->
      add_var_addr tac.result;
      let result = get_var_addr tac.result in
      Printf.fprintf out_file "# NEW: Adding var %s at position %s\n" tac.result
        result;
      [
        Instruction ("pushq", "%rbp", "", "");
        Instruction ("pushq", "%r12", "", "");
        Instruction ("movq", Printf.sprintf "$%s..new" tac.arg1, "%r14", "");
        Instruction ("call", "*%r14", "", "");
        Instruction ("movq", "%rax", Printf.sprintf "%s" result, "");
        Instruction ("popq", "%r12", "", "");
        Instruction ("popq", "%rbp", "", "");
      ]
  | Isvoid ->
      add_var_addr tac.result;
      let result = get_var_addr tac.result in
      Printf.fprintf out_file "# Isvoid: Adding var %s at position %s\n"
        tac.result result;
      let true_jump = get_jump () in
      let post_jump = get_jump () in
      let post_jump_label =
        Printf.sprintf ".globl l%d\nl%d:" post_jump post_jump
      in
      let true_jump_label =
        Printf.sprintf ".globl l%d\nl%d:" true_jump true_jump
      in
      (* Note: Is void code is the same minus the last few lines so we can optimize instruction count by combining these branches *)
      [
        Instruction ("cmpq", "$0", "%rax", "");
        Instruction ("je", Printf.sprintf "l%d" true_jump, "", "");
        Line "\t #false branch of isvoid";
        Instruction ("pushq", "%rbp", "", "");
        Instruction ("pushq", "%r12", "", "");
        Instruction ("movq", "$Bool..new", "%r14", "");
        Instruction ("call", "*%r14", "", "");
        Instruction ("popq", "%r12", "", "");
        Instruction ("popq", "%rbp", "", "");
        Instruction ("movq", "%rax", Printf.sprintf "%s" result, "");
        Instruction ("jmp", Printf.sprintf "l%d" post_jump, "", "");
        Line true_jump_label;
        Line "\t #true branch of isvoid";
        Instruction ("pushq", "%rbp", "", "");
        Instruction ("pushq", "%r12", "", "");
        Instruction ("movq", "$Bool..new", "%r14", "");
        Instruction ("call", "*%r14", "", "");
        Instruction ("popq", "%r12", "", "");
        Instruction ("popq", "%rbp", "", "");
        (* This works bc the bool created a few lines ago is stored in %r13, 24(%r13) is the third field of the bool (initially zero) *)
        Instruction ("movq", "$1", "24(%rax)", "");
        Instruction ("movq", "%rax", Printf.sprintf "%s" result, "");
        Instruction ("jmp", Printf.sprintf "l%d" post_jump, "", "");
        Line post_jump_label;
      ]

let get_start_method_boilerplate method_name class_name stack_space =
  let name = class_name ^ "." ^ method_name in
  [
    Line "\t.p2align 4";
    Line (Printf.sprintf "\t.globl\t%s" name);
    Line (Printf.sprintf "\t.type\t%s, @function" name);
    Line (Printf.sprintf "%s:" name);
    Instruction ("pushq", "%rbp", "", "");
    Instruction ("movq", "%rsp", "%rbp", "");
    Instruction ("subq", "$" ^ string_of_int stack_space, "%rsp", "");
  ]

let get_end_method_boilerplate method_name stack_space =
  [
    Line (Printf.sprintf ".%s.end:" method_name);
    Instruction ("addq", "$" ^ string_of_int stack_space, "%rsp", "");
    Instruction ("popq", "%rbp", "", "");
    Instruction ("ret", "", "", "");
  ]

let vtables =
  create_vtables ();
  create_default_vtables () @ !vtable_list

let asm_classes : asm_class list =
  List.filter_map make_asm_class parser_class_map

let new_funcs = new_funcs @ List.map generate_class_new_asm asm_classes

(* A list of (the assembly code for) methods *)
let method_asm =
  List.map
    (fun (cfg, class_name, method_name, temps) ->
      (* Printf.fprintf stdout "Method %s of Class %s uses %d temps\n"
          method_name class_name temps; *)
      (*let name = class_name ^ "." ^ method_name in*)
      let stack_space =
        if temps * 8 mod 16 != 0 then (temps + 1) * 8 else temps * 8
      in
      let method_tac = cfg |> List.flatten in
      get_start_method_boilerplate method_name class_name stack_space
      @ (List.map (fun tac -> tac_to_as tac method_name class_name) method_tac
        |> List.flatten)
        (* @ [Asm.Line (Printf.sprintf "\t.size\t%s, .-%s" name name)]*)
      @ get_end_method_boilerplate method_name stack_space)
    Cfg.cfg_list

let tac_list_to_asm lst = List.map tac_to_as lst
