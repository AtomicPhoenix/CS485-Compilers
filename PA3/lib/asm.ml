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
and vtable_func = {type_name : string; method_name : string}
and vtable = { name_id : string; methods : vtable_func list }

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

let var_locations = Hashtbl.create 32
let string_map = Hashtbl.create 32
let string_map = Hashtbl.create 32
let class_id_map = Hashtbl.create 32
let class_vtable_map = Hashtbl.create 32
let string_counter = ref 0
let class_tag_ctr = ref 0

let class_map = Parser.parse_class_map ()
let implementation_map = Parser.parse_implementation_map ()
let parent_map = Parser.parse_parent_map ()

let create_vtable itm : vtable = 
    let name = itm.name in
    let funcs = List.map (fun (meth : imp_method) -> {type_name = meth.type_name; method_name = meth.name} ) itm.methods in
    {name_id = name; methods = funcs}


    
let create_vtables () = 
    let tables = List.map create_vtable implementation_map in
    List.iter (fun i -> Hashtbl.add class_vtable_map i.name_id i) tables

let get_class_attributes attrs =
    let get_attribute i (attr : ast_attribute) =
        let name = attr.name in
        let index = i in
        let typename = attr.type_name in
        let expr = attr.attr_expr in
        {field_name = name; index = index; type_name = typename; expression = expr}
    in
    List.mapi get_attribute attrs

        

let make_asm_class (c:class_map_elem)=
    let tag = !class_tag_ctr in
    class_tag_ctr := !class_tag_ctr + 1;
    Hashtbl.add class_id_map c.name tag;
    (* 8 bytes/64 bits since every attribute is a pointer *)
    let siz = (List.length c.attrs) in
    let class_vtable = Hashtbl.find class_vtable_map c.name in
    let attrs = get_class_attributes c.attrs in
    {class_tag = tag; object_size = siz; vtable = class_vtable; attributes = attrs}
    
(*TODO: make this make and return an int object*)
(*let in_int =*)
(*"\n\*)
(*IO.in_int:\n\*)
(*\tpushq\t%rbx\n\*)
(*\tmovl\t$4096, %esi\n\*)
(*\tsubq\t$4112, %rsp\n\*)
(*\tmovq\tstdin(%rip), %rdx\n\*)
(*\tleaq\t16(%rsp), %rbx\n\*)
(*\tmovq\t%rbx, %rdi\n\*)
(*\tcall\tfgets\n\*)
(*\tleaq\t8(%rsp), %rdx\n\*)
(*\tmovq\t%rbx, %rdi\n\*)
(*\txorl\t%eax, %eax\n\*)
(*\tmovq\t$percent.ld, %rsi\n\*)
(*\tcall\tsscanf\n\*)
(*\tmovq\t8(%rsp), %rax\n\*)
(*\tmovl\t$2147483648, %edx\n\*)
(*\tmovl\t$4294967295, %ecx\n\*)
(*\taddq\t%rax, %rdx\n\*)
(*\tcmpq\t%rdx, %rcx\n\*)
(*\tmovl\t$0, %edx\n\*)
(*\tcmovb\t%rdx, %rax\n\*)
(*\taddq\t$4112, %rsp\n\*)
(*\tpopq\t%rbx\n\*)
(*\tret\n\*)
(*\t.size\tin_int, .-in_int\n"*)

(*let out_int =*)
(*"\n\*)
(*IO.out_int:*)
(*\tmovq\t24(%rsi), %rsi\n\*)
(*\tmovl\t$percent.d, %rdi\n\*)
(*\txorl\t%eax, %eax\n\*)
(*\tjmp\tprintf\n\*)
(*\t.size\tIO.out_int, .-IO.out_int*)
(*"*)
let in_int = [
    Line ("IO.in_int:");
    Instruction("pushq", "%rbp", "", "");
    Instruction("pushq", "%rbx", "", "");
    Instruction("subq", "%4120", "%rsp", "");
    Instruction("call", "Int..new", "", "");
    Instruction("leaq", "16(%rsp)", "%rbp", "");
    Instruction("movl", "$4096", "%esi", "");
    Instruction("movq", "stdin(%rip)", "%rdx", "");
    Instruction("movq", "%rbp", "%rdi", "");
    Instruction("movq", "%rax", "%rbx", "");
    Instruction("call", "fgets", "", "");
    Instruction("leaq", "8(%rsp)", "%rdx", "");
    Instruction("movq", "%rbp", "%rdi", "");
    Instruction("xorl", "%eax", "%eax", "");
    Instruction("movq", "$percent.ld", "%rsi", "");
    Instruction("call", "sscanf", "", "");
    Instruction("movq", "8(%rsp)", "%rax", "");
    Instruction("movl", "$2147483648", "%edx", "");
    Instruction("movl", "$4294967295", "%ecx", "");
    Instruction("addq", "%rax", "%rdx", "");
    Instruction("cmpq", "%rdx", "%rcx", "");
    Instruction("movl", "$0", "%edx", "");
    Instruction("cmovb", "%rdx", "%rax", "");
    Instruction("movq", "%rax", "24(%rbx)", "");
    Instruction("addq", "$4120", "%rsp", "");
    Instruction("movq", "%rbx", "%rax", "");
    Instruction("popq", "%rbx", "", "");
    Instruction("popq", "%rbp", "", "");
    Instruction("ret", "", "", "");
    Instruction(".size", "IO.in_int", ".-IO.in_int", "");
]
let out_int = [
    Line("IO.out_int:");
    Instruction("movq", "24(%rsi)", "%rsi", "");
    Instruction("movl", "$percent.d", "%rdi", "");
    Instruction("xorl", "%eax", "%eax", "");
    Instruction("jmp", "printf", "", "");
    Instruction(".size", "IO.out_int", ".-IO.out_int", "");
]

(* Bool is class tag 0 *)
let () = Hashtbl.add class_id_map "Bool" 0
let bool_new = [
    Line("Bool..new:");
    Instruction("subq", "$8", "%rsp", "");
    Instruction("movl", "$4", "%esi", "");
    Instruction("movl", "$8", "%edi", "");
    Instruction("call", "calloc", "", "");
    Line("\t#Set class tag, object size, vtable pointer");
    Instruction("movq", "$0", "(%rax)", "");
    Instruction("movq", "$4", "8(%rax)", "");
    Instruction("movq", "$Bool..vtable", "%r10", "");
    Instruction("movq", "%r10", "16(%rax)", "");
    Instruction("movq", "$0", "24(%rax)", "");
    Instruction("addq", "$8", "%rsp", "");
    Instruction("ret", "", "", "");
    Instruction(".size", "Bool..new", ".-Bool..new", "");
]
let () = Hashtbl.add class_id_map "IO" 1
let io_new = [
    Line("Int..new:");
    Instruction("subq", "$8", "%rsp", "");
    Instruction("movl", "$3", "%esi", "");
    Instruction("movl", "$8", "%edi", "");
    Instruction("call", "calloc", "", "");
    Line("\t#Set class tag, object size, vtable pointer");
    Instruction("movq", "$1", "(%rax)", "");
    Instruction("movq", "$3", "8(%rax)", "");
    Instruction("movq", "$IO..vtable", "%r10", "");
    Instruction("movq", "%r10", "16(%rax)", "");
    Instruction("addq", "$8", "%rsp", "");
    Instruction("ret", "", "", "");
    Instruction(".size", "IO..new", ".-IO..new", "");
]
let () = Hashtbl.add class_id_map "Int" 2
let int_new = [
    Line("Int..new:");
    Instruction("subq", "$8", "%rsp", "");
    Instruction("movl", "$4", "%esi", "");
    Instruction("movl", "$8", "%edi", "");
    Instruction("call", "calloc", "", "");
    Line("\t#Set class tag, object size, vtable pointer");
    Instruction("movq", "$2", "(%rax)", "");
    Instruction("movq", "$4", "8(%rax)", "");
    Instruction("movq", "$Int..vtable", "%r10", "");
    Instruction("movq", "%r10", "16(%rax)", "");
    Instruction("movq", "$0", "24(%rax)", "");
    Instruction("addq", "$8", "%rsp", "");
    Instruction("ret", "", "", "");
    Instruction(".size", "IO..new", ".-IO..new", "");
]
let () = Hashtbl.add class_id_map "Object" 3
let object_new = [
    Line("Int..new:");
    Instruction("subq", "$8", "%rsp", "");
    Instruction("movl", "$3", "%esi", "");
    Instruction("movl", "$8", "%edi", "");
    Instruction("call", "calloc", "", "");
    Line("\t#Set class tag, object size, vtable pointer");
    Instruction("movq", "$3", "(%rax)", "");
    Instruction("movq", "$3", "8(%rax)", "");
    Instruction("movq", "$Object..vtable", "%r10", "");
    Instruction("movq", "%r10", "16(%rax)", "");
    Instruction("addq", "$8", "%rsp", "");
    Instruction("ret", "", "", "");
    Instruction(".size", "Object..new", ".-Object..new", "");
]
let () = Hashtbl.add class_id_map "String" 4
let string_new = [
    Line("Int..new:");
    Instruction("subq", "$8", "%rsp", "");
    Instruction("movl", "$4", "%esi", "");
    Instruction("movl", "$8", "%edi", "");
    Instruction("call", "calloc", "", "");
    Line("\t#Set class tag, object size, vtable pointer");
    Instruction("movq", "$4", "(%rax)", "");
    Instruction("movq", "$4", "8(%rax)", "");
    Instruction("movq", "$String..vtable", "%r10", "");
    Instruction("movq", "%r10", "16(%rax)", "");
    Instruction("movq", "$empty.string", "%r10", "");
    Instruction("movq", "%r10", "24(%rax)", "");
    Instruction("addq", "$8", "%rsp", "");
    Instruction("ret", "", "", "");
    Instruction(".size", "String..new", ".-String..new", "");
]
let handlers = [
  Line("lt_handler:");
  Instruction("pushq", "%r12", "", "");
  Instruction("movq", "%rsi", "%r12", "");
  Instruction("pushq", "%rbp", "", "");
  Instruction("pushq", "%rbx", "", "");
  Instruction("movq", "%rdi", "%rbx", "");
  Instruction("call", "new_bool", "", "");
  Instruction("movq", "%rax", "%rbp", "");
  Instruction("testq", "%rbx", "%rbx", "");
  Instruction("je", ".L7", "", "");
  Instruction("testq", "%r12", "%r12", "");
  Instruction("je", ".L7", "", "");
  Instruction("movq", "(%r12)", "%rdx", "");
  Instruction("addq", "(%rbx)", "%rdx", "");
  Instruction("testq", "$-3", "%rdx", "");
  Instruction("je", ".L6", "", "");
  Instruction("xorl", "%eax", "%eax", "");
  Instruction("cmpq", "$6", "%rdx", "");
  Instruction("je", ".L10", "", "");
  Line(".L5:");
  Instruction("movq", "%rax", "24(%rbp)", "");
  Instruction("movq", "%rbp", "%rax", "");
  Instruction("popq", "%rbx", "", "");
  Instruction("popq", "%rbp", "", "");
  Instruction("popq", "%r12", "", "");
  Instruction("ret", "", "", "");
  Instruction("\t.p2align 4,,10", "", "", "");
  Instruction("\t.p2align 3", "", "", "");
  Line(".L6:");
  Instruction("movq", "24(%r12)", "%rax", "");
  Instruction("cmpq", "%rax", "24(%rbx)", "");
  Instruction("setge", "%al", "", "");
  Instruction("movzbl", "%al", "%eax", "");
  Instruction("movq", "%rax", "24(%rbp)", "");
  Instruction("movq", "%rbp", "%rax", "");
  Instruction("popq", "%rbx", "", "");
  Instruction("popq", "%rbp", "", "");
  Instruction("popq", "%r12", "", "");
  Instruction("ret", "", "", "");
  Instruction("\t.p2align 4,,10", "", "", "");
  Instruction("\t.p2align 3", "", "", "");
  Line(".L7:");
  Instruction("xorl", "%eax", "%eax", "");
  Instruction("movq", "%rax", "24(%rbp)", "");
  Instruction("movq", "%rbp", "%rax", "");
  Instruction("popq", "%rbx", "", "");
  Instruction("popq", "%rbp", "", "");
  Instruction("popq", "%r12", "", "");
  Instruction("ret", "", "", "");
  Instruction("\t.p2align 4,,10", "", "", "");
  Instruction("\t.p2align 3", "", "", "");
  Line(".L10:");
  Instruction("movq", "24(%r12)", "%rsi", "");
  Instruction("movq", "24(%rbx)", "%rdi", "");
  Instruction("call", "strcmp", "", "");
  Instruction("shrl", "$31", "%eax", "");
  Instruction("jmp", ".L5", "", "");
  Instruction("\t.size", "lt_handler", ".-lt_handler", "");
  Instruction("\t.p2align 4", "", "", "");
  Instruction("\t.globl", "le_handler", "", "");
  Instruction("\t.type", "le_handler", "@function", "");
  Line("le_handler:");
  Instruction("pushq", "%r12", "", "");
  Instruction("pushq", "%rbp", "", "");
  Instruction("movq", "%rsi", "%rbp", "");
  Instruction("pushq", "%rbx", "", "");
  Instruction("movq", "%rdi", "%rbx", "");
  Instruction("call", "new_bool", "", "");
  Instruction("movq", "%rax", "%r12", "");
  Instruction("testq", "%rbx", "%rbx", "");
  Instruction("je", ".L15", "", "");
  Instruction("testq", "%rbp", "%rbp", "");
  Instruction("je", ".L15", "", "");
  Instruction("movq", "0(%rbp)", "%rax", "");
  Instruction("addq", "(%rbx)", "%rax", "");
  Instruction("testq", "$-3", "%rax", "");
  Instruction("je", ".L13", "", "");
  Instruction("xorl", "%edx", "%edx", "");
  Instruction("cmpq", "%rbp", "%rbx", "");
  Instruction("sete", "%dl", "", "");
  Instruction("cmpq", "$6", "%rax", "");
  Instruction("je", ".L17", "", "");
  Line(".L12:");
  Instruction("movq", "%rdx", "24(%r12)", "");
  Instruction("movq", "%r12", "%rax", "");
  Instruction("popq", "%rbx", "", "");
  Instruction("popq", "%rbp", "", "");
  Instruction("popq", "%r12", "", "");
  Instruction("ret", "", "", "");
  Instruction("\t.p2align 4,,10", "", "", "");
  Instruction("\t.p2align 3", "", "", "");
  Line(".L13:");
  Instruction("movq", "24(%rbp)", "%rax", "");
  Instruction("xorl", "%edx", "%edx", "");
  Instruction("cmpq", "%rax", "24(%rbx)", "");
  Instruction("movq", "%r12", "%rax", "");
  Instruction("setg", "%dl", "", "");
  Instruction("movq", "%rdx", "24(%r12)", "");
  Instruction("popq", "%rbx", "", "");
  Instruction("popq", "%rbp", "", "");
  Instruction("popq", "%r12", "", "");
  Instruction("ret", "", "", "");
  Instruction("\t.p2align 4,,10", "", "", "");
  Instruction("\t.p2align 3", "", "", "");
  Line(".L15:");
  Instruction("xorl", "%edx", "%edx", "");
  Instruction("movq", "%r12", "%rax", "");
  Instruction("movq", "%rdx", "24(%r12)", "");
  Instruction("popq", "%rbx", "", "");
  Instruction("popq", "%rbp", "", "");
  Instruction("popq", "%r12", "", "");
  Instruction("ret", "", "", "");
  Instruction("\t.p2align 4,,10", "", "", "");
  Instruction("\t.p2align 3", "", "", "");
  Line(".L17:");
  Instruction("movq", "24(%rbp)", "%rsi", "");
  Instruction("movq", "24(%rbx)", "%rdi", "");
  Instruction("call", "strcmp", "", "");
  Instruction("xorl", "%edx", "%edx", "");
  Instruction("testl", "%eax", "%eax", "");
  Instruction("setle", "%dl", "", "");
  Instruction("jmp", ".L12", "", "");
  Instruction("\t.size", "le_handler", ".-le_handler", "");
  Instruction("\t.p2align 4", "", "", "");
  Instruction("\t.globl", "eq_handler", "", "");
  Instruction("\t.type", "eq_handler", "@function", "");
  Line("eq_handler:");
  Instruction("pushq", "%r12", "", "");
  Instruction("pushq", "%rbp", "", "");
  Instruction("movq", "%rsi", "%rbp", "");
  Instruction("pushq", "%rbx", "", "");
  Instruction("movq", "%rdi", "%rbx", "");
  Instruction("call", "new_bool", "", "");
  Instruction("movq", "%rax", "%r12", "");
  Instruction("testq", "%rbx", "%rbx", "");
  Instruction("je", ".L22", "", "");
  Instruction("testq", "%rbp", "%rbp", "");
  Instruction("je", ".L22", "", "");
  Instruction("movq", "0(%rbp)", "%rax", "");
  Instruction("addq", "(%rbx)", "%rax", "");
  Instruction("testq", "$-3", "%rax", "");
  Instruction("je", ".L20", "", "");
  Instruction("xorl", "%edx", "%edx", "");
  Instruction("cmpq", "%rbp", "%rbx", "");
  Instruction("sete", "%dl", "", "");
  Instruction("cmpq", "$6", "%rax", "");
  Instruction("je", ".L24", "", "");
  Line(".L19:");
  Instruction("movq", "%rdx", "24(%r12)", "");
  Instruction("movq", "%r12", "%rax", "");
  Instruction("popq", "%rbx", "", "");
  Instruction("popq", "%rbp", "", "");
  Instruction("popq", "%r12", "", "");
  Instruction("ret", "", "", "");
  Instruction("\t.p2align 4,,10", "", "", "");
  Instruction("\t.p2align 3", "", "", "");
  Line(".L20:");
  Instruction("movq", "24(%rbp)", "%rax", "");
  Instruction("xorl", "%edx", "%edx", "");
  Instruction("cmpq", "%rax", "24(%rbx)", "");
  Instruction("movq", "%r12", "%rax", "");
  Instruction("setne", "%dl", "", "");
  Instruction("movq", "%rdx", "24(%r12)", "");
  Instruction("popq", "%rbx", "", "");
  Instruction("popq", "%rbp", "", "");
  Instruction("popq", "%r12", "", "");
  Instruction("ret", "", "", "");
  Instruction("\t.p2align 4,,10", "", "", "");
  Instruction("\t.p2align 3", "", "", "");
  Line(".L22:");
  Instruction("xorl", "%edx", "%edx", "");
  Instruction("movq", "%r12", "%rax", "");
  Instruction("movq", "%rdx", "24(%r12)", "");
  Instruction("popq", "%rbx", "", "");
  Instruction("popq", "%rbp", "", "");
  Instruction("popq", "%r12", "", "");
  Instruction("ret", "", "", "");
  Instruction("\t.p2align 4,,10", "", "", "");
  Instruction("\t.p2align 3", "", "", "");
  Line(".L24:");
  Instruction("movq", "24(%rbp)", "%rsi", "");
  Instruction("movq", "24(%rbx)", "%rdi", "");
  Instruction("call", "strcmp", "", "");
  Instruction("xorl", "%edx", "%edx", "");
  Instruction("testl", "%eax", "%eax", "");
  Instruction("sete", "%dl", "", "");
  Instruction("jmp", ".L19", "", "");
  Instruction("\t.size", "eq_handler", ".-eq_handler", "");
]
let add_var_addr (var_name : string) =
  let fp_offset = 8 * Hashtbl.length var_locations in
  Hashtbl.add var_locations var_name fp_offset

let get_var_addr (var_name : string) : string =
  match Hashtbl.find_opt var_locations var_name with
  | Some addr -> Printf.sprintf "%d(%%rbp)" addr
  | None ->
      Printf.fprintf out_file "; Is this supposed to happen?\n";
      var_name


(** Method to convert a TAC element to assembly code *)
let tac_to_as (tac : tac_elem) elems =
  match tac.operand with
  (****************** TODO ******************)
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
  (****************** TODO ******************)
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
  (****************** TODO ******************)
  | Return ->
      elems
      @ [
          Instruction ("movq", "%rbp", "%rsp", "");
          Instruction ("popq", "%rbp", "%rsp", "");
          Instruction ("ret", "", "", "");
        ]
  (****************** TODO ******************)
  | LetNoInit ->
      add_var_addr tac.result;
      let result = get_var_addr tac.result in
      elems @ [
        Instruction ("movq", "$0", result, "");
      ]
    
  (****************** TODO ******************)
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
      elems
      @ [
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
      elems
      @ [
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
      elems
      @ [
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
  (****************** TODO ******************)
  | LessThan ->
      let arg1 = get_var_addr tac.arg1 in
      let arg2 = get_var_addr tac.arg2 in
      add_var_addr tac.result;
      let result = get_var_addr tac.result in
      elems
      @ [
          Instruction ("movq", arg1, "%rax", "");
          Instruction ("movq", "24(%rax)", "%rax", "");
          Instruction ("movq", arg2, "%rdx", "");
          Instruction ("movq", "24(%rdx)", "%rdx", "");
          Instruction ("pushq", "%rdi", "", "");
          Instruction ("pushq", "%rsi", "", "");
          Instruction ("movq", "%rax", "%rdi", "");
          Instruction ("movq", "%rdx", "%rsi", "");
          Instruction ("call", "lt_handler", "", "");
          Instruction ("pushq", "%rbp", "", "");
          Instruction ("pushq", "%rax", "", "");
          Instruction ("call", "$Bool..new", "", "");
          Instruction ("movq", "%rax", "%r10", "");
          Instruction ("popq", "%rax", "", "");
          Instruction ("popq", "%rbp", "", "");
          Instruction ("testq", "%rax", "%rax", "");
          Instruction ("movq", "$0", "24(%r10)", "");
          Instruction ("cmovlq", "$1", "24(%r10)", "");
          Instruction ("popq", "%rsi", "", "");
          Instruction ("popq", "%rdi", "", "");
          Instruction ("movq", "%r10", result, "");
        ]
  (****************** TODO ******************)
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
  (****************** TODO ******************)
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
  (****************** TODO ******************)
  | Not ->
      let arg1 = get_var_addr tac.arg1 in
      add_var_addr tac.result;
      let result = get_var_addr tac.result in
      elems
      @ [
          Instruction ("movq", arg1, "%rax", "");
          Instruction ("movq", "24(%rax)", "%rax", "");
          Instruction ("testq", "%rax", "%rax", "");
          Instruction ("movq", "$0", "%rdx", "");
          Instruction ("cmoveq", "$1", "%rdx", "");
          Instruction ("pushq", "%rbp", "", "");
          Instruction ("pushq", "%rdx", "", "");
          Instruction ("call", "$Bool..new", "", "");
          Instruction ("popq", "%rdx", "", "");
          Instruction ("popq", "%rbp", "", "");
          Instruction ("movq", "%rdx", "24(%rax)", "");
          Instruction ("movq", "%rax", result, "");
        ]
  (****************** TODO ******************)
  | Negate ->
      let arg1 = get_var_addr tac.arg1 in
      add_var_addr tac.result;
      let result = get_var_addr tac.result in
      elems
      @ [
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
  (****************** TODO ******************)
  | Int_Constant ->
      add_var_addr tac.result;
      let result = get_var_addr tac.result in
      elems @ [
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
          elems
          @ [
              Instruction ("call", "$String..new", "", "");
              Instruction ("movq", "$.string" ^ string_of_int str_id, "24(%rax)", "");
              Instruction ("movq", "%rax", result, "");
              (*Instruction ("movq", "$.string" ^ string_of_int str_id, result, "");*)
            ]
      | None ->
          string_counter := !string_counter + 1;
          Hashtbl.add string_map tac.arg1 !string_counter;
          elems
          @ [
              Instruction ("call", "$String..new", "", "");
              Instruction ("movq", "$.string" ^ string_of_int !string_counter, "24(%rax)", "");
              Instruction ("movq", "%rax", result, "");
              (*Instruction*)
                (*("movq", ".string" ^ string_of_int !string_counter, result, "");*)
            ]
      (* This is all we do because they're emitted later :) *))
  (* This is the later but that's another stage; needs to be NOT just in an expression lm ao*)
  (*Printf.fprintf out_file "\t%s\n" (".secton\t.rodata");*)
  (*Printf.fprintf out_file "%s\n" ("string" ^ string_of_int(!string_counter) ^ ":");*)
  (*Printf.fprintf out_file "\t%s\n" (".string \"" ^ string_of_int(!string_counter) ^ "\"");*)
  (*Printf.fprintf out_file "\t%s\n" (".string \"" ^ string_of_int(!string_counter) ^ "\"")*)

  (****************** TODO ******************)
   | Boolean_Constant ->
      add_var_addr tac.result;
      let result = get_var_addr tac.result in
      if tac.arg1 = "true" then
      elems @ [
        Instruction ("call", "$Bool..new", "", "");
        Instruction ("movq", "$1", "24(%rax)", "");
        Instruction ("movq", "%rax", result, "")
      ]
      else
      elems @ [
        Instruction ("call", "$Bool..new", "", "");
        Instruction ("movq", "$0", "24(%rax)", "");
        Instruction ("movq", "%rax", result, "")
      ]
  | _ -> assert false
