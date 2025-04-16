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

and error_reason =
  | ERR_VOID_DISPATCH
  | ERR_VOID_CASE
  | ERR_CASE_NO_BRANCH
  | ERR_DIV_BY_ZERO
  | ERR_SUBSTR_OUT_OF_RANGE

let err_to_num = function
  | ERR_VOID_DISPATCH -> "0"
  | ERR_VOID_CASE -> "1"
  | ERR_CASE_NO_BRANCH -> "2"
  | ERR_DIV_BY_ZERO -> "3"
  | ERR_SUBSTR_OUT_OF_RANGE -> "4"

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
let label_ctr = ref 1

let print_string_map () =
  Hashtbl.iter
    (fun k v ->
      Printf.fprintf Print.out_file
        "\t.align 8\n.string%d:\n\t.string\t\"%s\"\n" v k)
    string_map;
  Printf.fprintf Print.out_file "\t.align 8\n";
  Printf.fprintf Print.out_file
    "\t.globl empty.string\nempty.string:\n\t.string\t\"\"\n";
  Printf.fprintf Print.out_file "\t.align 8\n";
  Printf.fprintf Print.out_file
    "\t.globl percent.ld\npercent.ld:\n\t.string\t\"%%ld\"\n";
  Printf.fprintf Print.out_file "\t.align 8\n";
  Printf.fprintf Print.out_file
    "\t.globl percent.d\npercent.d:\n\t.string\t\"%%d\"\n";
  (*Printf.fprintf Print.out_file "\t.align 8\n";*)
  (*Printf.fprintf Print.out_file*)
  (*"\t.globl percent.s\npercent.s:\n\t.string\t\"%%s\"\n";*)
  Printf.fprintf Print.out_file "\t.align 8\n";
  Printf.fprintf Print.out_file
    "\t.globl .error_dispatch_void_string\n\
     .error_dispatch_void_string:\n\
     \t.string\t\"ERROR: %%zd: Exception: dispatch on void\\n\"\n";
  Printf.fprintf Print.out_file "\t.align 8\n";
  Printf.fprintf Print.out_file
    "\t.globl .error_case_void_string\n\
     error_case_void_string:\n\
     \t.string\t\"ERROR: %%zd: Exception: case on void\\n\"\n";
  Printf.fprintf Print.out_file "\t.align 8\n";
  Printf.fprintf Print.out_file
    "\t.globl .error_case_no_match_string\n\
     error_case_no_match_string:\n\
     \t.string\t\"ERROR: %%zd: Exception: case without matching branch\\n\"\n";
  Printf.fprintf Print.out_file "\t.align 8\n";
  Printf.fprintf Print.out_file
    "\t.globl .error_div_by_zero_string\n\
     error_div_by_zero_string:\n\
     \t.string\t\"ERROR: %%zd: Exception: division by zero\\n\"\n";
  Printf.fprintf Print.out_file "\t.align 8\n";
  Printf.fprintf Print.out_file
    "\t.globl .error_substr_index_bad_string\n\
     error_substr_index_bad_string:\n\
     \t.string\t\"ERROR: %%zd: Exception: String.substr out of range\\n\"\n";
  Printf.fprintf Print.out_file "\t.align 8\n";
  Printf.fprintf Print.out_file
    "\t.globl .abort_string\nabort_string:\n\t.string\t\"abort\"\n";
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
  Printf.fprintf Print.out_file "\t.quad .string%d\n" strid;
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
    Instruction ("movq", "$Bool..vtable", "%r11", "");
    Instruction ("movq", "%r11", "16(%rax)", "");
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
    Instruction ("movq", "$IO..vtable", "%r11", "");
    Instruction ("movq", "%r11", "16(%rax)", "");
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
    Instruction ("movq", "$Int..vtable", "%r11", "");
    Instruction ("movq", "%r11", "16(%rax)", "");
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
    Instruction ("movq", "$Object..vtable", "%r11", "");
    Instruction ("movq", "%r11", "16(%rax)", "");
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
    Instruction ("movq", "$String..vtable", "%r11", "");
    Instruction ("movq", "%r11", "16(%rax)", "");
    Instruction ("movq", "$empty.string", "%r11", "");
    Instruction ("movq", "%r11", "24(%rax)", "");
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

let get_unique_label () =
  label_ctr := !label_ctr + 1;
  ".label" ^ string_of_int !label_ctr

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

let get_offset method_name static_type current_class =
  match static_type with
  | Class c ->
      let vtab = Hashtbl.find class_vtable_map c in
      let res =
        Option.get
          (List.find_index (fun s -> s.method_name = method_name) vtab.methods)
      in
      string_of_int ((res + 1) * 8)
  | SELF_TYPE _ ->
      let vtab = Hashtbl.find class_vtable_map current_class in
      let res =
        Option.get
          (List.find_index (fun s -> s.method_name = method_name) vtab.methods)
      in
      string_of_int ((res + 1) * 8)

(** Method to convert a TAC element to assembly code *)
let tac_to_as (tac : tac_elem) cur_method class_name prev_result =
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
      let prev_addr = get_var_addr prev_result in
      let arg1 = get_var_addr tac.arg1 in
      let void_dispatch_label = get_unique_label () in
      let finish_void_dispatch_label = get_unique_label () in
      [
        Instruction ("movq", prev_addr, "%rax", "");
        Instruction ("movq", "(%rax)", "%rax", "");
        Instruction ("testl", "%rax", "%rax", "");
        Instruction ("je", void_dispatch_label, "", "");
      ]
      @ (if tac.arg2 = "" then
           [ Line "\t#Call w/o args start" ]
           (*@ pusha*)
           @ [
               (*Instruction ("andq", "$0xFFFFFFFFFFFFFFF0", "%rsp", "");*)
               (*Instruction ("call", "IO." ^ tac.arg1, "", "");*)
               Instruction ("pushq", "%rdi", "", "");
               Instruction ("movq", arg1, "%r11", "");
               Instruction ("movq", "16(%r11)", "%r11", "");
               Instruction
                 ( "movq",
                   get_offset cur_method tac.static_type class_name ^ "%r11",
                   "%r11",
                   "" );
               Instruction ("movq", prev_addr, "%rdi", "");
               Instruction ("call", "*%r11", "", "");
               Instruction ("movq", "%rax", result, "");
               Instruction ("jmp", finish_void_dispatch_label, "", "");
             ]
           (*@ popa*)
           @ [ Line "\t#Call w/o args end" ]
         else
           let gen_register_arglist args =
             let registers = [ "%rsi"; "%rdx"; "%rcx"; "%r8"; "%r9" ] in
             List.mapi
               (fun i arg ->
                 Instruction ("movq", get_var_addr arg, List.nth registers i, ""))
               args
           in
           let gen_mixed_arglist args =
             let first_five = List.filteri (fun i _ -> i <= 4) args in
             let remaining = List.rev (List.filteri (fun i _ -> i > 4) args) in
             gen_register_arglist first_five
             @ List.map
                 (fun arg -> Instruction ("pushq", get_var_addr arg, "", ""))
                 remaining
           in

           let args = String.split_on_char ' ' tac.arg2 in
           (* While there ARE 6 argument registers in the SysV convention, we are dedicating rdi to always be the self pointer *)
           let arglist =
             if List.length args <= 5 then gen_register_arglist args
             else gen_mixed_arglist args
           in
           [ Line "\t#Call w/ args start" ]
           @ pusha @ arglist
           @ [
               Instruction ("pushq", "%rdi", "", "");
               Instruction ("movq", arg1, "%r11", "");
               Instruction ("movq", "16(%r11)", "%r11", "");
               Instruction
                 ( "movq",
                   get_offset cur_method tac.static_type class_name ^ "%r11",
                   "%r11",
                   "" );
               Instruction ("movq", prev_addr, "%rdi", "");
               Instruction ("call", "*%r11", "", "");
               Instruction ("movq", "%rax", result, "");
               Instruction ("jmp", finish_void_dispatch_label, "", "");
             ]
           @ popa
           @ [ Line "\t#Call w/ args end" ])
      @
      (***** TODO: finish void dispatch label and void dispatch err handling *****)
      [
        Line (void_dispatch_label ^ ":");
        Instruction ("movl", "$" ^ string_of_int tac.line_num, "esi", "");
        Instruction ("movl", "$" ^ err_to_num ERR_VOID_DISPATCH, "", "");
        Instruction ("call", "cool_error", "", "");
        Line (finish_void_dispatch_label ^ ":");
      ]
      (* Push all variables onto stack *)
      (* Push all onto stack *)
      (*[Instruction{instruction = "callq"; arg1 = Some tac.arg1; arg2 = ""; arg3 = ""}]*)
      (*Printf.fprintf out_file "\tcallq %s\n" tac.arg1*)
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
        Instruction ("movq", "%rax", "%r11", "");
        Instruction ("popq", "%rax", "", "");
        Instruction ("popq", "%rbp", "", "");
        Instruction ("movq", "%r11", result, "");
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
        Instruction ("movq", "%rax", "%r11", "");
        Instruction ("popq", "%rax", "", "");
        Instruction ("popq", "%rbp", "", "");
        Instruction ("movq", "%rax", "24(%r11)", "");
        Instruction ("movq", "%r11", result, "");
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
        Instruction ("movq", "%rax", "%r11", "");
        Instruction ("popq", "%rax", "", "");
        Instruction ("popq", "%rbp", "", "");
        Instruction ("movq", "%rax", "24(%r11)", "");
        Instruction ("movq", "%r11", result, "");
        Line "\t#Minus end";
      ]
  | Divide ->
      let arg1 = get_var_addr tac.arg1 in
      let arg2 = get_var_addr tac.arg2 in
      add_var_addr tac.result;
      let result = get_var_addr tac.result in
      let error_label = get_unique_label () in
      let div_end_label = get_unique_label () in

      [
        Line "\t#Divide start";
        Instruction ("movq", arg2, "%rcx", "");
        Instruction ("movq", "24(%rcx)", "%rcx", "");
        Instruction ("testq", "%rcx", "%rcx", "");
        Instruction ("je", error_label, "", "");
        Instruction ("movq", arg1, "%rax", "");
        Instruction ("movq", "24(%rax)", "%rax", "");
        Instruction ("cltd", "", "", "");
        Instruction ("idivl", "%ecx", "", "");
        Instruction ("pushq", "%rbp", "", "");
        Instruction ("pushq", "%rax", "", "");
        Instruction ("call", "Int..new", "", "");
        Instruction ("movq", "%rax", "%r11", "");
        Instruction ("popq", "%rax", "", "");
        Instruction ("popq", "%rbp", "", "");
        Instruction ("movq", "%rax", "24(%r11)", "");
        Instruction ("movq", "%r11", result, "");
        Instruction ("jmp", div_end_label, "", "");
        Line (error_label ^ ":");
        Instruction ("movl", "$" ^ string_of_int tac.line_num, "esi", "");
        Instruction ("movl", "$" ^ err_to_num ERR_DIV_BY_ZERO, "", "");
        Instruction ("call", "cool_error", "", "");
        Line (div_end_label ^ ":");
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
        Instruction ("movq", "%rax", "%r11", "");
        Instruction ("popq", "%rax", "", "");
        Instruction ("popq", "%rbp", "", "");
        Instruction ("movq", "%rax", "24(%r11)", "");
        Instruction ("movq", "%r11", result, "");
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
        Instruction ("movq", "%rax", "%r11", "");
        Instruction ("popq", "%rax", "", "");
        Instruction ("popq", "%rbp", "", "");
        Instruction ("movq", "%rax", "24(%r11)", "");
        Instruction ("movq", "%r11", result, "");
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
              ("movq", "$.string" ^ string_of_int str_id, "24(%rax)", "");
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
              ( "movq",
                "$.string" ^ string_of_int !string_counter,
                "24(%rax)",
                "" );
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
let prev_result = ref ""

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
      @ (List.map
           (fun tac ->
             let t = tac_to_as tac method_name class_name !prev_result in
             prev_result := tac.result;
             t)
           method_tac
        |> List.flatten)
        (* @ [Asm.Line (Printf.sprintf "\t.size\t%s, .-%s" name name)]*)
      @ get_end_method_boilerplate method_name stack_space)
    Cfg.cfg_list

let tac_list_to_asm lst = List.map tac_to_as lst
