open Print
open Parser
open Tac

(* Instructions represent actual instructions *)
(* Lines represent comments *)
type asm_instruction = string * string * string * string
and asm_line = Instruction of asm_instruction | Line of string
and asm = asm_line list

(* A vtable function *)
type vtable_func = { type_name : string; method_name : string }

(* A vtable *)
and vtable = {
  name_id : string;
  name_string_id : int;
  methods : vtable_func list;
}

(* A class attribute *)
and attribute = {
  field_name : string;
  index : int;
  type_name : string;
  expression : (Cfg.cfg * string) option;
}

(* Internal representation of a class in assembly *)
and asm_class = {
  class_id : int;
  object_size : int;
  vtable : vtable;
  attributes : attribute list;
}

(* Represents [Class]..new *)
(* Class name * Assembly code *)
and new_func = string * asm

(* Error handling *)
type error_reason =
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

(* Print assembly instructions to file *)
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
  | Line s1 ->
      (*Printf.printf "%s\n" s1;*)
      if String.contains s1 '#' then Printf.fprintf debug_file "%s\n" s1
      else Printf.fprintf out_file "%s\n" s1

(* Print out new funcs *)
let print_new_funcs (funcs : new_func list) =
  let print_new_func func =
    let name, lines = func in
    Printf.fprintf Print.out_file "\t.p2align 4\n";
    Printf.fprintf Print.out_file "\t.globl\t%s..new\n" name;
    Printf.fprintf Print.out_file "\t.type\t%s..new, @function\n" name;
    List.iter (fun ln -> print_asm ln) lines;
    (*Printf.fprintf Print.out_file "\t.size\t%s, .-%s\n" name name;*)
    Printf.fprintf Print.debug_file
      "\t#;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;\n"
  in
  List.iter print_new_func funcs

(* Locations of variables *)
let var_locations = Hashtbl.create 32

(* Print var_locations map *)
let print_var_locations () =
  Hashtbl.iter
    (fun k v ->
      Printf.fprintf debug_file "\t#; Key: %s, Value: %d(%%rbp)\n" k v)
    var_locations

(* Sting Constants in the program; Counter for labeling each string *)
let string_map = Hashtbl.create 32
let string_counter = ref 10

(* Initialize string map *)
let () =
  let init_string_map () =
    let string0 = "Bool" in
    let string1 = "IO" in
    let string2 = "Int" in
    let string3 = "Object" in
    let string4 = "String" in
    let string6 = "abort" in
    let string7 = "ERROR: 0: Exception: String.substr out of range\\n" in
    let string8 = "ERROR: 6: Exception: case without matching branch\\n" in
    let string9 = "ERROR: 6: Exception: case on void\\n" in
    Hashtbl.add string_map string0 0;
    Hashtbl.add string_map string1 1;
    Hashtbl.add string_map string2 2;
    Hashtbl.add string_map string3 3;
    Hashtbl.add string_map string4 4;
    Hashtbl.add string_map string6 6;
    Hashtbl.add string_map string7 7;
    Hashtbl.add string_map string8 8;
    Hashtbl.add string_map string9 9
  in
  init_string_map ()

(* Class Ids for each class; Counter for assigning unique class ids *)
let class_id_map = Hashtbl.create 32
let class_id_ctr = ref 14

(* Initialize class_id_map *)
let () =
  let init_class_id_map () =
    Hashtbl.add class_id_map "Bool" 1;
    Hashtbl.add class_id_map "IO" 11;
    Hashtbl.add class_id_map "Int" 2;
    Hashtbl.add class_id_map "Object" 13;
    Hashtbl.add class_id_map "String" 5
  in
  init_class_id_map ()

(* Vtables for each class *)
let class_vtable_map = Hashtbl.create 32

let print_vtable_map () =
  Printf.fprintf stderr "#; Vtables:\n";
  Hashtbl.iter
    (fun k _ -> Printf.fprintf stderr "\t#; Class: %s;\n" k)
    class_vtable_map

(* Attributes for each class *)
let class_attribute_map = Hashtbl.create 32

(* Get specific attribute of a class *)
let get_attribute class_name attr_name =
  (* Get attribute map of a class *)
  let get_attribute_map class_name =
    match Hashtbl.find_opt class_attribute_map class_name with
    | Some v -> v
    | None ->
        Printf.fprintf stderr "Failed to find the attributes of class %s\n"
          class_name;
        assert false
  in
  let attrs = get_attribute_map class_name in
  List.find_opt (fun f -> f.field_name = attr_name) attrs

(* Initializes class_attribute_map *)
let () =
  let init_class_attribute_map () =
    let init class_name i (attr : ast_attribute) =
      let name = attr.name in
      let index = i in
      let typename = attr.type_name in
      let expr =
        match attr.attr_expr with
        | Some attr_expr ->
            var_ctr := 0;
            (*label_ctr := 0;*)
            let base_lst =
              [
                {
                  operand = Comment;
                  arg1 = "attr start";
                  arg2 = "";
                  result = "";
                  line = attr_expr.id.line_num;
                  static_type = attr_expr.static_type;
                };
              ]
            in
            let return_index = get_id !var_ctr in
            let exp_list =
              exp_to_tac attr_expr return_index class_name attr.name
            in
            (* let temps = !var_ctr + 1 in *)
            let ret =
              Cfg.tac_to_cfg (base_lst @ exp_list, "", "", [], !var_ctr + 1)
            in
            Some (ret, return_index)
        | None -> None
      in
      { field_name = name; index; type_name = typename; expression = expr }
    in
    List.iter
      (fun (class_elem : class_map_elem) ->
        let attributes = List.mapi (init class_elem.name) class_elem.attrs in
        Hashtbl.add class_attribute_map class_elem.name attributes)
      parser_class_map
  in
  init_class_attribute_map ()

(* Arguments for each method *)
let arg_map = Hashtbl.create 32

(* Class Map *)
let parser_class_map : class_map_elem list = Parser.parser_class_map

(* Implementation Map *)
let implementation_map = Parser.implementation_map

(* Parent Map *)
let parent_map = Parser.parent_map
let label_ctr = ref 1

(* Storage for the previous tac element, used for determining type of non-static method calls *)
let prev_tac =
  ref
    {
      operand = New;
      arg1 = "";
      arg2 = "";
      result = "";
      line = 0;
      static_type = None;
    }

(* Print string map *)
let print_string_map () =
  Hashtbl.iter
    (fun k v ->
      Printf.fprintf Print.out_file
        "\t.align 8\n.globl .string%d\n.string%d:\n\t.string\t\"%s\"\n" v v k)
    string_map;
  Printf.fprintf Print.out_file "\t.align 8\n";
  Printf.fprintf Print.out_file
    "\t.globl empty.string\nempty.string:\n\t.string\t\"\"\n";
  Printf.fprintf Print.out_file "\t.align 8\n";
  Printf.fprintf Print.out_file
    "\t.globl .percent.ld\n.percent.ld:\n\t.string\t\"%%ld\"\n";
  Printf.fprintf Print.out_file "\t.align 8\n";
  Printf.fprintf Print.out_file
    "\t.globl .percent.d\n.percent.d:\n\t.string\t\"%%d\"\n";
  Printf.fprintf Print.out_file "\t.align 8\n";
  Printf.fprintf Print.out_file
    "\t.globl .error_dispatch_void_string\n\
     .error_dispatch_void_string:\n\
     \t.string\t\"ERROR: %%zd: Exception: dispatch on void\\n\"\n";
  Printf.fprintf Print.out_file "\t.align 8\n";
  Printf.fprintf Print.out_file
    "\t.globl .error_case_void_string\n\
     .error_case_void_string:\n\
     \t.string\t\"ERROR: %%zd: Exception: case on void\\n\"\n";
  Printf.fprintf Print.out_file "\t.align 8\n";
  Printf.fprintf Print.out_file
    "\t.globl .error_case_no_match_string\n\
     .error_case_no_match_string:\n\
     \t.string\t\"ERROR: %%zd: Exception: case without matching branch\\n\"\n";
  Printf.fprintf Print.out_file "\t.align 8\n";
  Printf.fprintf Print.out_file
    "\t.globl .error_div_by_zero_string\n\
     .error_div_by_zero_string:\n\
     \t.string\t\"ERROR: %%zd: Exception: division by zero\\n\"\n";
  Printf.fprintf Print.out_file "\t.align 8\n";
  Printf.fprintf Print.out_file
    "\t.globl .error_substr_index_bad_string\n\
     .error_substr_index_bad_string:\n\
     \t.string\t\"ERROR: %%zd: Exception: String.substr out of range\\n\"\n";
  Printf.fprintf Print.out_file "\t.align 8\n";
  Printf.fprintf Print.out_file
    "\t.globl .abort_string\n.abort_string:\n\t.string\t\"abort\"\n";
  Printf.fprintf Print.out_file "\t.text\n"

(* Generate vtable from implementation_map_elem *)
let create_vtable (itm : implementation_map_elem) : vtable option =
  let name = itm.name in
  (* Don't create vtables for default classes *)
  if
    name <> "Bool" && name <> "IO" && name <> "Int" && name <> "Object"
    && name <> "String"
  then (
    (* Vtable is comprise of Class..new followed by class methods *)
    let funcs =
      [ { type_name = itm.name; method_name = ".new" } ]
      @ List.map
          (fun (meth : imp_method) ->
            { type_name = meth.type_name; method_name = meth.name })
          itm.methods
    in
    (* Add class_name to string_map *)
    string_counter := !string_counter + 1;
    Hashtbl.add string_map itm.name !string_counter;
    Some { name_id = name; name_string_id = !string_counter; methods = funcs })
  else None

(* Generate vtable from implementation_map *)
let create_vtables () =
  let tables = List.filter_map create_vtable implementation_map in
  List.iter (fun i -> Hashtbl.add class_vtable_map i.name_id i) tables

(* Print vtable function *)
let print_vtable (table : vtable) =
  let print_vtable_func func =
    Printf.fprintf Print.out_file "\t.quad %s.%s\n" func.type_name
      func.method_name
  in
  let name = table.name_id in
  let strid = table.name_string_id in
  Printf.fprintf Print.out_file ".globl %s..vtable\n" name;
  Printf.fprintf Print.out_file "%s..vtable:\n" name;
  Printf.fprintf Print.out_file "\t.quad .string%d\n" strid;
  List.iter print_vtable_func table.methods;
  Printf.fprintf Print.debug_file
    "\t#;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;\n"

(* Vtables for default values *)
let create_default_vtables () =
  let vtables =
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
  in
  List.iter (fun f -> Hashtbl.add class_vtable_map f.name_id f) vtables;
  vtables

(* Push all registers *)
let pusha =
  [
    Instruction ("pushq", "%rax", "", "");
    Instruction ("pushq", "%rdi", "", "");
    Instruction ("pushq", "%rsi", "", "");
    Instruction ("pushq", "%rcx", "", "");
    Instruction ("pushq", "%rdx", "", "");
    Instruction ("pushq", "%r8", "", "");
    Instruction ("pushq", "%r9", "", "");
    Instruction ("pushq", "%r10", "", "");
    Instruction ("pushq", "%r11", "", "");
    Instruction ("pushq", "%r12", "", "");
  ]

(* Pop all registers *)
let popa =
  [
    Instruction ("popq", "%r12", "", "");
    Instruction ("popq", "%r11", "", "");
    Instruction ("popq", "%r10", "", "");
    Instruction ("popq", "%r9", "", "");
    Instruction ("popq", "%r8", "", "");
    Instruction ("popq", "%rdx", "", "");
    Instruction ("popq", "%rcx", "", "");
    Instruction ("popq", "%rsi", "", "");
    Instruction ("popq", "%rdi", "", "");
    Instruction ("popq", "%rax", "", "");
  ]

(* Push all argument registers *)
let pushargs =
  [
    Instruction ("pushq", "%rdi", "", "");
    Instruction ("pushq", "%rsi", "", "");
    Instruction ("pushq", "%rdx", "", "");
    Instruction ("pushq", "%rcx", "", "");
    Instruction ("pushq", "%r8", "", "");
    Instruction ("pushq", "%r9", "", "");
  ]

(* Pop all argument registers *)
let popargs =
  [
    Instruction ("popq", "%r9", "", "");
    Instruction ("popq", "%r8", "", "");
    Instruction ("popq", "%rcx", "", "");
    Instruction ("popq", "%rdx", "", "");
    Instruction ("popq", "%rsi", "", "");
    Instruction ("popq", "%rdi", "", "");
  ]

(* Get assembly label *)
let get_label () =
  label_ctr := !label_ctr + 1;
  ".label" ^ string_of_int !label_ctr

(* Print class attributes *)
let print_class_attributes () =
  Hashtbl.iter
    (fun k attrlist ->
      Printf.fprintf debug_file "\t#; Class: %s\n" k;
      List.iter
        (fun attr ->
          Printf.fprintf debug_file "\t\t#; Attribute: %s\n" attr.field_name)
        attrlist)
    class_attribute_map

(* Create a class in assembly *)
let make_asm_class (c : class_map_elem) =
  let id = !class_id_ctr in
  class_id_ctr := !class_id_ctr + 1;

  if
    c.name <> "Bool" && c.name <> "IO" && c.name <> "Int" && c.name <> "Object"
    && c.name <> "String"
  then (
    Hashtbl.add class_id_map c.name id;
    (* 8 bytes/64 bits since every attribute is a pointer *)
    let siz = List.length c.attrs in
    let class_vtable = Hashtbl.find_opt class_vtable_map c.name in
    let attrs =
      match Hashtbl.find_opt class_attribute_map c.name with
      | Some v -> v
      | None ->
          Printf.fprintf stderr "Could not find attributes for class %s\n"
            c.name;
          assert false
    in
    match class_vtable with
    | Some cv ->
        Some
          { class_id = id; object_size = siz; vtable = cv; attributes = attrs }
    | None -> None)
  else None

(* Add variable to var_locations, if not already in *)
let add_var_addr (var_name : string) =
  let fp_offset = 8 * (Hashtbl.length var_locations + 1) in
  Printf.fprintf debug_file "#Adding var %s to location %d\n" var_name fp_offset;
  match Hashtbl.find_opt var_locations var_name with
  | None -> Hashtbl.add var_locations var_name fp_offset
  | Some _ ->
      (* Printf.fprintf debug_file "Error: Variable %s already has a location\n"
        var_name *)
      ()

(* Get address of a varaible *)
let get_var_addr (var_name : string) (class_name : string) : string =
  let find_attr class_name attr_name =
    match Hashtbl.find_opt class_attribute_map class_name with
    | Some attr_list -> (
        match
          List.find_opt (fun attr -> attr.field_name = attr_name) attr_list
        with
        | Some attr -> Printf.sprintf "%d(%%rdi)" ((attr.index + 3) * 8)
        | None -> attr_name)
    | None -> attr_name
  in
  (* First check if variable is an argument *)
  (*match Hashtbl.find_opt arg_map var_name with*)
  (*| Some addr ->*)
  (*if addr < 0 then *)
  (*let registers = [ "%rsi"; "%rdx"; "%rcx"; "%r8"; "%r9" ] in*)
  (*let register = List.nth registers (addr + 5) in*)
  (*Printf.fprintf out_file*)
  (*"\t#; CHECK1 Argument %s is stored in register %d (%s)\n" var_name addr*)
  (*register;*)
  (*register)*)
  (*else Printf.sprintf "%d(%%rbp)" addr*)
  (*| None -> *)
  (*match Hashtbl.find_opt var_locations var_name with*)
  (*(* Third check if variable is stored somewhere already *)*)
  (*| Some addr -> Printf.fprintf out_file "#Found argument -%d(%%rbp) for %s\n" addr var_name; Printf.sprintf "-%d(%%rbp)" addr*)
  (*| None ->*)
  (*(* Fourth check if variable refers to self pointer *)*)
  (*if var_name = "self" || var_name = "%rdi" then "%rdi"*)
  (*(* Fifth: That sucks, error *)*)
  (*else *)
  (*(*Printf.fprintf out_file*)*)
  (*(*"\t#; Failed to find a temp for variable %s (or it's an attribute!)\n" var_name;*)*)
  (*(*print_var_locations ();*)*)
  (*add_var_addr var_name; Printf.sprintf "-%d(%%rbp)" (Hashtbl.find var_locations var_name)))*)
  (* first check if in var map *)
  Printf.fprintf debug_file "# VAR NAME %s\n" var_name;
  if var_name.[0] = '$' then var_name
  else
    match Hashtbl.find_opt var_locations var_name with
    (* Third check if variable is stored somewhere already *)
    | Some addr ->
        Printf.fprintf debug_file "#Found variable -%d(%%rbp) for %s\n" addr
          var_name;
        Printf.sprintf "-%d(%%rbp)" addr
    | None -> (
        match Hashtbl.find_opt arg_map var_name with
        | Some arg ->
            if arg < 0 then (
              let registers = [ "%rsi"; "%rdx"; "%rcx"; "%r8"; "%r9" ] in
              let register = List.nth registers (arg + 6) in
              Printf.fprintf debug_file
                "\t#; CHECK1 Argument %s is stored in register %d (%s)\n"
                var_name arg register;
              register)
            else Printf.sprintf "%d(%%rbp)" arg
        | None ->
            if var_name = "self" || var_name = "%rdi" then "%rdi"
            else
              let a = find_attr class_name var_name in
              if a = var_name then (
                add_var_addr var_name;
                Printf.sprintf "-%d(%%rbp)"
                  (Hashtbl.find var_locations var_name))
              else a)

(* Fourth check if variable refers to self pointer *)
(*if var_name = "self" || var_name = "%rdi" then "%rdi"*)
(* Fifth: That sucks, error *)
(*else *)
(*Printf.fprintf out_file*)
(*"\t#; Failed to find a temp for variable %s (or it's an attribute!)\n" var_name;*)
(*print_var_locations ();*)
(*add_var_addr var_name; Printf.sprintf "-%d(%%rbp)" (Hashtbl.find var_locations var_name))*)

(* Global counter for jump points *)
let jump_number = ref 1

(* Increment & get jump number *)
let get_jump () =
  jump_number := !jump_number + 1;
  !jump_number

(* Taken/modified from https://stackoverflow.com/questions/31279920/finding-an-item-in-a-list-and-returning-its-index-ocaml *)
(* Index of an item in a list *)
let rec find x lst count =
  match lst with
  | [] -> assert false
  | h :: t -> if x = h.method_name then count else find x t (count + 1)

(* Get offset of a method within a vtable *)
let get_offset method_name static_type current_class =
  match static_type with
  | Class c -> (
      let c = if c = "SELF_TYPE" then current_class else c in
      let vtab = Hashtbl.find_opt class_vtable_map c in
      match vtab with
      | Some vtab ->
          let res = find method_name vtab.methods 0 in
          string_of_int ((res + 1) * 8)
      | None ->
          print_vtable_map ();
          Printf.fprintf stderr "Failed to find the vtable of class %s\n" c;
          assert false)
  | SELF_TYPE _ -> (
      let vtab = Hashtbl.find_opt class_vtable_map current_class in
      match vtab with
      | Some vtab ->
          let res = find method_name vtab.methods 0 in
          string_of_int ((res + 1) * 8)
      | None ->
          print_vtable_map ();
          Printf.fprintf stderr "Failed to find the vtable of class %s\n"
            current_class;
          assert false)

(* Character escaping*)
let transform_string s =
  String.fold_left
    (fun acc ch ->
      acc
      ^
      match ch with
      | ' ' .. '~' ->
          if ch = '"' then "\\\""
          else if ch = '\\' then "\\\\"
          else String.make 1 ch
      | '\x00' .. '\x1f' | '\x7f' ->
          if ch = '\n' then "\\n"
          else if ch = '\t' then "\\t"
          else Char.escaped ch
      | _ -> String.make 1 ch (* unicode we just hope is good *))
    "" s

(** Method to convert a TAC element to assembly code *)
let tac_to_as (tac : tac_elem) cur_method class_name prev_tac =
  [ Line ("#" ^ Tac.get_tac_elem tac) ]
  @
  match tac.operand with
  | Assignment -> (
      let attrs =
        match Hashtbl.find_opt class_attribute_map class_name with
        | Some v -> v
        | None ->
            Printf.fprintf stderr "Failed to find the attributes of class %s\n"
              class_name;
            assert false
      in
      match List.find_opt (fun f -> f.field_name = tac.arg1) attrs with
      | Some v ->
          Printf.fprintf debug_file
            "#; Assignment in Class %s to attribute %s : %s \n" class_name
            v.field_name v.type_name;
          (*add_var_addr tac.result;*)
          let result = get_var_addr tac.result class_name in
          let arg1 = Printf.sprintf "%d(%%rdi)" ((v.index + 3) * 8) in
          [
            Line "\t#Assignment start";
            Instruction ("movq", arg1, "%rax", "");
            Instruction ("movq", "%rax", result, "");
            Line "\t#Assignment end";
          ]
      | None ->
          Printf.fprintf debug_file
            "#; Class %s does not have an attribute named %s \n" class_name
            tac.result;
          let result = get_var_addr tac.result class_name in
          let arg1 = get_var_addr tac.arg1 class_name in
          [
            Line "\t#Assignment start";
            Instruction ("movq", arg1, "%rax", "");
            Instruction ("movq", "%rax", result, "");
            Line "\t#Assignment end";
          ])
  | Bt ->
      let arg1 = get_var_addr tac.arg1 class_name in
      [
        Line "\t#Branch True start";
        Instruction ("movq", arg1, "%rax", "");
        Instruction ("movq", "24(%rax)", "%rax", "");
        Instruction ("testq", "%rax", "%rax", "");
        Instruction ("jne", tac.arg2, "", "");
        Line "\t#Branch True end";
      ]
  | Call ->
      let result =
        (*match get_attribute class_name tac.result with*)
        (*| Some v -> Printf.sprintf "%d(%%rdi)" ((v.index + 3) * 8)*)
        (*| None ->*)
        (*Printf.fprintf out_file "#; %s.%s: %s is not an attribute\n"*)
        (*class_name cur_method tac.result;*)
        (*(*add_var_addr tac.result;*)*)
        (*get_var_addr tac.result class_name*)
        get_var_addr tac.result class_name
      in
      let prev_addr = get_var_addr prev_tac.result class_name in
      let meth = tac.arg1 in
      let void_dispatch_label = get_label () in
      let finish_void_dispatch_label = get_label () in
      (* pushargs *)
      [
        Instruction ("movq", prev_addr, "%rax", "");
        Instruction ("testl", "%eax", "%eax", "");
        Instruction ("je", void_dispatch_label, "", "");
        Instruction ("movq", "(%rax)", "%rax", "");
        Instruction ("testl", "%eax", "%eax", "");
        Instruction ("je", void_dispatch_label, "", "");
      ]
      @ (if tac.arg2 = "" then
           [
             Line ("\t#Call w/o args start for method " ^ meth);
             Instruction ("pushq", "%rsi", "", "");
             Instruction ("pushq", "%rdx", "", "");
             Instruction ("pushq", "%rcx", "", "");
             Instruction ("pushq", "%r8", "", "");
             Instruction ("pushq", "%r9", "", "");
           ]
           @ [
               (*Instruction ("andq", "$0xFFFFFFFFFFFFFFF0", "%rsp", "");*)
               (*Instruction ("call", "IO." ^ tac.arg1, "", "");*)
               Instruction ("pushq", "%rdi", "", "");
               Instruction ("movq", prev_addr, "%r11", "");
               Instruction ("movq", "16(%r11)", "%r11", "");
               Instruction
                 ( "movq",
                   get_offset meth (Option.get prev_tac.static_type) class_name
                   ^ "(%r11)",
                   "%r11",
                   "" );
               Instruction ("movq", prev_addr, "%rdi", "");
               Instruction ("call", "*%r11", "", "");
               Instruction ("popq", "%rdi", "", "");
               Instruction ("movq", "%rax", result, "");
             ]
           @ [
               Instruction ("popq", "%r9", "", "");
               Instruction ("popq", "%r8", "", "");
               Instruction ("popq", "%rcx", "", "");
               Instruction ("popq", "%rdx", "", "");
               Instruction ("popq", "%rsi", "", "");
               Line ("\t#Call w/o args end for method " ^ meth);
             ]
         else
           let gen_register_arglist args =
             let registers = [ "%rsi"; "%rdx"; "%rcx"; "%r8"; "%r9" ] in
             List.mapi
               (fun i arg ->
                 let arg_adr =
                   (*match get_attribute class_name arg with*)
                   (*| Some v -> Printf.sprintf "%d(%%rdi)" ((v.index + 3) * 8)*)
                   (*| None -> get_var_addr arg class_name*)
                   get_var_addr arg class_name
                 in
                 Instruction ("movq", arg_adr, List.nth registers i, ""))
               args
           in
           let gen_mixed_arglist args =
             let first_five = List.filteri (fun i _ -> i <= 4) args in
             let remaining = List.rev (List.filteri (fun i _ -> i > 4) args) in
             gen_register_arglist first_five
             @ List.map
                 (fun arg ->
                   let arg_adr =
                     (*match get_attribute class_name arg with*)
                     (*| Some v -> Printf.sprintf "%d(%%rdi)" ((v.index + 3) * 8)*)
                     (*| None -> get_var_addr arg class_name*)
                     get_var_addr arg class_name
                   in
                   Instruction ("pushq", arg_adr, "", ""))
                 remaining
           in

           let args = String.split_on_char ' ' tac.arg2 in
           (* While there ARE 6 argument registers in the SysV convention, we are dedicating rdi to always be the self pointer *)
           let arglist =
             if List.length args <= 5 then gen_register_arglist args
             else gen_mixed_arglist args
           in
           [
             Line ("\t#Call w/ args start for method " ^ meth);
             Instruction ("pushq", "%rsi", "", "");
             Instruction ("pushq", "%rdx", "", "");
             Instruction ("pushq", "%rcx", "", "");
             Instruction ("pushq", "%r8", "", "");
             Instruction ("pushq", "%r9", "", "");
             Instruction ("pushq", "%rdi", "", "");
           ]
           @ (if List.length arglist > 5 then
                if List.length arglist mod 2 = 1 then []
                else [ Instruction ("subq", "$8", "%rsp", "") ]
              else [])
           @ arglist
           @ [
               Instruction ("movq", prev_addr, "%r11", "");
               Instruction ("movq", "16(%r11)", "%r11", "");
               Instruction
                 ( "movq",
                   get_offset meth (Option.get prev_tac.static_type) class_name
                   ^ "(%r11)",
                   "%r11",
                   "" );
               Instruction ("movq", prev_addr, "%rdi", "");
               Instruction ("call", "*%r11", "", "");
             ]
           @ (if List.length arglist > 5 then
                [
                  Instruction
                    ( "addq",
                      "$"
                      ^ string_of_int
                          (if List.length arglist mod 2 = 1 then
                             8 * (List.length arglist - 5)
                           else 8 * (List.length arglist - 4)),
                      "%rsp",
                      "" );
                ]
              else [])
           @ [
               Instruction ("popq", "%rdi", "", "");
               Instruction ("movq", "%rax", result, "");
             ]
           @ [
               Instruction ("popq", "%r9", "", "");
               Instruction ("popq", "%r8", "", "");
               Instruction ("popq", "%rcx", "", "");
               Instruction ("popq", "%rdx", "", "");
               Instruction ("popq", "%rsi", "", "");
               Line ("\t#Call w/ args end for method" ^ meth);
             ])
      (* @ popargs *)
      @
      (***** TODO: finish void dispatch label and void dispatch err handling *****)
      [
        Instruction ("jmp", finish_void_dispatch_label, "", "");
        Line (void_dispatch_label ^ ":");
        Instruction ("movl", "$" ^ string_of_int tac.line, "%esi", "");
        Instruction ("movl", "$" ^ err_to_num ERR_VOID_DISPATCH, "%edi", "");
        Instruction ("call", "cool_error", "", "");
        Line (finish_void_dispatch_label ^ ":");
      ]
  | StaticCall static_class ->
      let result =
        (*match get_attribute static_class tac.result with*)
        (*| Some v -> Printf.sprintf "%d(%%rdi)" ((v.index + 3) * 8)*)
        (*| None ->*)
        (*Printf.fprintf out_file "#; %s.%s: %s is not an attribute\n"*)
        (*static_class cur_method tac.result;*)
        (*(*add_var_addr tac.result;*)*)
        get_var_addr tac.result class_name
      in
      let prev_addr = get_var_addr prev_tac.result class_name in
      let meth = tac.arg1 in
      let void_dispatch_label = get_label () in
      let finish_void_dispatch_label = get_label () in
      (* pushargs *)
      [
        Instruction ("movq", prev_addr, "%rax", "");
        Instruction ("testl", "%eax", "%eax", "");
        Instruction ("je", void_dispatch_label, "", "");
        Instruction ("movq", "(%rax)", "%rax", "");
        Instruction ("testl", "%eax", "%eax", "");
        Instruction ("je", void_dispatch_label, "", "");
      ]
      @ (if tac.arg2 = "" then
           [
             Line ("\t#Call w/o args start for method " ^ meth);
             Instruction ("pushq", "%rsi", "", "");
             Instruction ("pushq", "%rdx", "", "");
             Instruction ("pushq", "%rcx", "", "");
             Instruction ("pushq", "%r8", "", "");
             Instruction ("pushq", "%r9", "", "");
           ]
           @ [
               (*Instruction ("andq", "$0xFFFFFFFFFFFFFFF0", "%rsp", "");*)
               (*Instruction ("call", "IO." ^ tac.arg1, "", "");*)
               Instruction ("pushq", "%rdi", "", "");
               Instruction ("movq", "$" ^ static_class ^ "..vtable", "%r11", "");
               Instruction
                 ( "movq",
                   get_offset meth
                     (Option.get prev_tac.static_type)
                     static_class
                   ^ "(%r11)",
                   "%r11",
                   "" );
               Instruction ("movq", prev_addr, "%rdi", "");
               Instruction ("call", "*%r11", "", "");
               Instruction ("popq", "%rdi", "", "");
               Instruction ("movq", "%rax", result, "");
             ]
           @ [
               Instruction ("popq", "%r9", "", "");
               Instruction ("popq", "%r8", "", "");
               Instruction ("popq", "%rcx", "", "");
               Instruction ("popq", "%rdx", "", "");
               Instruction ("popq", "%rsi", "", "");
               Line ("\t#Call w/o args end for method " ^ meth);
             ]
         else
           let gen_register_arglist args =
             let registers = [ "%rsi"; "%rdx"; "%rcx"; "%r8"; "%r9" ] in
             List.mapi
               (fun i arg ->
                 let arg_adr =
                   match get_attribute static_class arg with
                   | Some v -> Printf.sprintf "%d(%%rdi)" ((v.index + 3) * 8)
                   | None -> get_var_addr arg class_name
                 in
                 Instruction ("movq", arg_adr, List.nth registers i, ""))
               args
           in
           let gen_mixed_arglist args =
             let first_five = List.filteri (fun i _ -> i <= 4) args in
             let remaining = List.rev (List.filteri (fun i _ -> i > 4) args) in
             gen_register_arglist first_five
             @ List.map
                 (fun arg ->
                   let arg_adr =
                     match get_attribute static_class arg with
                     | Some v -> Printf.sprintf "%d(%%rdi)" ((v.index + 3) * 8)
                     | None -> get_var_addr arg class_name
                   in
                   Instruction ("pushq", arg_adr, "", ""))
                 remaining
           in

           let args = String.split_on_char ' ' tac.arg2 in
           (* While there ARE 6 argument registers in the SysV convention, we are dedicating rdi to always be the self pointer *)
           let arglist =
             if List.length args <= 5 then gen_register_arglist args
             else gen_mixed_arglist args
           in
           [
             Line ("\t#Call w/ args start for method " ^ meth);
             Instruction ("pushq", "%rsi", "", "");
             Instruction ("pushq", "%rdx", "", "");
             Instruction ("pushq", "%rcx", "", "");
             Instruction ("pushq", "%r8", "", "");
             Instruction ("pushq", "%r9", "", "");
             Instruction ("pushq", "%rdi", "", "");
           ]
           @ (if List.length arglist > 5 then
                if List.length arglist mod 2 = 0 then []
                else [ Instruction ("subq", "$8", "%rsp", "") ]
              else [])
           @ arglist
           @ [
               Instruction ("movq", "$" ^ static_class ^ "..vtable", "%r11", "");
               Instruction
                 ( "movq",
                   get_offset meth
                     (Option.get prev_tac.static_type)
                     static_class
                   ^ "(%r11)",
                   "%r11",
                   "" );
               Instruction ("movq", prev_addr, "%rdi", "");
               Instruction ("call", "*%r11", "", "");
             ]
           @ (if List.length arglist > 5 then
                [
                  Instruction
                    ( "addq",
                      "$"
                      ^ string_of_int
                          (if List.length arglist mod 2 = 0 then
                             8 * (List.length arglist - 5)
                           else 8 * (List.length arglist - 4)),
                      "%rsp",
                      "" );
                ]
              else [])
           @ [
               Instruction ("popq", "%rdi", "", "");
               Instruction ("movq", "%rax", result, "");
             ]
           @ [
               Instruction ("popq", "%r9", "", "");
               Instruction ("popq", "%r8", "", "");
               Instruction ("popq", "%rcx", "", "");
               Instruction ("popq", "%rdx", "", "");
               Instruction ("popq", "%rsi", "", "");
               Line ("\t#Call w/ args end for method" ^ meth);
             ])
      (* @ popargs *)
      @
      (***** TODO: finish void dispatch label and void dispatch err handling *****)
      [
        Instruction ("jmp", finish_void_dispatch_label, "", "");
        Line (void_dispatch_label ^ ":");
        Instruction ("movl", "$" ^ string_of_int tac.line, "%esi", "");
        Instruction ("movl", "$" ^ err_to_num ERR_VOID_DISPATCH, "%edi", "");
        Instruction ("call", "cool_error", "", "");
        Line (finish_void_dispatch_label ^ ":");
      ]
  | ClassId -> (
      (*add_var_addr tac.result;*)
      let result = get_var_addr tac.result class_name in
      let class_id = Hashtbl.find_opt class_id_map (*class_name*) tac.arg1 in
      match class_id with
      | Some class_id ->
          (* Printf.fprintf debug_file "\t#; Class Id of type %s is %d\n" class_name
            class_id; *)
          [ Instruction ("movq", Printf.sprintf "$%d" class_id, result, "") ]
      | None ->
          let class_name = get_var_addr tac.arg1 class_name in
          [
            Instruction ("movq", class_name, "%r13", "");
            Instruction ("movq", "0(%r13)", "%r13", "");
            Instruction ("movq", "%r13", result, "");
          ])
  | Comment -> [ Line ("#" ^ tac.arg1) ]
  | Label -> [ Line "\t#Label" ] @ [ Line (Printf.sprintf "%s:" tac.arg1) ]
  | Jmp -> [ Line "\t#Jump" ] @ [ Instruction ("jmp", tac.arg1, "", "") ]
  | Return ->
      [
        Line "\t#Return start";
        Instruction ("jmp", class_name ^ "." ^ cur_method ^ ".end", "", "");
        Line "\t#Return end";
      ]
  | LetNoInit ->
      (*add_var_addr tac.result;*)
      let result = get_var_addr tac.result class_name in
      let name = tac.arg2 in
      if name = "Bool" || name = "Int" || name = "String" then
        (* if name = "SELF_TYPE" then
          [
            Line "\t#Let No Init start";
            Instruction ("pushq", "%rax", "", "");
            Instruction ("pushq", "%rdi", "", "");
            Instruction ("movq", "16(%rdi)", "%r14", "");
            Instruction ("movq", "8(%r14)", "%r14", "");
            Instruction ("call", "*%r14", "", "");
            Instruction ("movq", "%rax", "%r11", "");
            Instruction ("popq", "%rdi", "", "");
            Instruction ("popq", "%rax", "", "");
            Instruction ("movq", "%r11", result, "");
            Line "\t#Let No Init end";
          ]
        else *)
        pushargs
        @ [
            Line "\t#Let No Init start";
            Instruction ("pushq", "%rax", "", "");
            Instruction ("pushq", "%rdi", "", "");
            Instruction ("call", tac.arg2 ^ "..new", "", "");
            Instruction ("movq", "%rax", "%r11", "");
            Instruction ("popq", "%rdi", "", "");
            Instruction ("popq", "%rax", "", "");
            Instruction ("movq", "%r11", result, "");
            Line "\t#Let No Init end";
          ]
        @ popargs
      else
        [
          Line "\t#Let No Init start";
          Instruction ("movq", "$0", result, "");
          Line "\t#Let No Init end";
        ]
      (* [
            (*Instruction ("movq", "$0", result, "");*)
            Line "\t#Let No Init start";
            Instruction ("pushq", "%rax", "", "");
            Instruction ("pushq", "%rdi", "", "");
            Instruction ("call", tac.arg2 ^ "..new", "", "");
            Instruction ("movq", "%rax", "%r11", "");
            Instruction ("popq", "%rdi", "", "");
            Instruction ("popq", "%rax", "", "");
            Instruction ("movq", "%r11", result, "");
            Line "\t#Let No Init end";
          ]  *)
  | Ident_Expr ident_name ->
      let val_addr = get_var_addr ident_name class_name in
      (*let attrs =*)
      (*match Hashtbl.find_opt class_attribute_map class_name with*)
      (*| Some v -> v*)
      (*| None ->*)
      (*Printf.fprintf stderr "Failed to find the attributes of class %s\n"*)
      (*class_name;*)
      (*assert false*)
      (*in*)
      (*match List.find_opt (fun f -> f.field_name = tac.result) attrs with*)
      (*| Some v ->*)
      (*let result = Printf.sprintf "%d(%%rdi)" ((v.index + 3) * 8) in*)
      (*[*)
      (*Line (Printf.sprintf "\t#Assigning with result %s" tac.result);*)
      (*Line "\t#Ident Expr (with attr assignment) start";*)
      (*Instruction ("movq", val_addr, "%rax", "");*)
      (*Instruction ("movq", "%rax", result, "");*)
      (*Line "\t#Ident Expr end";*)
      (*]*)
      (*| None ->*)
      (*Printf.fprintf out_file*)
      (*"#; Class %s does not have an attribute named %s \n" class_name*)
      (*tac.result;*)
      (*add_var_addr tac.result;*)
      let result = get_var_addr tac.result class_name in
      [
        Line "\t#Ident Expr start";
        Instruction ("movq", val_addr, "%rax", "");
        Instruction ("movq", "%rax", result, "");
        Line "\t#Ident Expr end";
      ]
  | Plus ->
      let arg1 = get_var_addr tac.arg1 class_name in
      let arg2 = get_var_addr tac.arg2 class_name in
      let result =
        (*match get_attribute class_name tac.result with*)
        (*| Some v -> Printf.sprintf "%d(%%rdi)" ((v.index + 3) * 8)*)
        (*| None ->*)
        (*(*add_var_addr tac.result;*)*)
        get_var_addr tac.result class_name
      in

      [
        Line "\t#Plus start";
        Instruction ("movq", arg1, "%rax", "");
        Instruction ("movq", "24(%rax)", "%rax", "");
        Instruction ("movq", arg2, "%r11", "");
        Instruction ("movq", "24(%r11)", "%r11", "");
        Instruction ("addl", "%r11d", "%eax", "");
      ]
      @ pushargs
      @ [
          Instruction ("pushq", "%rax", "", "");
          Instruction ("subq", "$8", "%rsp", "");
          Instruction ("call", "Int..new", "", "");
          Instruction ("movq", "%rax", "%r11", "");
          Instruction ("addq", "$8", "%rsp", "");
          Instruction ("popq", "%rax", "", "");
        ]
      @ popargs
      @ [
          Instruction ("movq", "%rax", "24(%r11)", "");
          Instruction ("movq", "%r11", result, "");
          Line "\t#Plus end";
        ]
  | Minus ->
      let arg1 = get_var_addr tac.arg1 class_name in
      let arg2 = get_var_addr tac.arg2 class_name in
      let result =
        (*match get_attribute class_name tac.result with*)
        (*| Some v -> Printf.sprintf "%d(%%rdi)" ((v.index + 3) * 8)*)
        (*| None ->*)
        (*(*add_var_addr tac.result;*)*)
        get_var_addr tac.result class_name
      in

      [
        Line "\t#Minus start";
        Instruction ("movq", arg1, "%rax", "");
        Instruction ("movq", "24(%rax)", "%rax", "");
        Instruction ("movq", arg2, "%r11", "");
        Instruction ("movq", "24(%r11)", "%r11", "");
        Instruction ("subl", "%r11d", "%eax", "");
      ]
      @ pushargs
      @ [
          Instruction ("pushq", "%rax", "", "");
          Instruction ("subq", "$8", "%rsp", "");
          Instruction ("call", "Int..new", "", "");
          Instruction ("movq", "%rax", "%r11", "");
          Instruction ("addq", "$8", "%rsp", "");
          Instruction ("popq", "%rax", "", "");
        ]
      @ popargs
      @ [
          Instruction ("movq", "%rax", "24(%r11)", "");
          Instruction ("movq", "%r11", result, "");
          Line "\t#Minus end";
        ]
  | Divide ->
      let arg1 = get_var_addr tac.arg1 class_name in
      let arg2 = get_var_addr tac.arg2 class_name in
      let result =
        (*match get_attribute class_name tac.result with*)
        (*| Some v -> Printf.sprintf "%d(%%rdi)" ((v.index + 3) * 8)*)
        (*| None ->*)
        (*(*add_var_addr tac.result;*)*)
        get_var_addr tac.result class_name
      in

      let error_label = get_label () in
      let div_end_label = get_label () in

      [
        Line "\t#Divide start";
        Instruction ("pushq", "%rdx", "", "");
        Instruction ("pushq", "%rcx", "", "");
        Instruction ("movq", arg2, "%rcx", "");
        Instruction ("movq", "24(%rcx)", "%rcx", "");
        Instruction ("testq", "%rcx", "%rcx", "");
        Instruction ("je", error_label, "", "");
        Instruction ("movq", arg1, "%rax", "");
        Instruction ("movq", "24(%rax)", "%rax", "");
        Instruction ("cltd", "", "", "");
        Instruction ("idivl", "%ecx", "", "");
      ]
      @ pushargs
      @ [
          Instruction ("pushq", "%rax", "", "");
          Instruction ("subq", "$8", "%rsp", "");
          Instruction ("call", "Int..new", "", "");
          Instruction ("movq", "%rax", "%r11", "");
          Instruction ("addq", "$8", "%rsp", "");
          Instruction ("popq", "%rax", "", "");
        ]
      @ popargs
      @ [
          Instruction ("movq", "%rax", "24(%r11)", "");
          Instruction ("movq", "%r11", result, "");
          Instruction ("popq", "%rcx", "", "");
          Instruction ("popq", "%rdx", "", "");
          Instruction ("jmp", div_end_label, "", "");
          Line (error_label ^ ":");
          Instruction ("movl", "$" ^ string_of_int tac.line, "%esi", "");
          Instruction ("movl", "$" ^ err_to_num ERR_DIV_BY_ZERO, "%edi", "");
          Instruction ("call", "cool_error", "", "");
          Line (div_end_label ^ ":");
          Line "\t#Divide end";
        ]
  | Times ->
      let arg1 = get_var_addr tac.arg1 class_name in
      let arg2 = get_var_addr tac.arg2 class_name in
      let result =
        (*match get_attribute class_name tac.result with*)
        (*| Some v -> Printf.sprintf "%d(%%rdi)" ((v.index + 3) * 8)*)
        (*| None ->*)
        (*(*add_var_addr tac.result;*)*)
        get_var_addr tac.result class_name
      in
      [
        Line "\t#Times start";
        Instruction ("movq", arg1, "%rax", "");
        Instruction ("movq", "24(%rax)", "%rax", "");
        Instruction ("movq", arg2, "%r11", "");
        Instruction ("movq", "24(%r11)", "%r11", "");
        Instruction ("imull", "%r11d", "%eax", "");
      ]
      @ pushargs
      @ [
          Instruction ("pushq", "%rax", "", "");
          Instruction ("subq", "$8", "%rsp", "");
          Instruction ("call", "Int..new", "", "");
          Instruction ("movq", "%rax", "%r11", "");
          Instruction ("addq", "$8", "%rsp", "");
          Instruction ("popq", "%rax", "", "");
        ]
      @ popargs
      @ [
          Instruction ("movq", "%rax", "24(%r11)", "");
          Instruction ("movq", "%r11", result, "");
          Line "\t#Times end";
        ]
  | LessThan ->
      let arg1 = get_var_addr tac.arg1 class_name in
      let arg2 = get_var_addr tac.arg2 class_name in
      let result =
        (*match get_attribute class_name tac.result with*)
        (*| Some v -> Printf.sprintf "%d(%%rdi)" ((v.index + 3) * 8)*)
        (*| None ->*)
        (*(*add_var_addr tac.result;*)*)
        get_var_addr tac.result class_name
      in
      [ Line "\t#Less Than start" ]
      @ pushargs
      @ [
          Instruction ("movq", arg1, "%rdi", "");
          Instruction ("movq", arg2, "%rsi", "");
          Instruction ("call", "lt_handler", "", "");
        ]
      @ popargs
      @ [ Instruction ("movq", "%rax", result, ""); Line "\t#Less Than end" ]
  | LessEqual ->
      let arg1 = get_var_addr tac.arg1 class_name in
      let arg2 = get_var_addr tac.arg2 class_name in
      let result =
        (*match get_attribute class_name tac.result with*)
        (*| Some v -> Printf.sprintf "%d(%%rdi)" ((v.index + 3) * 8)*)
        (*| None ->*)
        (*(*add_var_addr tac.result;*)*)
        get_var_addr tac.result class_name
      in
      [ Line "\t#Less Equal start" ]
      @ pushargs
      @ [
          Instruction ("movq", arg1, "%rdi", "");
          Instruction ("movq", arg2, "%rsi", "");
          Instruction ("call", "le_handler", "", "");
        ]
      @ popargs
      @ [ Instruction ("movq", "%rax", result, ""); Line "\t#Less Equal end" ]
  | Equal ->
      let arg1 = get_var_addr tac.arg1 class_name in
      let arg2 = get_var_addr tac.arg2 class_name in
      let result =
        (*match get_attribute class_name tac.result with*)
        (*| Some v -> Printf.sprintf "%d(%%rdi)" ((v.index + 3) * 8)*)
        (*| None ->*)
        (*(*add_var_addr tac.result;*)*)
        get_var_addr tac.result class_name
      in

      [ Line "\t#Equal start" ] @ pushargs
      @ [
          Instruction ("movq", arg1, "%rdi", "");
          Instruction ("movq", arg2, "%rsi", "");
          Instruction ("call", "eq_handler", "", "");
        ]
      @ popargs
      @ [ Instruction ("movq", "%rax", result, ""); Line "\t#Equal end" ]
  | Not ->
      let arg1 = get_var_addr tac.arg1 class_name in
      let result =
        (*match get_attribute class_name tac.result with*)
        (*| Some v -> Printf.sprintf "%d(%%rdi)" ((v.index + 3) * 8)*)
        (*| None ->*)
        (*(*add_var_addr tac.result;*)*)
        get_var_addr tac.result class_name
      in

      [ Line "\t#Not start" ] @ pushargs
      @ [
          Instruction ("movq", arg1, "%rax", "");
          Instruction ("movq", "24(%rax)", "%rax", "");
          Instruction ("testq", "%rax", "%rax", "");
          Instruction ("movl", "$1", "%eax", "");
          Instruction ("movl", "$0", "%r11d", "");
          Instruction ("cmovel", "%eax", "%r11d", "");
          Instruction ("pushq", "%r11", "", "");
          Instruction ("pushq", "%rdi", "", "");
          Instruction ("call", "Bool..new", "", "");
          Instruction ("popq", "%rdi", "", "");
          Instruction ("popq", "%r11", "", "");
          Instruction ("movq", "%r11", "24(%rax)", "");
          Instruction ("movq", "%rax", result, "");
        ]
      @ popargs @ [ Line "\t#Not end" ]
  | Negate ->
      let arg1 = get_var_addr tac.arg1 class_name in
      let result =
        (*match get_attribute class_name tac.result with*)
        (*| Some v -> Printf.sprintf "%d(%%rdi)" ((v.index + 3) * 8)*)
        (*| None ->*)
        (*(*add_var_addr tac.result;*)*)
        get_var_addr tac.result class_name
      in

      [ Line "\t#Negate start" ] @ pushargs
      @ [
          Instruction ("movq", arg1, "%rax", "");
          Instruction ("movq", "24(%rax)", "%rax", "");
          Instruction ("negq", "%rax", "", "");
          Instruction ("pushq", "%rax", "", "");
          Instruction ("pushq", "%rdi", "", "");
          Instruction ("call", "Int..new", "", "");
          Instruction ("movq", "%rax", "%r11", "");
          Instruction ("popq", "%rdi", "", "");
          Instruction ("popq", "%rax", "", "");
          Instruction ("movq", "%rax", "24(%r11)", "");
          Instruction ("movq", "%r11", result, "");
        ]
      @ popargs @ [ Line "\t#Negate end" ]
  | Int_Constant ->
      let result =
        (*match get_attribute class_name tac.result with*)
        (*| Some v -> Printf.sprintf "%d(%%rdi)" ((v.index + 3) * 8)*)
        (*| None ->*)
        (*(*add_var_addr tac.result;*)*)
        get_var_addr tac.result class_name
      in

      [
        Line "\t#iconst start";
        Instruction ("pushq", "%rsi", "", "");
        Instruction ("pushq", "%rdx", "", "");
        Instruction ("pushq", "%rcx", "", "");
        Instruction ("pushq", "%r8", "", "");
        Instruction ("pushq", "%r9", "", "");
        Instruction ("pushq", "%rdi", "", "");
        Instruction ("call", "Int..new", "", "");
        Instruction ("popq", "%rdi", "", "");
        Instruction ("movq", "$" ^ tac.arg1, "24(%rax)", "");
        Instruction ("popq", "%r9", "", "");
        Instruction ("popq", "%r8", "", "");
        Instruction ("popq", "%rcx", "", "");
        Instruction ("popq", "%rdx", "", "");
        Instruction ("popq", "%rsi", "", "");
        Instruction ("movq", "%rax", result, "");
        Line "\t#iconst end";
      ]
  | String_Constant -> (
      let result =
        (*match get_attribute class_name tac.result with*)
        (*| Some v -> Printf.sprintf "%d(%%rdi)" ((v.index + 3) * 8)*)
        (*| None ->*)
        (*(*add_var_addr tac.result;*)*)
        get_var_addr tac.result class_name
      in
      let str = transform_string tac.arg1 in
      match Hashtbl.find_opt string_map str with
      | Some str_id ->
          [ Line "\t#sconst start" ] @ pushargs
          @ [
              Instruction ("call", "String..new", "", "");
              Instruction
                ("movq", "$.string" ^ string_of_int str_id, "24(%rax)", "");
            ]
          @ popargs
          @ [ Instruction ("movq", "%rax", result, ""); Line "\t#sconst end" ]
      | None ->
          string_counter := !string_counter + 1;
          Hashtbl.add string_map str !string_counter;

          [ Line "\t#sconst start" ] @ pushargs
          @ [
              Instruction ("call", "String..new", "", "");
              Instruction
                ( "movq",
                  "$.string" ^ string_of_int !string_counter,
                  "24(%rax)",
                  "" );
            ]
          @ popargs
          @ [ Instruction ("movq", "%rax", result, ""); Line "\t#sconst end" ]
      (* This is all we do because they're emitted later :) *)
      (* This is the later but that's another stage; needs to be NOT just in an expression lm ao*)
      (*Printf.fprintf out_file "\t%s\n" (".secton\t.rodata");*)
      (*Printf.fprintf out_file "%s\n" ("string" ^ string_of_int(!string_counter) ^ ":");*)
      (*Printf.fprintf out_file "\t%s\n" (".string \"" ^ string_of_int(!string_counter) ^ "\"");*)
      (*Printf.fprintf out_file "\t%s\n" (".string \"" ^ string_of_int(!string_counter) ^ "\"")*)
      )
  | Boolean_Constant ->
      let result =
        (*match get_attribute class_name tac.result with*)
        (*| Some v -> Printf.sprintf "%d(%%rdi)" ((v.index + 3) * 8)*)
        (*| None ->*)
        (*(*add_var_addr tac.result;*)*)
        get_var_addr tac.result class_name
      in

      if tac.arg1 = "true" then
        [ Line "\t#bconst start" ] @ pushargs
        @ [
            Instruction ("call", "Bool..new", "", "");
            Instruction ("movq", "$1", "24(%rax)", "");
          ]
        @ popargs
        @ [ Instruction ("movq", "%rax", result, ""); Line "\t#bconst end" ]
      else
        [ Line "\t#bconst start" ] @ pushargs
        @ [
            Instruction ("call", "Bool..new", "", "");
            Instruction ("movq", "$0", "24(%rax)", "");
          ]
        @ popargs
        @ [ Instruction ("movq", "%rax", result, ""); Line "\t#bconst end" ]
  | Case_Header ->
      let arg = get_var_addr tac.arg1 class_name in
      let jump_to = tac.result in
      [
        Line "\t# Case header start";
        Instruction ("movq", arg, "%rax", "");
        Instruction ("testq", "%rax", "%rax", "");
        Instruction ("jz", jump_to, "", "");
        Line "\t# Case Header end";
      ]
  | Case jump ->
      let arg1 = get_var_addr tac.arg1 class_name in
      let arg2 = get_var_addr tac.arg2 class_name in
      [
        Instruction ("pushq", "%r13", "", "");
        Instruction ("pushq", "%r14", "", "");
        Instruction ("movq", arg1, "%r13", "");
        Instruction ("movq", arg2, "%r14", "");
        Instruction ("cmpq", "%r13", "%r14", "");
        Instruction ("popq", "%r14", "", "");
        Instruction ("popq", "%r13", "", "");
        Instruction ("je", jump, "", "");
      ]
  | EmptyCase ->
      [
        Line (Printf.sprintf "%s:" tac.arg1);
        Line "## case expression: error case";
        Instruction ("movl", "$" ^ string_of_int tac.line, "%esi", "");
        Instruction ("movl", "$" ^ err_to_num ERR_CASE_NO_BRANCH, "%edi", "");
        Instruction ("call", "cool_error", "", "");
      ]
  | VoidCase ->
      [
        Line (Printf.sprintf "%s:" tac.arg1);
        Line "## case expression: error case";
        Instruction ("movl", "$" ^ string_of_int tac.line, "%esi", "");
        Instruction ("movl", "$" ^ err_to_num ERR_VOID_CASE, "%edi", "");
        Instruction ("call", "cool_error", "", "");
      ]
  | Default ->
      let result =
        (*match get_attribute class_name tac.result with*)
        (*| Some v -> Printf.sprintf "%d(%%rdi)" ((v.index + 3) * 8)*)
        (*| None ->*)
        (*(*add_var_addr tac.result;*)*)
        (*get_var_addr tac.result class_name*)
        get_var_addr tac.result class_name
      in
      Printf.fprintf debug_file "# NEW: Adding var %s at position %s\n"
        tac.result result;
      let name = tac.arg1 in
      let new_call =
        if
          name = "Bool" || name = "IO" || name = "Int" || name = "Object"
          || name = "String"
        then
          if tac.arg1 = "SELF_TYPE" then
            [
              Instruction ("pushq", "%rdi", "", "");
              Instruction ("movq", "16(%rdi)", "%r14", "");
              Instruction ("movq", "8(%r14)", "%r14", "");
              Instruction ("call", "*%r14", "", "");
              Instruction ("popq", "%rdi", "", "");
              Instruction ("movq", "%rax", Printf.sprintf "%s" result, "");
            ]
          else
            [
              Instruction ("pushq", "%rdi", "", "");
              Instruction ("call", tac.arg1 ^ "..new", "", "");
              Instruction ("popq", "%rdi", "", "");
              Instruction ("movq", "%rax", Printf.sprintf "%s" result, "");
            ]
        else [ Instruction ("movq", "$0", result, "") ]
      in
      let pu =
        [
          Instruction ("pushq", "%rsi", "", "");
          Instruction ("pushq", "%rdx", "", "");
          Instruction ("pushq", "%rcx", "", "");
          Instruction ("pushq", "%r8", "", "");
          Instruction ("pushq", "%r9", "", "");
        ]
      in
      let po =
        [
          Instruction ("popq", "%r9", "", "");
          Instruction ("popq", "%r8", "", "");
          Instruction ("popq", "%rcx", "", "");
          Instruction ("popq", "%rdx", "", "");
          Instruction ("popq", "%rsi", "", "");
        ]
      in
      pu @ new_call @ po
  | New ->
      let result =
        (*match get_attribute class_name tac.result with*)
        (*| Some v -> Printf.sprintf "%d(%%rdi)" ((v.index + 3) * 8)*)
        (*| None ->*)
        (*(*add_var_addr tac.result;*)*)
        (*get_var_addr tac.result class_name*)
        get_var_addr tac.result class_name
      in
      Printf.fprintf debug_file "# NEW: Adding var %s at position %s\n"
        tac.result result;
      let new_call =
        if tac.arg1 = "SELF_TYPE" then
          [
            Instruction ("pushq", "%rdi", "", "");
            Instruction ("movq", "16(%rdi)", "%r14", "");
            Instruction ("movq", "8(%r14)", "%r14", "");
            Instruction ("call", "*%r14", "", "");
            Instruction ("popq", "%rdi", "", "");
            Instruction ("movq", "%rax", Printf.sprintf "%s" result, "");
          ]
        else
          [
            Instruction ("pushq", "%rdi", "", "");
            Instruction ("call", tac.arg1 ^ "..new", "", "");
            Instruction ("popq", "%rdi", "", "");
            Instruction ("movq", "%rax", Printf.sprintf "%s" result, "");
          ]
      in
      let pu =
        [
          Instruction ("pushq", "%rsi", "", "");
          Instruction ("pushq", "%rdx", "", "");
          Instruction ("pushq", "%rcx", "", "");
          Instruction ("pushq", "%r8", "", "");
          Instruction ("pushq", "%r9", "", "");
        ]
      in
      let po =
        [
          Instruction ("popq", "%r9", "", "");
          Instruction ("popq", "%r8", "", "");
          Instruction ("popq", "%rcx", "", "");
          Instruction ("popq", "%rdx", "", "");
          Instruction ("popq", "%rsi", "", "");
        ]
      in
      pu @ new_call @ po
  | Isvoid ->
      let result =
        (*match get_attribute class_name tac.result with*)
        (*| Some v -> Printf.sprintf "%d(%%rdi)" ((v.index + 3) * 8)*)
        (*| None ->*)
        (*(*add_var_addr tac.result;*)*)
        (*get_var_addr tac.result class_name*)
        get_var_addr tac.result class_name
      in
      Printf.fprintf debug_file "# Isvoid: Adding var %s at position %s\n"
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
      ]
      @ pushargs
      @ [ Instruction ("call", "Bool..new", "", "") ]
      @ popargs
      @ [
          Instruction ("movq", "%rax", Printf.sprintf "%s" result, "");
          Instruction ("jmp", Printf.sprintf "l%d" post_jump, "", "");
          Line true_jump_label;
          Line "\t #true branch of isvoid";
        ]
      @ pushargs
      @ [ Instruction ("call", "Bool..new", "", "") ]
      @ popargs
      @ [
          (* This works bc the bool created a few lines ago is stored in %r13, 24(%r13) is the third field of the bool (initially zero) *)
          Instruction ("movq", "$1", "24(%rax)", "");
          Instruction ("movq", "%rax", Printf.sprintf "%s" result, "");
          Instruction ("jmp", Printf.sprintf "l%d" post_jump, "", "");
          Line post_jump_label;
        ]

let print_start () =
  let start =
    let part1 =
      [
        Line ".globl start";
        Line "start:                  ## program begins here";
        Line ".globl main";
        Line ".type main, @function";
        Line "main:";
        (*Instruction ("movq", "$Main..new", "%r14", "");*)
        Instruction ("pushq", "%rbp", "", "");
        Instruction ("call", "Main..new", "", "");
        Instruction ("movq", "%rax", "%rdi", "");
      ]
    in
    let part2 =
      [
        Instruction ("movl", "$0", "%edi", "");
        Instruction ("call", "exit", "", "");
      ]
    in
    let mainfunc =
      List.find
        (fun func -> func.method_name = "main")
        (Hashtbl.find class_vtable_map "Main").methods
    in
    part1
    @ [
        Instruction
          ("call", mainfunc.type_name ^ "." ^ mainfunc.method_name, "", "");
      ]
    @ part2
  in
  List.iter print_asm start

let vtables =
  create_vtables ();
  let vtable_list =
    Hashtbl.fold (fun _ v acc -> v :: acc) class_vtable_map []
  in
  create_default_vtables () @ vtable_list

let bool_new =
  [
    Line "Bool..new:";
    Instruction ("subq", "$8", "%rsp", "");
    Instruction ("movl", "$4", "%esi", "");
    Instruction ("movl", "$8", "%edi", "");
    Instruction ("call", "calloc", "", "");
    Line "\t#Set class tag, object size, vtable pointer";
    Instruction ("movq", "$1", "(%rax)", "");
    Instruction ("movq", "$4", "8(%rax)", "");
    Instruction ("movq", "$Bool..vtable", "%r11", "");
    Instruction ("movq", "%r11", "16(%rax)", "");
    Instruction ("movq", "$0", "24(%rax)", "");
    Instruction ("addq", "$8", "%rsp", "");
    Instruction ("ret", "", "", "");
    (*Instruction (".size", "Bool..new", ".-Bool..new", "");*)
  ]

let io_new =
  [
    Line "IO..new:";
    Instruction ("subq", "$8", "%rsp", "");
    Instruction ("movl", "$3", "%esi", "");
    Instruction ("movl", "$8", "%edi", "");
    Instruction ("call", "calloc", "", "");
    Line "\t#Set class tag, object size, vtable pointer";
    Instruction ("movq", "$11", "(%rax)", "");
    Instruction ("movq", "$3", "8(%rax)", "");
    Instruction ("movq", "$IO..vtable", "%r11", "");
    Instruction ("movq", "%r11", "16(%rax)", "");
    Instruction ("addq", "$8", "%rsp", "");
    Instruction ("ret", "", "", "");
    (*Instruction (".size", "IO..new", ".-IO..new", "");*)
  ]

let int_new =
  [
    Line "Int..new:";
    Instruction ("subq", "$8", "%rsp", "");
    Instruction ("movl", "$4", "%esi", "");
    Instruction ("movl", "$8", "%edi", "");
    Instruction ("call", "calloc", "", "");
    Line "\t#Set class tag, object size, vtable pointer";
    Instruction ("movq", "$2", "(%rax)", "");
    Instruction ("movq", "$4", "8(%rax)", "");
    Instruction ("movq", "$Int..vtable", "%r11", "");
    Instruction ("movq", "%r11", "16(%rax)", "");
    Instruction ("movq", "$0", "24(%rax)", "");
    Instruction ("addq", "$8", "%rsp", "");
    Instruction ("ret", "", "", "");
    (*Instruction (".size", "Int..new", ".-Int..new", "");*)
  ]

let object_new =
  [
    Line "Object..new:";
    Instruction ("subq", "$8", "%rsp", "");
    Instruction ("movl", "$3", "%esi", "");
    Instruction ("movl", "$8", "%edi", "");
    Instruction ("call", "calloc", "", "");
    Line "\t#Set class tag, object size, vtable pointer";
    Instruction ("movq", "$13", "(%rax)", "");
    Instruction ("movq", "$3", "8(%rax)", "");
    Instruction ("movq", "$Object..vtable", "%r11", "");
    Instruction ("movq", "%r11", "16(%rax)", "");
    Instruction ("addq", "$8", "%rsp", "");
    Instruction ("ret", "", "", "");
    (*Instruction (".size", "Object..new", ".-Object..new", "");*)
  ]

let string_new =
  [
    Line "String..new:";
    Instruction ("pushq", "%rbx", "", "");
    Instruction ("movl", "$4", "%esi", "");
    Instruction ("movl", "$8", "%edi", "");
    Instruction ("call", "calloc", "", "");
    Instruction ("movl", "$1", "%edi", "");
    Instruction ("movq", "%rax", "%rbx", "");
    Line "\t#Set class tag, object size, vtable pointer";
    Instruction ("movq", "$5", "(%rax)", "");
    Instruction ("movq", "$4", "8(%rax)", "");
    Instruction ("movl", "$String..vtable", "%eax", "");
    Instruction ("movq", "%rax", "16(%rbx)", "");
    Instruction ("call", "malloc", "", "");
    Instruction ("movq", "%rax", "24(%rbx)", "");
    Instruction ("movb", "$0", "(%rax)", "");
    Instruction ("movq", "%rbx", "%rax", "");
    Instruction ("popq", "%rbx", "", "");
    Instruction ("ret", "", "", "");
  ]

let new_funcs =
  (* Generate assembly code for Class..new functions*)
  let generate_class_new_asm asm_class_var =
    let func_name = Printf.sprintf "%s..new:" asm_class_var.vtable.name_id in
    let class_id = Printf.sprintf "$%d" asm_class_var.class_id in
    let object_size = Printf.sprintf "$%d" (3 + asm_class_var.object_size) in
    let vtable_name =
      Printf.sprintf "$%s..vtable" asm_class_var.vtable.name_id
    in
    let stack_room = ref 16 in
    let attr_creation_lines =
      List.map
        (* 
              Instruction ("pushq", "%rdi", "", "");
              Instruction ("movq", "16(%rdi)", "%r14", "");
              Instruction ("movq", "8(%r14)", "%r14", "");
              Instruction ("call", "*%r14", "", "");
              Instruction ("popq", "%rdi", "", "");
              Instruction ("movq", "%rax", Printf.sprintf "%s" result, "");

           *)
        (fun (attr : attribute) : asm_line list ->
          let var_index = 3 + attr.index in
          let type_new = Printf.sprintf "%s..new" attr.type_name in
          let stack_location = Printf.sprintf "%d(%%rdi)" (8 * var_index) in
          if
            attr.type_name = "Bool" || attr.type_name = "Int"
            || attr.type_name = "String"
          then
            [
              Line "\t#Attr No Init start";
              Line
                (Printf.sprintf "\t## self[%d] holds field %s : %s" var_index
                   attr.field_name attr.type_name);
              Line (Printf.sprintf "\t## new %s" attr.type_name);
              Instruction ("pushq", "%rbp", "", "");
              Instruction ("pushq", "%rdi", "", "");
              Instruction ("call", type_new, "", "");
              Instruction ("movq", "%rax", "%r13", "");
              Instruction ("popq", "%rdi", "", "");
              Instruction ("popq", "%rbp", "", "");
              Instruction ("movq", "%r13", stack_location, "");
              Line "\t#Attr No Init end";
            ]
          else
            [
              Line "\t#Attr No Init start";
              Instruction ("movq", "$0", stack_location, "");
              Line "\t#Attr No Init end";
            ])
        asm_class_var.attributes
      |> List.flatten
    in
    let attr_init_lines =
      List.map
        (fun (attr : attribute) : asm_line list ->
          match attr.expression with
          | Some (node, retval) ->
              stack_room := max !stack_room node.temp_count;
              Hashtbl.reset var_locations;
              (*add_var_addr retval;*)
              let ret = get_var_addr retval asm_class_var.vtable.name_id in
              let stack_location =
                Printf.sprintf "%d(%%rdi)" (8 * (attr.index + 3))
              in
              [
                Instruction ("pushq", "%r13", "", "");
                Instruction ("pushq", "%rdi", "", "");
              ]
              @ (List.map
                   (fun tac ->
                     let t =
                       tac_to_as tac attr.field_name
                         asm_class_var.vtable.name_id !prev_tac
                     in
                     prev_tac := tac;
                     t)
                   (Cfg.get_method_tac node.cfg)
                |> List.flatten)
              @ [
                  Instruction ("movq", ret, "%r13", "");
                  Instruction ("popq", "%rdi", "", "");
                  Instruction ("movq", "%r13", stack_location, "");
                  Instruction ("popq", "%r13", "", "");
                ]
          | None -> [])
        asm_class_var.attributes
      |> List.flatten
    in
    let return_lines =
      [
        Instruction ("movq", "%rdi", "%rax", "");
        Instruction ("movq", "%rbp", "%rsp", "");
        Instruction ("popq", "%rbp", "", "");
        Instruction ("ret", "", "", "");
      ]
    in
    let stack_space =
      if !stack_room mod 2 != 0 then (!stack_room + 1) * 8 else !stack_room * 8
    in
    ( asm_class_var.vtable.name_id,
      [
        (* Name *)
        Line func_name;
        Line
          (Printf.sprintf "## constructor for %s" asm_class_var.vtable.name_id);
        (* Set stack pointer (make room for temporaries *)
        Instruction ("pushq", "%rbp", "", "");
        Instruction ("movq", "%rsp", "%rbp", "");
        Line "\t## stack room for temporaries: ?";
        Instruction ("subq", "$" ^ string_of_int stack_space, "%rsp", "");
        Line "\t## return address handling";
        (*  Allocate space for the variable (rsi*rdi)=num*bytes *)
        Instruction ("movq", "$8", "%rsi", "");
        Instruction ("movq", object_size, "%rdi", "");
        Instruction ("call", "calloc", "", "");
        (* NOTE: Alot of these can be simplified to one line operations *)
        Line "\t## store class tag, object size and vtable pointer";
        Instruction ("movq", "%rax", "%rdi", "");
        Instruction ("movq", class_id, "0(%rdi)", "");
        Instruction ("movq", object_size, "%r14", "");
        Instruction ("movq", "%r14", "8(%rdi)", "");
        Instruction ("movq", vtable_name, "%r14", "");
        Instruction ("movq", "%r14", "16(%rdi)", "");
        Line "\t## return address handling";
        Line "\t## ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;";
        Line "\t## initialize attributes";
      ]
      @ attr_creation_lines @ attr_init_lines @ return_lines )
  in
  let asm_classes : asm_class list =
    List.filter_map make_asm_class parser_class_map
  in
  [
    ("Bool", bool_new);
    ("IO", io_new);
    ("Int", int_new);
    ("Object", object_new);
    ("String", string_new);
  ]
  @ List.map generate_class_new_asm asm_classes

(* A list of (the assembly code for) methods *)
let get_method_asm cfg_param =
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
  in
  let get_end_method_boilerplate class_name method_name =
    let ret =
      (*match get_attribute class_name !prev_tac.result with*)
      (*| Some v -> Printf.sprintf "%d(%%rdi)" ((v.index + 3) * 8)*)
      (*| None -> get_var_addr !prev_tac.result class_name*)
      get_var_addr !prev_tac.result class_name
    in
    let retline =
      Printf.sprintf "#; The return of %s.%s is %s\n" class_name method_name ret
    in
    [
      Line (Printf.sprintf "%s.%s.end:" class_name method_name);
      Line retline;
      Instruction ("movq", ret, "%rax", "");
      Instruction ("movq", "%rbp", "%rsp", "");
      Instruction ("popq", "%rbp", "", "");
      Instruction ("ret", "", "", "");
    ]
  in
  List.map
    (fun (graph : Cfg.cfg) ->
      Hashtbl.reset var_locations;
      Hashtbl.reset arg_map;
      (*Printf.printf "new method!!!\n";*)
      let gen_register_arglist args =
        let registers = [ "%rsi"; "%rdx"; "%rcx"; "%r8"; "%r9" ] in
        List.mapi
          (fun i arg ->
            Hashtbl.add arg_map arg (i - 6);
            Printf.fprintf debug_file
              "\t#; Placing var %s in register %d (%s)\n" arg i
              (List.nth registers i);
            Instruction
              ( "movq",
                get_var_addr arg graph.class_name,
                List.nth registers i,
                "" ))
          args
      in
      let gen_mixed_arglist args =
        let first_five = List.filteri (fun i _ -> i <= 4) args in
        let remaining = List.filteri (fun i _ -> i > 4) args in
        List.iteri
          (fun i arg ->
            Printf.fprintf debug_file
              "\t#; Adding var %s as position %d(%%rbp)\n" arg
              (8 * (i + 5));
            Hashtbl.add arg_map arg (8 * (i + 2)))
          remaining;
        gen_register_arglist first_five
      in

      let args =
        List.map (fun (arg : ast_formal) -> arg.name.name) graph.arguments
      in
      (* While there ARE 6 argument registers in the SysV convention, we are dedicating rdi to always be the self pointer *)
      let arglist =
        if List.length args <= 5 then gen_register_arglist args
        else gen_mixed_arglist args
      in
      Printf.fprintf debug_file "#; %s.%s:\n" graph.class_name graph.method_name;
      List.iteri
        (fun i (arg : ast_formal) ->
          Printf.fprintf debug_file "\t#; Argument %d: %s\n" i arg.name.name)
        graph.arguments;
      let temps = graph.temp_count in
      let stack_space =
        if temps * 8 mod 16 != 0 then (temps + 1) * 8 else temps * 8
      in
      let method_tac = Cfg.get_method_tac graph.cfg in
      let asms =
        List.map
          (fun tac ->
            let t =
              tac_to_as tac graph.method_name graph.class_name !prev_tac
            in
            prev_tac := tac;
            t)
          method_tac
        |> List.flatten
      in
      get_start_method_boilerplate graph.method_name graph.class_name
        stack_space
      @ arglist @ asms
      @ get_end_method_boilerplate graph.class_name graph.method_name)
    cfg_param

let tac_list_to_asm lst = List.map tac_to_as lst
