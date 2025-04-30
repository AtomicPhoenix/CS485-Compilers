let file_name = Sys.argv.(1)
let file = open_in file_name
let base_file_name = String.sub file_name 0 (String.length file_name - 8)
let out_file = open_out (base_file_name ^ ".s")
let debug_file = open_out "/dev/null"

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

type static_type =
  | Class of string (* "Int" or "Object" *)
  | SELF_TYPE of string (* "Self_Type_c" *)

and identifier = { line_num : int; name : string }

and sub_expr =
  | Assignment of identifier * expr  (** var, rhs *)
  | Dynamic_Dispatch of expr * identifier * expr list  (** e, method, args*)
  | Static_Dispatch of expr * identifier * identifier * expr list
      (** e, type, method, args *)
  | Self_Dispatch of identifier * expr list  (** method, args *)
  | If of expr * expr * expr  (** pred, then, body *)
  | While of expr * expr  (** pred, body **)
  | Block of expr list  (** body *)
  | New of identifier  (** class *)
  | Isvoid of expr  (** exp *)
  | Plus of expr * expr  (** arith, x, y*)
  | Minus of expr * expr
  | Divide of expr * expr
  | Times of expr * expr
  | LessThan of expr * expr
  | LessEqual of expr * expr
  | Equal of expr * expr
  | Not of expr  (** x: exp *)
  | Negate of expr  (** x : exp *)
  | Int_Constant of int  (** int *)
  | String_Constant of string  (** str *)
  | Ident_Expr of identifier  (** id*)
  | Boolean_Constant of bool_val  (** bool_val *)
  | Let_Expr of (identifier * identifier * expr option) list * expr
      (** (variable, type, value option) list, body*)
  | Case of expr * case_el list  (** expr, case-element-list *)
  | Internal of
      string * string * static_type (* Classname, Methodname, Returntype *)

and bool_val = True | False
and case_el = { variable : identifier; typename : identifier; elem_body : expr }

and expr = {
  id : identifier;
  sub_expr : sub_expr;
  static_type : static_type option;
}

and imp_formal = { name : string }
and ast_formal = { name : identifier; formal_type : identifier }

and ast_attribute = {
  name : string;
  type_name : string;
  attr_expr : expr option;
}

and imp_method = {
  name : string;
  method_formals : imp_formal list;
  type_name : string;
  method_expr : expr;
}

(* | Method of identifier * formal list * identifier * expr *)
and ast_method = {
  name : string;
  method_formals : ast_formal list;
  type_name : string;
  method_expr : expr;
}

and class_map_elem = { name : string; attrs : ast_attribute list }
and implementation_map_elem = { name : string; methods : imp_method list }
and parent_map_elem = { child_name : string; parent_name : string }

and annotated_ast_elem = {
  class_name : identifier;
  inherits : identifier option;
  features : feature list;
}

and feature =
  | Attribute of (identifier * identifier * expr option)
      (** name (identifier), type (identifier), and assignment (expr option, to
          cover no init) *)
  | Method of (identifier * ast_formal list * identifier * expr)
      (** name (identifier), formal list (formal list), type (identifier), and
          body (expression) *)

(* 
    Output class_map\n.
    Output the number of classes and then \n.
    Output each class in turn (in ascending alphabetical order):
        Output the name of the class and then \n.
        Output the number of attributes and then \n.
        Output each attribute in turn (in order of appearance, with inherited attributes from a superclass coming first):
            Output no_initializer \n and then the attribute name \n and then the type name \n.
            or Output initializer \n and then the attribute name \n and then the type name \n and then the initializer expression.
*)

let rec get_identifier () : identifier =
  let linenum = read_int () in
  let name = read () in
  { line_num = linenum; name }

(* 
    Output the line number of the expression and then a newline (as in the .cl-ast format).
    Output the name of type associated with the expression and then a newline. For example, the expression 3+x is associated with the type Int. This is not required for any of the checkpoints for PA2, only in the final version of PA2.
    Output the name of the expression and then a newline and then any subparts (as in the .cl-ast format).
*)

(** Read in an expression *)
and get_expression () : expr =
  let line_num = read_int () in
  let static_type = Some (Class (read ())) in
  let name = read () in
  let id = { line_num; name } in
  { id; sub_expr = get_sub_expr id; static_type }

(** read in a list of expressions for a class *)
and get_expression_list () =
  List.init (read_int ()) (fun _ -> get_expression ())

(** Get the variable part of an expression (the non-identifier) *)
and get_sub_expr name =
  match name.name with
  | "assign" ->
      let i = get_identifier () in
      let e = get_expression () in
      Assignment (i, e)
  | "dynamic_dispatch" ->
      let e = get_expression () in
      let i = get_identifier () in
      let el = get_expression_list () in
      Dynamic_Dispatch (e, i, el)
  | "static_dispatch" ->
      let e = get_expression () in
      let typename = get_identifier () in
      let methodname = get_identifier () in
      let el = get_expression_list () in
      Static_Dispatch (e, typename, methodname, el)
  | "self_dispatch" ->
      let i = get_identifier () in
      let el = get_expression_list () in
      Self_Dispatch (i, el)
  | "if" ->
      let e1 = get_expression () in
      let e2 = get_expression () in
      let e3 = get_expression () in
      If (e1, e2, e3)
  | "while" ->
      let e1 = get_expression () in
      let e2 = get_expression () in
      While (e1, e2)
  | "block" ->
      let res = get_expression_list () in
      Block res
  | "new" -> New (get_identifier ())
  | "isvoid" -> Isvoid (get_expression ())
  | "plus" ->
      let e1 = get_expression () in
      let e2 = get_expression () in
      Plus (e1, e2)
  | "minus" ->
      let e1 = get_expression () in
      let e2 = get_expression () in
      Minus (e1, e2)
  | "times" ->
      let e1 = get_expression () in
      let e2 = get_expression () in
      Times (e1, e2)
  | "divide" ->
      let e1 = get_expression () in
      let e2 = get_expression () in
      Divide (e1, e2)
  | "lt" ->
      let e1 = get_expression () in
      let e2 = get_expression () in
      LessThan (e1, e2)
  | "le" ->
      let e1 = get_expression () in
      let e2 = get_expression () in
      LessEqual (e1, e2)
  | "eq" ->
      let e1 = get_expression () in
      let e2 = get_expression () in
      Equal (e1, e2)
  | "not" -> Not (get_expression ())
  | "negate" -> Negate (get_expression ())
  | "integer" -> Int_Constant (read_int ())
  | "string" ->
      let str = read () in
      String_Constant str
  | "identifier" -> Ident_Expr (get_identifier ())
  | "true" -> Boolean_Constant True
  | "false" -> Boolean_Constant False
  | "let" ->
      let expl = get_base_let_list () in
      let exp_body = get_expression () in
      Let_Expr (expl, exp_body)
  | "case" ->
      let e = get_expression () in
      Case (e, get_case_element_list ())
  | "internal" -> Internal ("TODO", "TODO", Class (read ()))
  | _ -> assert false

(** Read in a case element *)
and get_case_element () =
  let var = get_identifier () in
  let typ = get_identifier () in
  let exp = get_expression () in
  { variable = var; typename = typ; elem_body = exp }

and get_base_let_list () = List.init (read_int ()) (fun _ -> get_base_let ())

and get_case_element_list () =
  try List.init (read_int ()) (fun _ -> get_case_element ())
  with _ -> raise (Invalid_argument "Not a num")

(** Read in an uninitialized let binding *)
and get_base_let () =
  let binding = read () in
  let name = get_identifier () in
  let typename = get_identifier () in
  let let_expr =
    match binding with
    | "let_binding_no_init" -> None
    | "let_binding_init" -> Some (get_expression ())
    | _ -> assert false
  in
  (name, typename, let_expr)

and get_attr () =
  let attr_type = read () in
  let attr_name = read () in
  let type_name = read () in
  let attr_expr =
    match attr_type with
    | "no_initializer" -> None
    | "initializer" -> Some (get_expression ())
    | _ -> assert false
  in
  { name = attr_name; type_name; attr_expr }

and get_attrs () = List.init (read_int ()) (fun _ -> get_attr ())

and get_formal () =
  let name = read () in
  { name }

and get_formal_list () = List.init (read_int ()) (fun _ -> get_formal ())

(* 
    Output the method name and then \n.
    Output the number of formals and then \n.
    Output each formal’s name only:
        Output the name and then \n
    If this method is inherited from a parent class and not overriden, output the name of the ultimate parent class that defined the method body expression and then \n. Otherwise, output the name of the current class and then \n.
    Output the method body expression.
*)
let parse_class_map () : class_map_elem list =
  let _ = read () in
  let get_class () =
    let name = read () in
    let attrs = get_attrs () in
    { name; attrs }
  in
  let classes = List.init (read_int ()) (fun _ -> get_class ()) in
  classes
(*
The Implementation Map
    Output implementation_map\n.
    Output the number of classes and then \n.
    Output each class in turn (in ascending alphabetical order):
        Output the name of the class and then \n.
        Output the number of methods for that class and then \n.
        Output each method in turn (in order of appearance, with inherited or overridden methods from a superclass coming first; internal methods are defined to appear in ascending alphabetical order):
            Output the method name and then \n.
            Output the number of formals and then \n.
            Output each formal’s name only:
                Output the name and then \n
            If this method is inherited from a parent class and not overriden, output the name of the ultimate parent class that defined the method body expression and then \n. Otherwise, output the name of the current class and then \n.
            Output the method body expression.
*)

and parse_implementation_map () =
  let _ = read () in
  let get_method () =
    let name = read () in
    let method_formals = get_formal_list () in
    let type_name = read () in
    let method_expr = get_expression () in
    { name; method_formals; type_name; method_expr }
  in
  let get_methods () = List.init (read_int ()) (fun _ -> get_method ()) in
  let get_class () =
    let name = read () in
    let methods = get_methods () in
    { name; methods }
  in
  (* Read number of classes, and then that many classes *)
  List.init (read_int ()) (fun _ -> get_class ())

and parse_parent_map () =
  let _ = read () in
  List.init (read_int ()) (fun _ ->
      let child = read () in
      let parent = read () in
      { child_name = child; parent_name = parent })

(*
and annotated_ast_elem = {
  class_name : identifier;
  inherits : string;
  features : feature list;
}
        *)
and parse_annotated_ast () : annotated_ast_elem list =
  let rec get_class () =
    let class_name = get_identifier () in
    let does_inherit = read () in
    let inherits =
      if does_inherit = "no_inherits" then None else Some (get_identifier ())
    in
    let features = get_feature_list () in
    { class_name; inherits; features }
  (* Get a feature (method or attribute) *)
  and get_formal () =
    let name = get_identifier () in

    let formal_type = get_identifier () in
    { name; formal_type }
  and get_formal_list () = List.init (read_int ()) (fun _ -> get_formal ())
  (* and get_feature (class_name : string) : feature = *)
  and get_feature () : feature =
    let feat_type = read () in
    match feat_type with
    | "method" ->
        let name = get_identifier () in
        let method_formals = get_formal_list () in
        let type_name = get_identifier () in
        let method_expr = get_expression () in
        Method (name, method_formals, type_name, method_expr)
    | attr_type ->
        let attr_name = get_identifier () in
        let type_name = get_identifier () in
        let attr_expr =
          match attr_type with
          | "attribute_init" -> Some (get_expression ())
          | _ -> None
        in
        Attribute (attr_name, type_name, attr_expr)
  (* read in a list of features for a class *)
  and get_feature_list () =
    List.init (read_int ()) (fun _ ->
        let f = get_feature () in
        f)
  in
  List.init (read_int ()) (fun _ -> get_class ())
  |> List.sort (fun el1 el2 ->
         String.compare el1.class_name.name el2.class_name.name)

let parser_class_map = parse_class_map ()
let implementation_map = parse_implementation_map ()
let parent_map = parse_parent_map ()
let annotated_ast = parse_annotated_ast ()

(* TAC Element *)
type tac_elem = {
  operand : tac_operand;
  arg1 : string;
  arg2 : string;
  result : string;
  line : int;
  static_type : static_type option;
}

(* TAC Operands *)
and tac_operand =
  | Assignment
  | Bt
  | Call
  | StaticCall of string
  | Comment
  | Label
  | Jmp
  | Default
  | Case of string
  | VoidCase
  | EmptyCase
  | Return
  | LetNoInit
  | Ident_Expr of string
  | New
  | Isvoid
  | Plus
  | Minus
  | Divide
  | Times
  | LessThan
  | LessEqual
  | Equal
  | Not
  | Negate
  | Int_Constant
  | String_Constant
  | Boolean_Constant
  | ClassId

(* Counter for temp values *)
let var_ctr = ref 0

(* Counter for labels *)
let label_ctr = ref 0

(* Clas Map *)
let class_map = Hashtbl.create 32

(* Defined Variables at a given point in execution *)
let letTable = Hashtbl.create 32

(** Creates a list of all ancestors of a cool class*)
let rec get_ancestors (name : string) acc =
  let c = Hashtbl.find_opt class_map name in
  match c with
  | Some ast_elem -> (
      let acc = ast_elem :: acc in
      match ast_elem.inherits with
      | Some parent -> get_ancestors parent.name acc
      | None -> acc)
  | None -> []

(** Add a class to the class map *)
and add_class (ast_elem : annotated_ast_elem) =
  let name = ast_elem.class_name.name in
  match Hashtbl.find_opt class_map name with
  | None -> Hashtbl.add class_map name ast_elem
  | Some _ -> ()

(* Get the methods of a class *)
let get_all_methods (ast_elem : annotated_ast_elem) =
  let ancestors = get_ancestors ast_elem.class_name.name [] in
  (* Gets all methods of ancestors in ancestry order -> alphabetical order *)
  (* Compare features sorts all methods first by class (starting with inherited methods) then alphabetically within each class *)
  let get_feature_name feat =
    match feat with Attribute (id, _, _) | Method (id, _, _, _) -> id.name
  in
  let remove_duplicates lst =
    let latest_feature_map = Hashtbl.create (List.length lst) in
    List.iter
      (fun pair ->
        let feat, _ = pair in
        let name = get_feature_name feat in
        Hashtbl.replace latest_feature_map name pair)
      lst;
    let filtered_list = ref [] in
    let seen_names_in_order = Hashtbl.create (List.length lst) in
    List.iter
      (fun pair ->
        let feat, _ = pair in
        let name = get_feature_name feat in
        if not (Hashtbl.mem seen_names_in_order name) then
          match Hashtbl.find_opt latest_feature_map name with
          | Some latest_pair ->
              filtered_list := latest_pair :: !filtered_list;
              Hashtbl.add seen_names_in_order name true
          | None -> () (* Should not happen, but just in case *))
      lst;
    List.rev !filtered_list (* Reverse to maintain original first-seen order *)
  in
  List.map
    (fun ast_elem ->
      List.filter_map
        (fun feat ->
          match feat with
          | Method (id, f1, id2, exp) ->
              Some (Method (id, f1, id2, exp), ast_elem)
          | _ -> None)
        ast_elem.features)
    ancestors
  |> List.flatten |> remove_duplicates

(* Convert tac operand into a string *)
let operand_to_string (operand : tac_operand) : string =
  match operand with
  | Assignment -> "assignment"
  | Bt -> "bt"
  | Call -> "call"
  | StaticCall s -> Printf.sprintf "StaticCall (%s)" s
  | Case jump -> Printf.sprintf "jump to :%s after comparison of" jump
  | VoidCase -> "VoidCase"
  | EmptyCase -> "EmptyCase"
  | Comment -> "comment"
  | Label -> "label"
  | Jmp -> "jmp"
  | Default -> "default"
  | Return -> "return"
  | LetNoInit -> "letnoinit"
  | Ident_Expr v -> v
  | New -> "new"
  | Isvoid -> "isvoid"
  | Plus -> "+"
  | Minus -> "-"
  | Divide -> "/"
  | Times -> "*"
  | LessThan -> "<"
  | LessEqual -> "<="
  | Equal -> "equal"
  | Not -> "not"
  | Negate -> "negate"
  | Int_Constant -> "int"
  | String_Constant -> "string"
  | Boolean_Constant -> "bool"
  | ClassId -> "classId"

(* get tac string in a form that is printable in an assembly file *)
let get_tac_elem_commented t =
  match t.operand with
  | Label -> Printf.sprintf "#;label %s" t.arg1
  | Jmp -> Printf.sprintf "#;jmp %s" t.arg1
  | Return -> Printf.sprintf "#;return %s" t.arg1
  | Comment -> Printf.sprintf "#;comment %s" t.arg1
  | Bt -> Printf.sprintf "#;bt %s %s" t.arg1 t.arg2
  | Assignment -> Printf.sprintf "#;%s <- %s" t.result t.arg1
  | LetNoInit -> Printf.sprintf "#;%s <- %s %s" t.result t.arg1 t.arg2
  | String_Constant -> Printf.sprintf "#;%s <- %s" t.result t.arg1
  | Case c -> Printf.sprintf "#Cmp %s, %s -> jump to %s" t.arg1 t.arg2 c
  | VoidCase -> Printf.sprintf "#VoidCase: %s" t.arg1
  | EmptyCase -> Printf.sprintf "#EmptyCase: %s" t.arg1
  | _ ->
      if t.arg2 = "" && t.arg1 = "" then
        Printf.sprintf "#;%s <- %s" t.result (operand_to_string t.operand)
      else if t.arg2 = "" then
        Printf.sprintf "#;%s <- %s %s" t.result
          (operand_to_string t.operand)
          t.arg1
      else
        Printf.sprintf "#;%s <- %s %s %s" t.result
          (operand_to_string t.operand)
          t.arg1 t.arg2

let print_tac_elem_commented t =
  Printf.fprintf out_file "%s\n" (get_tac_elem_commented t)

let print_tac_elems (t : tac_elem list) = List.iter print_tac_elem_commented t

(* Get the different cases for a case statement according to the operational semantics of case *)
(* Return a list of (string * string) (class_name * the case it corresponds to) *)
let rec get_cases cases cname mname =
  (* Get list of classes in program *)
  let class_list =
    Hashtbl.fold (fun k v acc -> (k, v) :: acc) class_map []
    |> List.sort (fun (k, _) (k2, _) -> compare k k2)
  in
  label_ctr := !label_ctr + 1;
  let empty_jump = get_label !label_ctr mname cname in
  let jump_points =
    ("emptycase", empty_jump)
    :: List.map
         (fun case ->
           label_ctr := !label_ctr + 1;
           let case_label = get_label !label_ctr mname cname in
           (case, case_label))
         cases
  in
  List.iter
    (fun (name, label) -> Printf.fprintf out_file "#Jump %s: %s\n" name label)
    jump_points;
  let get_case (class_name, _) =
    let ancestors =
      List.map (fun k -> k.class_name.name) (get_ancestors class_name [])
    in
    let ancestors =
      (match List.find_opt (fun ancestor -> ancestor = "Object") ancestors with
      | Some _ -> ancestors
      | None -> "Object" :: ancestors)
      |> List.rev
    in
    let rec get_matching_case lst =
      match lst with
      | hd :: _ when List.find_opt (fun case -> case = hd) cases <> None ->
          Some (List.find (fun case -> case = hd) cases)
      | _ :: tail -> get_matching_case tail
      | [] -> None
    in
    let mtch =
      match get_matching_case ancestors with Some v -> v | None -> "emptycase"
    in
    let label = snd (List.find (fun (name, _) -> name = mtch) jump_points) in
    Printf.fprintf out_file "#Class: %s\n" class_name;
    Printf.fprintf out_file "#\tAncestors: ";
    List.iter
      (fun ancestor -> Printf.fprintf out_file "%s, " ancestor)
      ancestors;
    Printf.fprintf out_file "\n";
    Printf.fprintf out_file "#\tMatching case: %s\n" mtch;
    Printf.fprintf out_file "#\tMatching label: %s\n" label;
    (class_name, label)
  in
  List.map get_case class_list

(* Converts AST to TAC *)
and ast_to_tac (ast : annotated_ast_elem list) :
    (tac_elem list * string * string * ast_formal list * int) list =
  let get_tac_elem (ast_elem : annotated_ast_elem) =
    List.filter_map
      (fun (feat, _) ->
        match feat with
        | Method (method_name, arguments, _, exp) ->
            (* Printf.printf "Parsing expression: %s in method %s in class %s\n"
              exp.id.name id1.name ast_elem.class_name.name;  *)
            var_ctr := 0;
            label_ctr := 0;
            Hashtbl.reset letTable;
            let base_lst =
              [
                {
                  operand = Comment;
                  arg1 = "start";
                  arg2 = "";
                  result = "";
                  line = exp.id.line_num;
                  static_type = exp.static_type;
                };
                {
                  operand = Label;
                  arg1 =
                    ast_elem.class_name.name ^ "_" ^ method_name.name ^ "_0";
                  arg2 = "";
                  result = "";
                  line = exp.id.line_num;
                  static_type = exp.static_type;
                };
              ]
            in
            let exp_list =
              exp_to_tac exp (get_id !var_ctr) ast_elem.class_name.name
                method_name.name
            in
            let return_val = (List.rev exp_list |> List.hd).result in
            let rtrn =
              [
                {
                  operand = Return;
                  arg1 = return_val;
                  arg2 = "";
                  result = return_val;
                  line = 0;
                  static_type = exp.static_type;
                };
              ]
            in
            let temps = !var_ctr + 1 in
            Some
              ( base_lst @ exp_list @ rtrn,
                ast_elem.class_name.name,
                method_name.name,
                arguments,
                temps )
        | Attribute _ -> None)
      (get_all_methods ast_elem)
  in
  List.map get_tac_elem ast |> List.flatten

and get_bool bool_val = match bool_val with True -> "true" | False -> "false"
and get_id n = "t$" ^ string_of_int n

and get_label n class_name method_name =
  class_name ^ "_" ^ method_name ^ "_" ^ string_of_int n

and exp_to_tac (exp : expr) result cname mname : tac_elem list =
  match exp.sub_expr with
  | Assignment (id, exp) ->
      let var_id = Hashtbl.find_opt letTable id.name in
      let exp_res = match var_id with Some v -> v | None -> id.name in
      let last_var = exp_to_tac exp exp_res cname mname in

      Printf.fprintf out_file "#; id.name: %s\n" id.name;
      Printf.fprintf out_file "#; exp_res: %s\n" exp_res;
      Printf.fprintf out_file "#; result: %s\n" result;
      last_var
      @ [
          {
            operand = Ident_Expr exp_res;
            arg1 = "";
            arg2 = "";
            result;
            line = exp.id.line_num;
            static_type = exp.static_type;
          };
        ]
  | Dynamic_Dispatch (dispatch_exp, method_name, args) ->
      let arg_tacs =
        if List.length args > 0 then
          List.map
            (fun arg ->
              var_ctr := !var_ctr + 1;
              match arg.sub_expr with
              | Assignment (id, _) ->
                  let var_id = Hashtbl.find_opt letTable id.name in
                  let result =
                    match var_id with Some v -> v | None -> id.name
                  in
                  (result, exp_to_tac arg result cname mname)
              | _ ->
                  let id = get_id !var_ctr in
                  (id, exp_to_tac arg id cname mname))
            args
        else []
      in
      var_ctr := !var_ctr + 1;
      let arg2 = String.concat " " (List.map (fun (id, _) -> id) arg_tacs) in
      (List.map (fun (_, v) -> v) arg_tacs |> List.flatten)
      @ exp_to_tac dispatch_exp (get_id !var_ctr) cname mname
      @ [
          {
            operand = Call;
            arg1 = method_name.name;
            arg2;
            result;
            line = exp.id.line_num;
            static_type = exp.static_type;
          };
        ]
  | Static_Dispatch (dispatch_exp, typ, method_name, args) ->
      Printf.fprintf out_file "#; %s.%s w/ type %s, # of args: %d\n" cname
        method_name.name typ.name (List.length args);
      let arg_tacs =
        if List.length args > 0 then
          List.map
            (fun arg ->
              var_ctr := !var_ctr + 1;
              match arg.sub_expr with
              | Assignment (id, _) ->
                  let var_id = Hashtbl.find_opt letTable id.name in
                  let result =
                    match var_id with Some v -> v | None -> id.name
                  in
                  (result, exp_to_tac arg result cname mname)
              | _ ->
                  let id = get_id !var_ctr in
                  (id, exp_to_tac arg id cname mname))
            args
        else []
      in
      var_ctr := !var_ctr + 1;
      let arg2 = String.concat " " (List.map (fun (s, _) -> s) arg_tacs) in
      (List.map (fun (_, v) -> v) arg_tacs |> List.flatten)
      @ exp_to_tac dispatch_exp (get_id !var_ctr) cname mname
      @ [
          {
            operand = StaticCall typ.name;
            arg1 = method_name.name;
            arg2;
            result;
            line = exp.id.line_num;
            static_type = exp.static_type;
          };
        ]
  | Self_Dispatch (id, args) ->
      let arg_tacs =
        if List.length args > 0 then
          List.map
            (fun arg ->
              var_ctr := !var_ctr + 1;
              match arg.sub_expr with
              | Assignment (id, _) ->
                  let var_id = Hashtbl.find_opt letTable id.name in
                  let result =
                    match var_id with Some v -> v | None -> id.name
                  in
                  (result, exp_to_tac arg result cname mname)
              | _ ->
                  let id = get_id !var_ctr in
                  (id, exp_to_tac arg id cname mname))
            args
        else []
      in
      var_ctr := !var_ctr + 1;
      let arg2 = String.concat " " (List.map (fun (s, _) -> s) arg_tacs) in
      (List.map (fun (_, v) -> v) arg_tacs |> List.flatten)
      @ [
          {
            operand = Comment;
            arg1 = "self";
            arg2 = "";
            result = "%rdi";
            line = exp.id.line_num;
            static_type = Some (SELF_TYPE cname);
          };
          {
            operand = Call;
            arg1 = id.name;
            arg2;
            result;
            line = exp.id.line_num;
            static_type = exp.static_type;
          };
        ]
  | If (pred_exp, then_exp, else_exp) ->
      (* 
        NOTE: Control-Flow to Three-Address Code
        The traditional approach to converting control-flow statements to three-address code involves a recursive descent traversal of the abstract syntax tree. The recursive descent traversal returns a list of three-address code instructions.
        ... code to evaluate COND
        bt COND then_label
        ... code to evaluate ELSE_BRANCH
        jmp end_label
        label then_label
        ... code to evaluate THEN_BRACH
        label end_label
      *)
      (*ret := !var_ctr;*)
      var_ctr := !var_ctr + 1;
      let condResult = get_id !var_ctr in
      let cond_tac = exp_to_tac pred_exp condResult cname mname in
      let then_tac = exp_to_tac then_exp result cname mname in
      let else_tac = exp_to_tac else_exp result cname mname in
      var_ctr := !var_ctr + 1;
      let jump_else_value = get_id !var_ctr in
      label_ctr := !label_ctr + 1;
      let then_label = get_label !label_ctr mname cname in
      label_ctr := !label_ctr + 1;
      let else_label = get_label !label_ctr mname cname in
      label_ctr := !label_ctr + 1;
      let join_label = get_label !label_ctr mname cname in
      let true_location = (List.hd (List.rev cond_tac)).result in
      cond_tac
      (* may be possible bug, may need to get the last value of cond_tac instead of result *)
      @ [
          {
            operand = Not;
            arg1 = true_location;
            arg2 = "";
            result = jump_else_value;
            line = exp.id.line_num;
            static_type = exp.static_type;
          };
          {
            operand = Bt;
            arg1 = jump_else_value;
            arg2 = else_label;
            result;
            line = exp.id.line_num;
            static_type = exp.static_type;
          };
          (* @ [ { operand = Bt; arg1 = true_location; arg2 = then_label; result; line=exp.id.line_num} ] *)
          {
            operand = Comment;
            arg1 = "then branch";
            arg2 = "";
            result;
            line = exp.id.line_num;
            static_type = exp.static_type;
          };
          {
            operand = Label;
            arg1 = then_label;
            arg2 = "";
            result;
            line = exp.id.line_num;
            static_type = exp.static_type;
          };
        ]
      @ then_tac
      @ [
          {
            operand = Jmp;
            arg1 = join_label;
            arg2 = "";
            result;
            line = exp.id.line_num;
            static_type = exp.static_type;
          };
          {
            operand = Comment;
            arg1 = "else branch";
            arg2 = "";
            result;
            line = exp.id.line_num;
            static_type = exp.static_type;
          };
          {
            operand = Label;
            arg1 = else_label;
            arg2 = "";
            result;
            line = exp.id.line_num;
            static_type = exp.static_type;
          };
        ]
      @ else_tac
      @ [
          {
            operand = Jmp;
            arg1 = join_label;
            arg2 = "";
            result;
            line = exp.id.line_num;
            static_type = exp.static_type;
          };
          {
            operand = Comment;
            arg1 = "if-join";
            arg2 = "";
            result;
            line = exp.id.line_num;
            static_type = exp.static_type;
          };
          {
            operand = Label;
            arg1 = join_label;
            arg2 = "";
            result;
            line = exp.id.line_num;
            static_type = exp.static_type;
          };
        ]
  | While (pred_exp, body_exp) ->
      var_ctr := !var_ctr + 1;
      let pred_result = get_id !var_ctr in
      var_ctr := !var_ctr + 1;
      let body_result = get_id !var_ctr in
      var_ctr := !var_ctr + 1;
      let cond_tac = exp_to_tac pred_exp pred_result cname mname in
      let body_tac = exp_to_tac body_exp body_result cname mname in
      var_ctr := !var_ctr + 1;
      let jump_else_value = get_id !var_ctr in
      label_ctr := !label_ctr + 1;
      let cond_label = get_label !label_ctr cname mname in
      label_ctr := !label_ctr + 1;
      let join_label = get_label !label_ctr cname mname in
      label_ctr := !label_ctr + 1;
      let body_label = get_label !label_ctr cname mname in
      let true_location = (List.hd (List.rev cond_tac)).result in
      [
        {
          operand = Jmp;
          arg1 = cond_label;
          arg2 = "";
          result;
          line = exp.id.line_num;
          static_type = exp.static_type;
        };
        {
          operand = Comment;
          arg1 = "while-pred";
          arg2 = "";
          result;
          line = exp.id.line_num;
          static_type = exp.static_type;
        };
        {
          operand = Label;
          arg1 = cond_label;
          arg2 = "";
          result;
          line = exp.id.line_num;
          static_type = exp.static_type;
        };
      ]
      @ cond_tac
      @ [
          {
            operand = Not;
            arg1 = true_location;
            arg2 = "";
            result = jump_else_value;
            line = exp.id.line_num;
            static_type = exp.static_type;
          };
          {
            operand = Bt;
            arg1 = jump_else_value;
            arg2 = join_label;
            result;
            line = exp.id.line_num;
            static_type = exp.static_type;
          };
          {
            operand = Bt;
            arg1 = true_location;
            arg2 = body_label;
            result;
            line = exp.id.line_num;
            static_type = exp.static_type;
          };
          {
            operand = Comment;
            arg1 = "while-body";
            arg2 = "";
            result;
            line = exp.id.line_num;
            static_type = exp.static_type;
          };
          {
            operand = Label;
            arg1 = body_label;
            arg2 = "";
            result;
            line = exp.id.line_num;
            static_type = exp.static_type;
          };
        ]
      @ body_tac
      @ [
          {
            operand = Jmp;
            arg1 = cond_label;
            arg2 = "";
            result;
            line = exp.id.line_num;
            static_type = exp.static_type;
          };
          {
            operand = Comment;
            arg1 = "while-join";
            arg2 = "";
            result;
            line = exp.id.line_num;
            static_type = exp.static_type;
          };
          {
            operand = Label;
            arg1 = join_label;
            arg2 = "";
            result;
            line = exp.id.line_num;
            static_type = exp.static_type;
          };
          {
            operand = Default;
            arg1 = "Object";
            arg2 = "";
            result;
            line = exp.id.line_num;
            static_type = exp.static_type;
          };
        ]
  | Block exp_list ->
      List.mapi
        (fun i elem ->
          (* Printf.fprintf out_file
            "Parsing expression: %s for method %s in class %s; var_ctr = %d\n"
            elem.id.name mname cname !var_ctr; *)
          let elem_result =
            if i = List.length exp_list - 1 then result
            else (
              var_ctr := !var_ctr + 1;
              get_id !var_ctr)
          in
          exp_to_tac elem elem_result cname mname)
        exp_list
      |> List.flatten
  | New id ->
      [
        {
          operand = New;
          arg1 = id.name;
          arg2 = "";
          result = get_id !var_ctr;
          line = exp.id.line_num;
          static_type = exp.static_type;
        };
      ]
  | Isvoid exp ->
      var_ctr := !var_ctr + 1;
      let arg1 = get_id !var_ctr in
      exp_to_tac exp arg1 cname mname
      @ [
          {
            operand = Isvoid;
            arg1;
            arg2 = "";
            result;
            line = exp.id.line_num;
            static_type = exp.static_type;
          };
        ]
  | Minus (exp, exp2) ->
      var_ctr := !var_ctr + 1;
      let arg1 = get_id !var_ctr in
      let exp1 = exp_to_tac exp arg1 cname mname in
      var_ctr := !var_ctr + 1;
      let arg2 = get_id !var_ctr in
      let exp2 = exp_to_tac exp2 arg2 cname mname in
      exp1 @ exp2
      @ [
          {
            operand = Minus;
            arg1;
            arg2;
            result;
            line = exp.id.line_num;
            static_type = exp.static_type;
          };
        ]
  | Divide (exp, exp2) ->
      var_ctr := !var_ctr + 1;
      let arg1 = get_id !var_ctr in
      let exp1 = exp_to_tac exp arg1 cname mname in
      var_ctr := !var_ctr + 1;
      let arg2 = get_id !var_ctr in
      let exp2 = exp_to_tac exp2 arg2 cname mname in
      exp1 @ exp2
      @ [
          {
            operand = Divide;
            arg1;
            arg2;
            result;
            line = exp.id.line_num;
            static_type = exp.static_type;
          };
        ]
  | Plus (exp, exp2) ->
      var_ctr := !var_ctr + 1;
      let arg1 = get_id !var_ctr in
      let exp1 = exp_to_tac exp arg1 cname mname in
      var_ctr := !var_ctr + 1;
      let arg2 = get_id !var_ctr in
      let exp2 = exp_to_tac exp2 arg2 cname mname in
      exp1 @ exp2
      @ [
          {
            operand = Plus;
            arg1;
            arg2;
            result;
            line = exp.id.line_num;
            static_type = exp.static_type;
          };
        ]
  | Times (exp, exp2) ->
      var_ctr := !var_ctr + 1;
      let arg1 = get_id !var_ctr in
      let exp1 = exp_to_tac exp arg1 cname mname in
      var_ctr := !var_ctr + 1;
      let arg2 = get_id !var_ctr in
      let exp2 = exp_to_tac exp2 arg2 cname mname in
      exp1 @ exp2
      @ [
          {
            operand = Times;
            arg1;
            arg2;
            result;
            line = exp.id.line_num;
            static_type = exp.static_type;
          };
        ]
  | Equal (exp, exp2) ->
      var_ctr := !var_ctr + 1;
      let arg1 = get_id !var_ctr in
      let exp1 = exp_to_tac exp arg1 cname mname in
      var_ctr := !var_ctr + 1;
      let arg2 = get_id !var_ctr in
      let exp2 = exp_to_tac exp2 arg2 cname mname in
      exp1 @ exp2
      @ [
          {
            operand = Equal;
            arg1;
            arg2;
            result;
            line = exp.id.line_num;
            static_type = exp.static_type;
          };
        ]
  | LessEqual (exp, exp2) ->
      var_ctr := !var_ctr + 1;
      let arg1 = get_id !var_ctr in
      let exp1 = exp_to_tac exp arg1 cname mname in
      var_ctr := !var_ctr + 1;
      let arg2 = get_id !var_ctr in
      let exp2 = exp_to_tac exp2 arg2 cname mname in
      exp1 @ exp2
      @ [
          {
            operand = LessEqual;
            arg1;
            arg2;
            result;
            line = exp.id.line_num;
            static_type = exp.static_type;
          };
        ]
  | LessThan (exp, exp2) ->
      var_ctr := !var_ctr + 1;
      let arg1 = get_id !var_ctr in
      let exp1 = exp_to_tac exp arg1 cname mname in
      var_ctr := !var_ctr + 1;
      let arg2 = get_id !var_ctr in
      let exp2 = exp_to_tac exp2 arg2 cname mname in
      exp1 @ exp2
      @ [
          {
            operand = LessThan;
            arg1;
            arg2;
            result;
            line = exp.id.line_num;
            static_type = exp.static_type;
          };
        ]
  | Not exp ->
      var_ctr := !var_ctr + 1;
      exp_to_tac exp (get_id !var_ctr) cname mname
      @ [
          {
            operand = Not;
            arg1 = get_id !var_ctr;
            arg2 = "";
            result;
            line = exp.id.line_num;
            static_type = exp.static_type;
          };
        ]
  | Negate exp ->
      (*let result = get_id !var_ctr in*)
      (*var_ctr := !var_ctr + 1;*)
      exp_to_tac exp (get_id !var_ctr) cname mname
      @ [
          {
            operand = Negate;
            arg1 = get_id !var_ctr;
            arg2 = "";
            result;
            line = exp.id.line_num;
            static_type = exp.static_type;
          };
        ]
  | Int_Constant i ->
      [
        {
          operand = Int_Constant;
          arg1 = string_of_int i;
          arg2 = "";
          result;
          line = exp.id.line_num;
          static_type = exp.static_type;
        };
      ]
  | String_Constant s ->
      [
        {
          operand = String_Constant;
          arg1 = Printf.sprintf "%s" s;
          arg2 = "";
          result;
          line = exp.id.line_num;
          static_type = exp.static_type;
        };
      ]
  | Ident_Expr s -> (
      match Hashtbl.find_opt letTable s.name with
      | Some t ->
          Printf.fprintf out_file "#; %s.%s: Retrieved variable %s as temp %s\n"
            cname mname s.name t;
          [
            {
              operand = Ident_Expr t;
              arg1 = "";
              arg2 = "";
              result;
              line = exp.id.line_num;
              static_type = exp.static_type;
            };
          ]
      | None ->
          Printf.fprintf out_file "#; %s.%s: Retrieved variable %s\n" cname
            mname s.name;
          [
            {
              operand = Ident_Expr s.name;
              arg1 = "";
              arg2 = "";
              result;
              line = exp.id.line_num;
              static_type = exp.static_type;
            };
          ])
  | Boolean_Constant v ->
      [
        {
          operand = Boolean_Constant;
          arg1 = get_bool v;
          arg2 = "";
          result;
          line = exp.id.line_num;
          static_type = exp.static_type;
        };
      ]
  | Let_Expr (binding_list, exp) ->
      let b =
        List.map
          (fun ((var : identifier), (let_type : identifier), value) ->
            var_ctr := !var_ctr + 1;
            let result = get_id !var_ctr in
            (* Printf.fprintf out_file "Adding variable %s as temp %s\n" var.name
              result; *)
            Hashtbl.add letTable var.name result;
            match value with
            | Some value -> exp_to_tac value result cname mname
            | None ->
                [
                  {
                    operand = LetNoInit;
                    arg1 = "default";
                    arg2 = let_type.name;
                    result;
                    line = exp.id.line_num;
                    static_type = exp.static_type;
                  };
                ])
          binding_list
        |> List.flatten
      in
      let returnVal = b @ exp_to_tac exp result cname mname in
      List.iter
        (fun ((var : identifier), _, _) -> Hashtbl.remove letTable var.name)
        binding_list;
      returnVal
  | Case (case_expr, case_elements) ->
      let init_label_ctr = !label_ctr in
      var_ctr := !var_ctr + 1;
      let caseResult = get_id !var_ctr in
      let case_expr_value = exp_to_tac case_expr caseResult mname cname in
      let caseExprResult = get_id !var_ctr in
      var_ctr := !var_ctr + 1;
      let case_id = get_id !var_ctr in
      let case_expr_id =
        [
          {
            operand = ClassId;
            arg1 = caseExprResult;
            arg2 = "";
            result = case_id;
            line = exp.id.line_num;
            static_type = exp.static_type;
          };
        ]
      in
      label_ctr := !label_ctr + 1;
      let null_case_label = get_label !label_ctr mname cname in
      let null_case_jump =
        [
          {
            operand = Case null_case_label;
            arg1 = "$0";
            arg2 = case_id;
            result = "";
            line = exp.id.line_num;
            static_type = exp.static_type;
          };
        ]
      in
      let join_label = cname ^ "_" ^ mname ^ "_join" in
      let case_names =
        List.map (fun case -> case.typename.name) case_elements
      in
      let case_jumps = get_cases case_names cname mname in
      let defined_case_jumps =
        List.map
          (fun (class_name, case_label) ->
            var_ctr := !var_ctr + 1;
            let caseElemClassResult = get_id !var_ctr in
            var_ctr := !var_ctr + 1;
            let equalResult = get_id !var_ctr in
            [
              {
                operand = ClassId;
                arg1 = class_name;
                arg2 = "";
                result = caseElemClassResult;
                line = exp.id.line_num;
                static_type = exp.static_type;
              };
              {
                operand = Case case_label;
                arg1 = caseElemClassResult;
                arg2 = case_id;
                result = equalResult;
                line = exp.id.line_num;
                static_type = exp.static_type;
              };
            ])
          case_jumps
        |> List.flatten
      in
      (* let empty_case_jump =
        label_ctr := !label_ctr + 1;
        let jump_label = get_label !label_ctr mname cname in
        [
          {
            operand = Jmp;
            arg1 = jump_label;
            arg2 = "";
            result = "";
            line = exp.id.line_num;
            static_type = exp.static_type;
          };
        ] in *)
      label_ctr := init_label_ctr;
      label_ctr := !label_ctr + 1;
      let case_label = get_label !label_ctr mname cname in
      let null_case =
        [
          {
            operand = VoidCase;
            arg1 = case_label;
            arg2 = "";
            result;
            line = exp.id.line_num;
            static_type = exp.static_type;
          };
        ]
      in
      label_ctr := !label_ctr + 1;
      let empty_case =
        [
          {
            operand = EmptyCase;
            arg1 = get_label !label_ctr mname cname;
            arg2 = "";
            result;
            line = exp.id.line_num;
            static_type = exp.static_type;
          };
        ]
      in

      let defined_cases =
        List.map
          (fun elem ->
            (* var_ctr := !var_ctr + 1;
            let var_id = get_id !var_ctr in
            Hashtbl.add letTable elem.variable.name var_id; *)
            Hashtbl.add letTable elem.variable.name caseExprResult;
            label_ctr := !label_ctr + 1;
            let case_label = get_label !label_ctr mname cname in
            [
              {
                operand = Label;
                arg1 = case_label;
                arg2 = "";
                result;
                line = exp.id.line_num;
                static_type = exp.static_type;
              };
            ]
            @ exp_to_tac elem.elem_body result mname cname
            @ [
                {
                  operand = Jmp;
                  arg1 = join_label;
                  arg2 = "";
                  result;
                  line = exp.id.line_num;
                  static_type = exp.static_type;
                };
              ])
          case_elements
        |> List.flatten
      in
      (* case_expr_value @ case_class_id @ null_case @ case_element_values
      @ case_expressions  *)
      let case_join =
        [
          {
            operand = Label;
            arg1 = join_label;
            arg2 = "";
            result;
            line = exp.id.line_num;
            static_type = exp.static_type;
          };
        ]
      in
      let jumps = null_case_jump @ defined_case_jumps in
      let cases = null_case @ defined_cases @ empty_case @ case_join in
      case_expr_value @ case_expr_id @ jumps @ cases
  | Internal _ ->
      Printf.fprintf out_file
        "Something is fundamentally wrong (We should not be parsing Internal \
         to TAC)";
      assert false

(* Print all methods in AST; Debugging purposes only *)
let print_methods (ast_elem : annotated_ast_elem) =
  Printf.printf "%s\n" ast_elem.class_name.name;
  List.iter
    (fun (feat, _) ->
      match feat with
      | Method (name, _, _, _) -> Printf.printf "\t%s\n" name.name
      | Attribute _ -> ())
    (get_all_methods ast_elem)

let () =
  (* Default classes*)
  let default_classes : annotated_ast_elem list =
    [
      {
        class_name = { line_num = 0; name = "Object" };
        inherits = None;
        features = [];
      };
      {
        class_name = { line_num = 0; name = "Bool" };
        inherits = Some { line_num = 0; name = "Object" };
        features = [];
      };
      {
        class_name = { line_num = 0; name = "String" };
        inherits = Some { line_num = 0; name = "Object" };
        features = [];
      };
      {
        class_name = { line_num = 0; name = "Int" };
        inherits = Some { line_num = 0; name = "Object" };
        features = [];
      };
      {
        class_name = { line_num = 0; name = "IO" };
        inherits = Some { line_num = 0; name = "Object" };
        features = [];
      };
    ]
  in
  List.iter add_class default_classes;
  List.iter add_class annotated_ast

(* TAC Code of program *)
let tacs = ast_to_tac annotated_ast

(* Traverse AST to find parts where the control flow changes *)
(* 
    - Conditional Statements
    - Loops
    - Function Calls
    - Start and end of a method call
*)

(* 
  - Sequential statements stored normally in a basic_block
  - If statement stored as: Cond -> if       -> end
                                 -> elseifs  ->
                                 -> else     ->
  - Loops stored as: Cond -> LoopBody
                      |   <- 
                      v 
                     Exit
*)

type basic_block = tac_elem list
and cfg = basic_block list

(*
and control_flow_graph_elem =
  (* Basic Block followed by next CFG elem *)
  | Sequence of basic_block
  (* Basic Block (Condition) followed by Basic Block List (If bodies) followed by next CFG elem *)
  | If_Statement of basic_block * basic_block list
  (* Basic Block (Condition) followed by Basic Block(Loop Body) followed by next CFG elem *)
  | Loop of basic_block * basic_block
  | Function_Call of basic_block

and cfg = control_flow_graph_elem list *)

let is_break_point (tac : tac_elem) =
  match tac.operand with
  | Bt | Call | Jmp | Case _ | Default | Return -> true
  | _ -> false

let tac_to_cfg (tacs, class_name, method_name, attributes, temp_count) :
    cfg * string * string * ast_formal list * int =
  let rec create_cfg (tac_list : tac_elem list) acc cfg =
    match tac_list with
    | tac :: tail -> (
        match is_break_point tac with
        | true -> create_cfg tail [] ([ List.rev (tac :: acc) ] @ cfg)
        | false -> create_cfg tail (tac :: acc) cfg)
    | [] -> [ List.rev acc ] @ cfg
  in
  ( List.rev (create_cfg tacs [] []),
    class_name,
    method_name,
    attributes,
    temp_count )

and print_cfg bbl = List.iter (List.iter print_tac_elem_commented) bbl

let cfg_list : (cfg * string * string * ast_formal list * int) list =
  List.map tac_to_cfg tacs

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
  expression : (tac_elem list * string) option;
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
  | Line s1 -> (*Printf.printf "%s\n" s1;*) Printf.fprintf out_file "%s\n" s1

(* Print out new funcs *)
let print_new_funcs (funcs : new_func list) =
  let print_new_func func =
    let name, lines = func in
    Printf.fprintf out_file "\t.p2align 4\n";
    Printf.fprintf out_file "\t.globl\t%s..new\n" name;
    Printf.fprintf out_file "\t.type\t%s..new, @function\n" name;
    List.iter (fun ln -> print_asm ln) lines;
    (*Printf.fprintf out_file "\t.size\t%s, .-%s\n" name name;*)
    Printf.fprintf out_file
      "\t#;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;\n"
  in
  List.iter print_new_func funcs

(* Locations of variables *)
let var_locations = Hashtbl.create 32

(* Print var_locations map *)
let print_var_locations () =
  Hashtbl.iter
    (fun k v -> Printf.fprintf out_file "\t#; Key: %s, Value: %d(%%rbp)\n" k v)
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
            Some (base_lst @ exp_list, return_index)
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
let parser_class_map : class_map_elem list = parser_class_map

(* Implementation Map *)
let implementation_map = implementation_map

(* Parent Map *)
let parent_map = parent_map
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
      Printf.fprintf out_file
        "\t.align 8\n.globl .string%d\n.string%d:\n\t.string\t\"%s\"\n" v v k)
    string_map;
  Printf.fprintf out_file "\t.align 8\n";
  Printf.fprintf out_file
    "\t.globl empty.string\nempty.string:\n\t.string\t\"\"\n";
  Printf.fprintf out_file "\t.align 8\n";
  Printf.fprintf out_file
    "\t.globl .percent.ld\n.percent.ld:\n\t.string\t\"%%ld\"\n";
  Printf.fprintf out_file "\t.align 8\n";
  Printf.fprintf out_file
    "\t.globl .percent.d\n.percent.d:\n\t.string\t\"%%d\"\n";
  Printf.fprintf out_file "\t.align 8\n";
  Printf.fprintf out_file
    "\t.globl .error_dispatch_void_string\n\
     .error_dispatch_void_string:\n\
     \t.string\t\"ERROR: %%zd: Exception: dispatch on void\\n\"\n";
  Printf.fprintf out_file "\t.align 8\n";
  Printf.fprintf out_file
    "\t.globl .error_case_void_string\n\
     .error_case_void_string:\n\
     \t.string\t\"ERROR: %%zd: Exception: case on void\\n\"\n";
  Printf.fprintf out_file "\t.align 8\n";
  Printf.fprintf out_file
    "\t.globl .error_case_no_match_string\n\
     .error_case_no_match_string:\n\
     \t.string\t\"ERROR: %%zd: Exception: case without matching branch\\n\"\n";
  Printf.fprintf out_file "\t.align 8\n";
  Printf.fprintf out_file
    "\t.globl .error_div_by_zero_string\n\
     .error_div_by_zero_string:\n\
     \t.string\t\"ERROR: %%zd: Exception: division by zero\\n\"\n";
  Printf.fprintf out_file "\t.align 8\n";
  Printf.fprintf out_file
    "\t.globl .error_substr_index_bad_string\n\
     .error_substr_index_bad_string:\n\
     \t.string\t\"ERROR: %%zd: Exception: String.substr out of range\\n\"\n";
  Printf.fprintf out_file "\t.align 8\n";
  Printf.fprintf out_file
    "\t.globl .abort_string\n.abort_string:\n\t.string\t\"abort\"\n";
  Printf.fprintf out_file "\t.text\n"

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
    Printf.fprintf out_file "\t.quad %s.%s\n" func.type_name
      func.method_name
  in
  let name = table.name_id in
  let strid = table.name_string_id in
  Printf.fprintf out_file ".globl %s..vtable\n" name;
  Printf.fprintf out_file "%s..vtable:\n" name;
  Printf.fprintf out_file "\t.quad .string%d\n" strid;
  List.iter print_vtable_func table.methods;
  Printf.fprintf out_file
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
      Printf.fprintf out_file "\t#; Class: %s\n" k;
      List.iter
        (fun attr ->
          Printf.fprintf out_file "\t\t#; Attribute: %s\n" attr.field_name)
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
  match Hashtbl.find_opt var_locations var_name with
  | None -> Hashtbl.add var_locations var_name fp_offset
  | Some _ ->
      (* Printf.fprintf debug_file "Error: Variable %s already has a location\n"
        var_name *)
      ()

(* Get address of a varaible *)
let get_var_addr (var_name : string) : string =
  (* First check if variable is an argument *)
  match Hashtbl.find_opt arg_map var_name with
  | Some addr ->
      if addr < 0 then (
        let registers = [ "%rsi"; "%rdx"; "%rcx"; "%r8"; "%r9" ] in
        let register = List.nth registers (addr + 5) in
        Printf.fprintf debug_file
          "\t#; Argument %s is stored in register %d (%s)\n" var_name addr
          register;
        register)
      else Printf.sprintf "%d(%%rbp)" addr
  | None -> (
      match Hashtbl.find_opt var_locations var_name with
      (* Second check if variable is in a register *)
      | Some addr when addr < 0 ->
          let registers = [ "%rsi"; "%rdx"; "%rcx"; "%r8"; "%r9" ] in
          let register = List.nth registers (addr + 5) in
          Printf.fprintf debug_file
            "\t#; Argument %s is stored in register %d (%s)\n" var_name addr
            register;
          register
      (* Third check if variable is stored somewhere already *)
      | Some addr -> Printf.sprintf "-%d(%%rbp)" addr
      | None ->
          (* Fourth check if variable refers to self pointer *)
          if var_name = "self" || var_name = "%rdi" then "%rdi"
          (* Fifth: That sucks, error *)
            else (
            Printf.fprintf debug_file
              "\t#; Failed to find a temp for variable %s\n" var_name;
            print_var_locations ();
            var_name))

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
  [ Line (get_tac_elem_commented tac) ]
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
          add_var_addr tac.result;
          let result = get_var_addr tac.result in
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
          let result = get_var_addr tac.result in
          let arg1 = get_var_addr tac.arg1 in
          [
            Line "\t#Assignment start";
            Instruction ("movq", arg1, "%rax", "");
            Instruction ("movq", "%rax", result, "");
            Line "\t#Assignment end";
          ])
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
      let result =
        match get_attribute class_name tac.result with
        | Some v -> Printf.sprintf "%d(%%rdi)" ((v.index + 3) * 8)
        | None ->
            Printf.fprintf debug_file "#; %s.%s: %s is not an attribute\n"
              class_name cur_method tac.result;
            add_var_addr tac.result;
            get_var_addr tac.result
      in
      let prev_addr = get_var_addr prev_tac.result in
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
                   match get_attribute class_name arg with
                   | Some v -> Printf.sprintf "%d(%%rdi)" ((v.index + 3) * 8)
                   | None -> get_var_addr arg
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
                     match get_attribute class_name arg with
                     | Some v -> Printf.sprintf "%d(%%rdi)" ((v.index + 3) * 8)
                     | None -> get_var_addr arg
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
        match get_attribute static_class tac.result with
        | Some v -> Printf.sprintf "%d(%%rdi)" ((v.index + 3) * 8)
        | None ->
            Printf.fprintf debug_file "#; %s.%s: %s is not an attribute\n"
              static_class cur_method tac.result;
            add_var_addr tac.result;
            get_var_addr tac.result
      in
      let prev_addr = get_var_addr prev_tac.result in
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
                   | None -> get_var_addr arg
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
                     | None -> get_var_addr arg
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
      add_var_addr tac.result;
      let result = get_var_addr tac.result in
      let class_name = get_var_addr tac.arg1 in
      let class_id = Hashtbl.find_opt class_id_map class_name in
      match class_id with
      | Some class_id ->
          (* Printf.fprintf debug_file "\t#; Class Id of type %s is %d\n" class_name
            class_id; *)
          [ Instruction ("movq", Printf.sprintf "$%d" class_id, result, "") ]
      | None ->
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
      add_var_addr tac.result;
      let result = get_var_addr tac.result in
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
        [
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
  | Ident_Expr ident_name -> (
      let get_attribute class_name attr_name =
        match Hashtbl.find_opt class_attribute_map class_name with
        | Some attr_list ->
            List.find_opt (fun attr -> attr.field_name = attr_name) attr_list
        | None -> None
      in
      let val_addr =
        match Hashtbl.find_opt var_locations ident_name with
        | Some addr ->
            if addr < 0 then (
              let registers = [ "%rsi"; "%rdx"; "%rcx"; "%r8"; "%r9" ] in
              let register = List.nth registers (addr + 5) in
              Printf.fprintf debug_file
                "\t#; Argument %s is stored in register %d (%s)\n" ident_name
                addr register;
              register)
            else Printf.sprintf "-%d(%%rbp)" addr
        | None -> (
            match get_attribute class_name ident_name with
            | Some v -> Printf.sprintf "%d(%%rdi)" ((v.index + 3) * 8)
            | None ->
                Printf.fprintf debug_file
                  "\t#; Failed to find a temp for variable %s\n" ident_name;
                print_class_attributes ();
                get_var_addr ident_name)
      in
      let attrs =
        match Hashtbl.find_opt class_attribute_map class_name with
        | Some v -> v
        | None ->
            Printf.fprintf stderr "Failed to find the attributes of class %s\n"
              class_name;
            assert false
      in
      match List.find_opt (fun f -> f.field_name = tac.result) attrs with
      | Some v ->
          let result = Printf.sprintf "%d(%%rdi)" ((v.index + 3) * 8) in
          [
            Line (Printf.sprintf "\t#Assigning with result %s" tac.result);
            Line "\t#Ident Expr (with attr assignment) start";
            Instruction ("movq", val_addr, "%rax", "");
            Instruction ("movq", "%rax", result, "");
            Line "\t#Ident Expr end";
          ]
      | None ->
          Printf.fprintf debug_file
            "#; Class %s does not have an attribute named %s \n" class_name
            tac.result;
          add_var_addr tac.result;
          let result = get_var_addr tac.result in
          [
            Line "\t#Ident Expr start";
            Instruction ("movq", val_addr, "%rax", "");
            Instruction ("movq", "%rax", result, "");
            Line "\t#Ident Expr end";
          ])
  | Plus ->
      let arg1 = get_var_addr tac.arg1 in
      let arg2 = get_var_addr tac.arg2 in
      let result =
        match get_attribute class_name tac.result with
        | Some v -> Printf.sprintf "%d(%%rdi)" ((v.index + 3) * 8)
        | None ->
            add_var_addr tac.result;
            get_var_addr tac.result
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
      let arg1 = get_var_addr tac.arg1 in
      let arg2 = get_var_addr tac.arg2 in
      let result =
        match get_attribute class_name tac.result with
        | Some v -> Printf.sprintf "%d(%%rdi)" ((v.index + 3) * 8)
        | None ->
            add_var_addr tac.result;
            get_var_addr tac.result
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
      let arg1 = get_var_addr tac.arg1 in
      let arg2 = get_var_addr tac.arg2 in
      let result =
        match get_attribute class_name tac.result with
        | Some v -> Printf.sprintf "%d(%%rdi)" ((v.index + 3) * 8)
        | None ->
            add_var_addr tac.result;
            get_var_addr tac.result
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
      let arg1 = get_var_addr tac.arg1 in
      let arg2 = get_var_addr tac.arg2 in
      let result =
        match get_attribute class_name tac.result with
        | Some v -> Printf.sprintf "%d(%%rdi)" ((v.index + 3) * 8)
        | None ->
            add_var_addr tac.result;
            get_var_addr tac.result
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
      let arg1 = get_var_addr tac.arg1 in
      let arg2 = get_var_addr tac.arg2 in
      let result =
        match get_attribute class_name tac.result with
        | Some v -> Printf.sprintf "%d(%%rdi)" ((v.index + 3) * 8)
        | None ->
            add_var_addr tac.result;
            get_var_addr tac.result
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
      let arg1 = get_var_addr tac.arg1 in
      let arg2 = get_var_addr tac.arg2 in
      let result =
        match get_attribute class_name tac.result with
        | Some v -> Printf.sprintf "%d(%%rdi)" ((v.index + 3) * 8)
        | None ->
            add_var_addr tac.result;
            get_var_addr tac.result
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
      let arg1 = get_var_addr tac.arg1 in
      let arg2 = get_var_addr tac.arg2 in
      let result =
        match get_attribute class_name tac.result with
        | Some v -> Printf.sprintf "%d(%%rdi)" ((v.index + 3) * 8)
        | None ->
            add_var_addr tac.result;
            get_var_addr tac.result
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
      let arg1 = get_var_addr tac.arg1 in
      let result =
        match get_attribute class_name tac.result with
        | Some v -> Printf.sprintf "%d(%%rdi)" ((v.index + 3) * 8)
        | None ->
            add_var_addr tac.result;
            get_var_addr tac.result
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
      let arg1 = get_var_addr tac.arg1 in
      let result =
        match get_attribute class_name tac.result with
        | Some v -> Printf.sprintf "%d(%%rdi)" ((v.index + 3) * 8)
        | None ->
            add_var_addr tac.result;
            get_var_addr tac.result
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
        match get_attribute class_name tac.result with
        | Some v -> Printf.sprintf "%d(%%rdi)" ((v.index + 3) * 8)
        | None ->
            add_var_addr tac.result;
            get_var_addr tac.result
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
        Instruction ("movq", "%rax", result, "");
        Instruction ("popq", "%r9", "", "");
        Instruction ("popq", "%r8", "", "");
        Instruction ("popq", "%rcx", "", "");
        Instruction ("popq", "%rdx", "", "");
        Instruction ("popq", "%rsi", "", "");
        Line "\t#iconst end";
      ]
  | String_Constant -> (
      let result =
        match get_attribute class_name tac.result with
        | Some v -> Printf.sprintf "%d(%%rdi)" ((v.index + 3) * 8)
        | None ->
            add_var_addr tac.result;
            get_var_addr tac.result
      in
      let str = transform_string tac.arg1 in
      match Hashtbl.find_opt string_map str with
      | Some str_id ->
          [ Line "\t#sconst start" ] @ pushargs
          @ [
              Instruction ("call", "String..new", "", "");
              Instruction
                ("movq", "$.string" ^ string_of_int str_id, "24(%rax)", "");
              Instruction ("movq", "%rax", result, "");
            ]
          @ popargs @ [ Line "\t#sconst end" ]
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
              Instruction ("movq", "%rax", result, "");
            ]
          @ popargs @ [ Line "\t#sconst end" ]
      (* This is all we do because they're emitted later :) *)
      (* This is the later but that's another stage; needs to be NOT just in an expression lm ao*)
      (*Printf.fprintf out_file "\t%s\n" (".secton\t.rodata");*)
      (*Printf.fprintf out_file "%s\n" ("string" ^ string_of_int(!string_counter) ^ ":");*)
      (*Printf.fprintf out_file "\t%s\n" (".string \"" ^ string_of_int(!string_counter) ^ "\"");*)
      (*Printf.fprintf out_file "\t%s\n" (".string \"" ^ string_of_int(!string_counter) ^ "\"")*)
      )
  | Boolean_Constant ->
      let result =
        match get_attribute class_name tac.result with
        | Some v -> Printf.sprintf "%d(%%rdi)" ((v.index + 3) * 8)
        | None ->
            add_var_addr tac.result;
            get_var_addr tac.result
      in

      if tac.arg1 = "true" then
        [ Line "\t#bconst start" ] @ pushargs
        @ [
            Instruction ("call", "Bool..new", "", "");
            Instruction ("movq", "$1", "24(%rax)", "");
            Instruction ("movq", "%rax", result, "");
          ]
        @ popargs @ [ Line "\t#bconst end" ]
      else
        [ Line "\t#bconst start" ] @ pushargs
        @ [
            Instruction ("call", "Bool..new", "", "");
            Instruction ("movq", "$0", "24(%rax)", "");
            Instruction ("movq", "%rax", result, "");
          ]
        @ popargs @ [ Line "\t#bconst end" ]
  | Case jump ->
      let arg1 = get_var_addr tac.arg1 in
      let arg2 = get_var_addr tac.arg2 in
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
        match get_attribute class_name tac.result with
        | Some v -> Printf.sprintf "%d(%%rdi)" ((v.index + 3) * 8)
        | None ->
            add_var_addr tac.result;
            get_var_addr tac.result
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
              Instruction ("call", tac.arg1 ^ "..new", "", "");
              Instruction ("movq", "%rax", Printf.sprintf "%s" result, "");
            ]
        else [ Instruction ("movq", "$0", result, "") ]
      in
      pushargs @ new_call @ popargs
  | New ->
      let result =
        match get_attribute class_name tac.result with
        | Some v -> Printf.sprintf "%d(%%rdi)" ((v.index + 3) * 8)
        | None ->
            add_var_addr tac.result;
            get_var_addr tac.result
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
            Instruction ("call", tac.arg1 ^ "..new", "", "");
            Instruction ("movq", "%rax", Printf.sprintf "%s" result, "");
          ]
      in
      pushargs @ new_call @ popargs
  | Isvoid ->
      let result =
        match get_attribute class_name tac.result with
        | Some v -> Printf.sprintf "%d(%%rdi)" ((v.index + 3) * 8)
        | None ->
            add_var_addr tac.result;
            get_var_addr tac.result
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
    let stack_room = Printf.sprintf "$%d" 16 in
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
          if attr.type_name = "SELF_TYPE" then
            [
              Line
                (Printf.sprintf "\t## self[%d] holds field %s : %s" var_index
                   attr.field_name attr.type_name);
              Line (Printf.sprintf "\t## new %s" attr.type_name);
              Instruction ("pushq", "%rbp", "", "");
              Instruction ("pushq", "%rdi", "", "");
              Instruction ("movq", "16(%rdi)", "%r14", "");
              Instruction ("movq", "8(%r14)", "%r14", "");
              Instruction ("call", "*%r14", "", "");
              Instruction ("movq", "%rax", "%r13", "");
              Instruction ("popq", "%rdi", "", "");
              Instruction ("popq", "%rbp", "", "");
              Instruction ("movq", "%r13", stack_location, "");
            ]
          else if
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
          | Some (tacs, retval) ->
              add_var_addr retval;
              let ret = get_var_addr retval in
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
                   tacs
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
        Instruction ("subq", stack_room, "%rsp", "");
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
let method_asm =
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
      match get_attribute class_name !prev_tac.result with
      | Some v -> Printf.sprintf "%d(%%rdi)" ((v.index + 3) * 8)
      | None -> get_var_addr !prev_tac.result
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
    (fun (cfg, class_name, method_name, method_args, temps) ->
      Hashtbl.reset var_locations;
      Hashtbl.reset arg_map;
      (*Printf.printf "new method!!!\n";*)
      let gen_register_arglist args =
        let registers = [ "%rsi"; "%rdx"; "%rcx"; "%r8"; "%r9" ] in
        List.mapi
          (fun i arg ->
            Hashtbl.add arg_map arg (i - 5);
            Printf.fprintf debug_file
              "\t#; Placing var %s in register %d (%s)\n" arg i
              (List.nth registers i);
            Instruction ("movq", get_var_addr arg, List.nth registers i, ""))
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
        List.map (fun (arg : ast_formal) -> arg.name.name) method_args
      in
      (* While there ARE 6 argument registers in the SysV convention, we are dedicating rdi to always be the self pointer *)
      let arglist =
        if List.length args <= 5 then gen_register_arglist args
        else gen_mixed_arglist args
      in
      Printf.fprintf debug_file "#; %s.%s:\n" class_name method_name;
      List.iteri
        (fun i (arg : ast_formal) ->
          Printf.fprintf debug_file "\t#; Argument %d: %s\n" i arg.name.name)
        method_args;

      let stack_space =
        if temps * 8 mod 16 != 0 then (temps + 1) * 8 else temps * 8
      in
      let method_tac = cfg |> List.flatten in
      let asms =
        List.map
          (fun tac ->
            let t = tac_to_as tac method_name class_name !prev_tac in
            prev_tac := tac;
            t)
          method_tac
        |> List.flatten
      in
      get_start_method_boilerplate method_name class_name stack_space
      @ arglist @ asms
      @ get_end_method_boilerplate class_name method_name)
    cfg_list

let tac_list_to_asm lst = List.map tac_to_as lst

let cool_error =
  [
    Line "\t.p2align 4";
    Instruction (".globl", "cool_error", "", "");
    Instruction (".type", "cool_error", "@function", "");
    Line "cool_error:";
    Instruction ("subq", "$8", "%rsp", "");
    Instruction ("cmpq", "$4", "%rdi", "");
    Instruction ("ja", ".error_exit", "", "");
    Instruction ("jmp", "*.jump_table(,%rdi,8)", "", "");
    Instruction (".section", ".rodata", "", "");
    Line "\t.align 8";
    Line "\t.align 4";
    Line ".jump_table:";
    Instruction (".quad", ".error_dispatch_void", "", "");
    Instruction (".quad", ".error_case_void", "", "");
    Instruction (".quad", ".error_case_no_match", "", "");
    Instruction (".quad", ".error_div_by_zero", "", "");
    Instruction (".quad", ".error_substr_index_bad", "", "");
    Line "\t.text";
    Line ".error_div_by_zero:";
    Instruction ("movl", "$.error_div_by_zero_string", "%edi", "");
    Instruction ("xorl", "%eax", "%eax", "");
    Instruction ("call", "printf", "", "");
    Line ".error_exit:";
    Instruction ("xorl", "%edi", "%edi", "");
    Instruction ("call", "exit", "", "");
    Line ".error_substr_index_bad:";
    Instruction ("movl", "$.error_substr_index_bad_string", "%edi", "");
    Instruction ("xorl", "%eax", "%eax", "");
    Instruction ("call", "printf", "", "");
    Instruction ("jmp", ".error_exit", "", "");
    Line ".error_dispatch_void:";
    Instruction ("movl", "$.error_dispatch_void_string", "%edi", "");
    Instruction ("xorl", "%eax", "%eax", "");
    Instruction ("call", "printf", "", "");
    Instruction ("jmp", ".error_exit", "", "");
    Line ".error_case_void:";
    Instruction ("movl", "$.error_case_void_string", "%edi", "");
    Instruction ("xorl", "%eax", "%eax", "");
    Instruction ("call", "printf", "", "");
    Instruction ("jmp", ".error_exit", "", "");
    Line ".error_case_no_match:";
    Instruction ("movl", "$.error_case_no_match_string", "%edi", "");
    Instruction ("xorl", "%eax", "%eax", "");
    Instruction ("call", "printf", "", "");
    Instruction ("jmp", ".error_exit", "", "");
    Instruction (".size", "cool_error", ".-cool_error", "");
  ]

let abort =
  [
    Line "\t.p2align 4";
    Instruction (".globl", "Object.abort", "", "");
    Instruction (".type", "Object.abort", "@function", "");
    Line "Object.abort:";
    Instruction ("movl", "$.abort_string", "%edi", "");
    Instruction ("subq", "$8", "%rsp", "");
    Instruction ("call", "puts", "", "");
    Instruction ("xorl", "%edi", "%edi", "");
    Instruction ("call", "exit", "", "");
    (*Instruction (".size", "Object.abort", ".-Object.abort", "");*)
  ]

let copy =
  [
    Line "\t.p2align 4";
    Instruction (".globl", "Object.copy", "", "");
    Instruction (".type", "Object.copy", "@function", "");
    Line "Object.copy:";
    Instruction ("pushq", "%rbp", "", "");
    Instruction ("pushq", "%rbx", "", "");
    Instruction ("movq", "%rdi", "%rbx", "");
    Instruction ("subq", "$8", "%rsp", "");
    Instruction ("movq", "8(%rdi)", "%rax", "");
    Instruction ("leaq", "0(,%rax,8)", "%rbp", "");
    Instruction ("movq", "%rbp", "%rdi", "");
    Instruction ("call", "malloc", "", "");
    Instruction ("addq", "$8", "%rsp", "");
    Instruction ("movq", "%rbp", "%rdx", "");
    Instruction ("movq", "%rbx", "%rsi", "");
    Instruction ("movq", "%rax", "%rdi", "");
    Instruction ("popq", "%rbx", "", "");
    Instruction ("popq", "%rbp", "", "");
    Instruction ("jmp", "memcpy", "", "");
    (*Instruction (".size", "Object.copy", ".-Object.copy", "");*)
  ]

let type_name =
  [
    Line "\t.p2align 4";
    Instruction (".globl", "Object.type_name", "", "");
    Instruction (".type", "Object.type_name", "@function", "");
    Line "Object.type_name:";
    Instruction ("pushq", "%rbx", "", "");
    Instruction ("movq", "%rdi", "%rbx", "");
    Instruction ("call", "String..new", "", "");
    Instruction ("movq", "16(%rbx)", "%rdx", "");
    Instruction ("movq", "(%rdx)", "%rdx", "");
    Instruction ("movq", "%rdx", "24(%rax)", "");
    Instruction ("popq", "%rbx", "", "");
    Instruction ("ret", "", "", "");
    (*Instruction (".size", "Object.type_name", ".-Object.type_name", "");*)
  ]

let in_int =
  [
    Line "\t.p2align 4";
    Instruction (".globl", "IO.in_int", "", "");
    Instruction (".type", "IO.in_int", "@function", "");
    Line "IO.in_int:";
    Instruction ("pushq", "%rbx", "", "");
    Instruction ("subq", "$32", "%rsp", "");
    Instruction ("movq", "stdin(%rip)", "%rdx", "");
    Instruction ("leaq", "8(%rsp)", "%rdi", "");
    Instruction ("leaq", "16(%rsp)", "%rsi", "");
    Instruction ("movq", "$0", "8(%rsp)", "");
    Instruction ("movq", "$0", "16(%rsp)", "");
    Instruction ("movq", "$0", "24(%rsp)", "");
    Instruction ("call", "getline", "", "");
    Instruction ("movq", "8(%rsp)", "%rdi", "");
    Instruction ("cmpq", "$-1", "%rax", "");
    Instruction ("je", ".in_int_string_error", "", "");
    Instruction ("testq", "%rdi", "%rdi", "");
    Instruction ("je", ".in_int_string_error", "", "");
    Line ".in_int_bounds_check:";
    Instruction ("leaq", "24(%rsp)", "%rsi", "");
    Instruction ("movl", "$10", "%edx", "");
    Instruction ("call", "strtol", "", "");
    Instruction ("movl", "$4294967295", "%edx", "");
    Instruction ("movq", "%rax", "%rbx", "");
    Instruction ("movl", "$2147483648", "%eax", "");
    Instruction ("addq", "%rbx", "%rax", "");
    Instruction ("cmpq", "%rax", "%rdx", "");
    Instruction ("movl", "$0", "%eax", "");
    Instruction ("cmovb", "%rax", "%rbx", "");
    Instruction ("call", "Int..new", "", "");
    Instruction ("movq", "%rbx", "24(%rax)", "");
    Instruction ("addq", "$32", "%rsp", "");
    Instruction ("popq", "%rbx", "", "");
    Instruction ("ret", "", "", "");
    Line "\t.p2align 4,,10";
    Line "\t.p2align 3";
    Line ".in_int_string_error:";
    Instruction ("call", "free", "", "");
    Instruction ("movl", "$1", "%edi", "");
    Instruction ("movl", "$1", "%esi", "");
    Instruction ("call", "calloc", "", "");
    Instruction ("movq", "%rax", "8(%rsp)", "");
    Instruction ("movq", "%rax", "%rdi", "");
    Instruction ("jmp", ".in_int_bounds_check", "", "");
    (*Instruction (".size", "IO.in_int", ".-IO.in_int", "");*)
  ]

let out_int =
  [
    Line "\t.p2align 4";
    Instruction (".globl", "IO.out_int", "", "");
    Instruction (".type", "IO.out_int", "@function", "");
    Line "IO.out_int:";
    Instruction ("pushq", "%rbx", "", "");
    Instruction ("movl", "24(%rsi)", "%esi", "");
    Instruction ("movq", "%rdi", "%rbx", "");
    Instruction ("xorl", "%eax", "%eax", "");
    Instruction ("movl", "$.percent.d", "%edi", "");
    Instruction ("call", "printf", "", "");
    Instruction ("movq", "%rbx", "%rax", "");
    Instruction ("popq", "%rbx", "", "");
    Instruction ("ret", "", "", "");
    (*Instruction (".size", "IO.out_int", ".-IO.out_int", "");*)
  ]

let in_string =
  [
    Line "\t.p2align 4";
    Instruction (".globl", "IO.in_string", "", "");
    Instruction (".type", "IO.in_string", "@function", "");
    Line "IO.in_string:";
    Instruction ("pushq", "%rbp", "", "");
    Instruction ("pushq", "%rbx", "", "");
    Instruction ("subq", "$24", "%rsp", "");
    Instruction ("movq", "stdin(%rip)", "%rdx", "");
    Instruction ("leaq", "8(%rsp)", "%rsi", "");
    Instruction ("movq", "%rsp", "%rdi", "");
    Instruction ("movq", "$0", "(%rsp)", "");
    Instruction ("movq", "$0", "8(%rsp)", "");
    Instruction ("call", "getline", "", "");
    Instruction ("movq", "(%rsp)", "%rbp", "");
    Instruction ("cmpq", "$-1", "%rax", "");
    Instruction ("je", ".in_string_null", "", "");
    Instruction ("testq", "%rbp", "%rbp", "");
    Instruction ("je", ".in_string_null", "", "");
    Instruction ("xorl", "%esi", "%esi", "");
    Instruction ("movq", "%rax", "%rdx", "");
    Instruction ("movq", "%rbp", "%rdi", "");
    Instruction ("movq", "%rax", "%rbx", "");
    Instruction ("call", "memchr", "", "");
    Instruction ("testq", "%rax", "%rax", "");
    Instruction ("jne", ".in_string_null", "", "");
    Instruction ("leaq", "-1(%rbp,%rbx)", "%rax", "");
    Instruction ("cmpb", "$10", "(%rax)", "");
    Instruction ("jne", ".in_string_newline", "", "");
    Instruction ("movb", "$0", "(%rax)", "");
    Instruction ("movq", "(%rsp)", "%rbp", "");
    Line ".in_string_resize:";
    Instruction ("movq", "%rbx", "%rsi", "");
    Instruction ("movq", "%rbp", "%rdi", "");
    Instruction ("call", "realloc", "", "");
    Line ".in_string_end:";
    Instruction ("movq", "%rax", "(%rsp)", "");
    Instruction ("call", "String..new", "", "");
    Instruction ("movq", "24(%rax)", "%rdi", "");
    Instruction ("movq", "%rax", "%rbx", "");
    Instruction ("call", "free", "", "");
    Instruction ("movq", "(%rsp)", "%rax", "");
    Instruction ("movq", "%rax", "24(%rbx)", "");
    Instruction ("addq", "$24", "%rsp", "");
    Instruction ("movq", "%rbx", "%rax", "");
    Instruction ("popq", "%rbx", "", "");
    Instruction ("popq", "%rbp", "", "");
    Instruction ("ret", "", "", "");
    Line "\t.p2align 4,,10";
    Line "\t.p2align 3";
    Line ".in_string_null:";
    Instruction ("movq", "%rbp", "%rdi", "");
    Instruction ("call", "free", "", "");
    Instruction ("movl", "$1", "%esi", "");
    Instruction ("movl", "$1", "%edi", "");
    Instruction ("call", "calloc", "", "");
    Instruction ("jmp", ".in_string_end", "", "");
    Line "\t.p2align 4,,10";
    Line "\t.p2align 3";
    Line ".in_string_newline:";
    Instruction ("addq", "$1", "%rbx", "");
    Instruction ("jmp", ".in_string_resize", "", "");
    (*Instruction (".size", "IO.in_string", ".-IO.in_string", "");*)
    (*Line "\t## ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;";*)
  ]

let out_string =
  [
    Line "\t.p2align 4";
    Instruction (".globl", "IO.out_string", "", "");
    Instruction (".type", "IO.out_string", "@function", "");
    Line "IO.out_string:";
    Instruction ("pushq", "%r12", "", "");
    Instruction ("movq", "%rdi", "%r12", "");
    Instruction ("pushq", "%rbp", "", "");
    Instruction ("pushq", "%rbx", "", "");
    Instruction ("movq", "24(%rsi)", "%rbx", "");
    Instruction ("movsbl", "(%rbx)", "%edi", "");
    Instruction ("testb", "%dil", "%dil", "");
    Instruction ("jne", ".L44", "", "");
    Instruction ("jmp", ".L57", "", "");
    Line "\t.p2align 4,,10";
    Line "\t.p2align 3";
    Line ".L61:";
    Instruction ("call", "putchar", "", "");
    Line ".L49:";
    Instruction ("movsbl", "1(%rbx)", "%edi", "");
    Instruction ("addq", "$1", "%rbx", "");
    Instruction ("testb", "%dil", "%dil", "");
    Instruction ("je", ".L57", "", "");
    Line ".L44:";
    Instruction ("cmpb", "$92", "%dil", "");
    Instruction ("jne", ".L61", "", "");
    Instruction ("movzbl", "1(%rbx)", "%ebp", "");
    Instruction ("addq", "$1", "%rbx", "");
    Instruction ("testb", "%bpl", "%bpl", "");
    Instruction ("je", ".L51", "", "");
    Instruction ("cmpb", "$110", "%bpl", "");
    Instruction ("je", ".L62", "", "");
    Instruction ("cmpb", "$116", "%bpl", "");
    Instruction ("je", ".L46", "", "");
    Instruction ("movl", "$92", "%edi", "");
    Instruction ("addq", "$1", "%rbx", "");
    Instruction ("call", "putchar", "", "");
    Instruction ("movsbl", "%bpl", "%edi", "");
    Instruction ("call", "putchar", "", "");
    Instruction ("movsbl", "(%rbx)", "%edi", "");
    Instruction ("testb", "%dil", "%dil", "");
    Instruction ("jne", ".L44", "", "");
    Line ".L57:";
    Instruction ("movq", "%r12", "%rax", "");
    Instruction ("popq", "%rbx", "", "");
    Instruction ("popq", "%rbp", "", "");
    Instruction ("popq", "%r12", "", "");
    Instruction ("ret", "", "", "");
    Line "\t.p2align 4,,10";
    Line "\t.p2align 3";
    Line ".L62:";
    Instruction ("movl", "$10", "%edi", "");
    Instruction ("call", "putchar", "", "");
    Instruction ("jmp", ".L49", "", "");
    Line "\t.p2align 4,,10";
    Line "\t.p2align 3";
    Line ".L46:";
    Instruction ("movl", "$9", "%edi", "");
    Instruction ("call", "putchar", "", "");
    Instruction ("jmp", ".L49", "", "");
    Line "\t.p2align 4,,10";
    Line "\t.p2align 3";
    Line ".L51:";
    Instruction ("movl", "$92", "%edi", "");
    Instruction ("call", "putchar", "", "");
    Instruction ("movq", "%r12", "%rax", "");
    Instruction ("popq", "%rbx", "", "");
    Instruction ("popq", "%rbp", "", "");
    Instruction ("popq", "%r12", "", "");
    Instruction ("ret", "", "", "");
    (*Instruction (".size", "IO.out_string", ".-IO.out_string", "");*)
  ]

(*let cooloutstr =*)
(*[*)
(*Instruction (".globl\tcooloutstr", "", "", "");*)
(*Instruction (".type\tcooloutstr, @function", "", "", "");*)
(*Instruction ("cooloutstr:", "", "", "");*)
(*Instruction (".LFB6:", "", "", "");*)
(*Instruction (".cfi_startproc", "", "", "");*)
(*Instruction ("endbr64", "", "", "");*)
(*Instruction ("pushq\t%rbp", "", "", "");*)
(*Instruction (".cfi_def_cfa_offset 16", "", "", "");*)
(*Instruction (".cfi_offset 6, -16", "", "", "");*)
(*Instruction ("movq\t%rsp, %rbp", "", "", "");*)
(*Instruction (".cfi_def_cfa_register 6", "", "", "");*)
(*Instruction ("subq\t$32, %rsp", "", "", "");*)
(*Instruction ("movq\t%rdi, -24(%rbp)", "", "", "");*)
(*Instruction ("movl\t$0, -4(%rbp)", "", "", "");*)
(*Instruction ("jmp\t.L2", "", "", "");*)
(*Instruction (".L5:", "", "", "");*)
(*Instruction ("movl\t-4(%rbp), %eax", "", "", "");*)
(*Instruction ("movslq\t%eax, %rdx", "", "", "");*)
(*Instruction ("movq\t-24(%rbp), %rax", "", "", "");*)
(*Instruction ("addq\t%rdx, %rax", "", "", "");*)
(*Instruction ("movzbl\t(%rax), %eax", "", "", "");*)
(*Instruction ("cmpb\t$92, %al", "", "", "");*)
(*Instruction ("jne\t.L3", "", "", "");*)
(*Instruction ("movl\t-4(%rbp), %eax", "", "", "");*)
(*Instruction ("cltq", "", "", "");*)
(*Instruction ("leaq\t1(%rax), %rdx", "", "", "");*)
(*Instruction ("movq\t-24(%rbp), %rax", "", "", "");*)
(*Instruction ("addq\t%rdx, %rax", "", "", "");*)
(*Instruction ("movzbl\t(%rax), %eax", "", "", "");*)
(*Instruction ("cmpb\t$110, %al", "", "", "");*)
(*Instruction ("jne\t.L3", "", "", "");*)
(*Instruction ("movq\tstdout(%rip), %rax", "", "", "");*)
(*Instruction ("movq\t%rax, %rsi", "", "", "");*)
(*Instruction ("movl\t$10, %edi", "", "", "");*)
(*Instruction ("call\tfputc@PLT", "", "", "");*)
(*Instruction ("addl\t$2, -4(%rbp)", "", "", "");*)
(*Instruction ("jmp\t.L2", "", "", "");*)
(*Instruction (".L3:", "", "", "");*)
(*Instruction ("movl\t-4(%rbp), %eax", "", "", "");*)
(*Instruction ("movslq\t%eax, %rdx", "", "", "");*)
(*Instruction ("movq\t-24(%rbp), %rax", "", "", "");*)
(*Instruction ("addq\t%rdx, %rax", "", "", "");*)
(*Instruction ("movzbl\t(%rax), %eax", "", "", "");*)
(*Instruction ("cmpb\t$92, %al", "", "", "");*)
(*Instruction ("jne\t.L4", "", "", "");*)
(*Instruction ("movl\t-4(%rbp), %eax", "", "", "");*)
(*Instruction ("cltq", "", "", "");*)
(*Instruction ("leaq\t1(%rax), %rdx", "", "", "");*)
(*Instruction ("movq\t-24(%rbp), %rax", "", "", "");*)
(*Instruction ("addq\t%rdx, %rax", "", "", "");*)
(*Instruction ("movzbl\t(%rax), %eax", "", "", "");*)
(*Instruction ("cmpb\t$116, %al", "", "", "");*)
(*Instruction ("jne\t.L4", "", "", "");*)
(*Instruction ("movq\tstdout(%rip), %rax", "", "", "");*)
(*Instruction ("movq\t%rax, %rsi", "", "", "");*)
(*Instruction ("movl\t$9, %edi", "", "", "");*)
(*Instruction ("call\tfputc@PLT", "", "", "");*)
(*Instruction ("addl\t$2, -4(%rbp)", "", "", "");*)
(*Instruction ("jmp\t.L2", "", "", "");*)
(*Instruction (".L4:", "", "", "");*)
(*Instruction ("movq\tstdout(%rip), %rdx", "", "", "");*)
(*Instruction ("movl\t-4(%rbp), %eax", "", "", "");*)
(*Instruction ("movslq\t%eax, %rcx", "", "", "");*)
(*Instruction ("movq\t-24(%rbp), %rax", "", "", "");*)
(*Instruction ("addq\t%rcx, %rax", "", "", "");*)
(*Instruction ("movzbl\t(%rax), %eax", "", "", "");*)
(*Instruction ("movsbl\t%al, %eax", "", "", "");*)
(*Instruction ("movq\t%rdx, %rsi", "", "", "");*)
(*Instruction ("movl\t%eax, %edi", "", "", "");*)
(*Instruction ("call\tfputc@PLT", "", "", "");*)
(*Instruction ("addl\t$1, -4(%rbp)", "", "", "");*)
(*Instruction (".L2:", "", "", "");*)
(*Instruction ("movl\t-4(%rbp), %eax", "", "", "");*)
(*Instruction ("movslq\t%eax, %rdx", "", "", "");*)
(*Instruction ("movq\t-24(%rbp), %rax", "", "", "");*)
(*Instruction ("addq\t%rdx, %rax", "", "", "");*)
(*Instruction ("movzbl\t(%rax), %eax", "", "", "");*)
(*Instruction ("testb\t%al, %al", "", "", "");*)
(*Instruction ("jne\t.L5", "", "", "");*)
(*Instruction ("movq\tstdout(%rip), %rax", "", "", "");*)
(*Instruction ("movq\t%rax, %rdi", "", "", "");*)
(*Instruction ("call\tfflush@PLT", "", "", "");*)
(*Instruction ("nop", "", "", "");*)
(*Instruction ("leave", "", "", "");*)
(*Instruction (".cfi_def_cfa 7, 8", "", "", "");*)
(*Instruction ("ret", "", "", "");*)
(*Instruction (".cfi_endproc", "", "", "");*)
(*Instruction (".LFE6:", "", "", "");*)
(*Instruction (".size\tcooloutstr, .-cooloutstr", "", "", "");*)
(*Instruction (".globl\tcoolstrlen", "", "", "");*)
(*Instruction (".type\tcoolstrlen, @function", "", "", "");*)
(*Instruction ("", "", "", "");*)
(*]*)

let string_length =
  [
    Line "\t.p2align 4";
    Instruction (".globl", "String.length", "", "");
    Instruction (".type", "String.length", "@function", "");
    Line "String.length:";
    Instruction ("pushq", "%rbp", "", "");
    Instruction ("movq", "%rdi", "%rbp", "");
    Instruction ("pushq", "%rbx", "", "");
    Instruction ("subq", "$8", "%rsp", "");
    Instruction ("call", "Int..new", "", "");
    Instruction ("movq", "24(%rbp)", "%rdi", "");
    Instruction ("movq", "%rax", "%rbx", "");
    Instruction ("call", "strlen", "", "");
    Instruction ("movq", "%rax", "24(%rbx)", "");
    Instruction ("addq", "$8", "%rsp", "");
    Instruction ("movq", "%rbx", "%rax", "");
    Instruction ("popq", "%rbx", "", "");
    Instruction ("popq", "%rbp", "", "");
    Instruction ("ret", "", "", "");
    (*Instruction (".size", "String.length", ".-String.length", "");*)
  ]

let concat =
  [
    Line "\t.p2align 4";
    Instruction (".globl", "String.concat", "", "");
    Instruction (".type", "String.concat", "@function", "");
    Line "String.concat:";
    Instruction ("pushq", "%r13", "", "");
    Instruction ("pushq", "%r12", "", "");
    Instruction ("movq", "%rsi", "%r12", "");
    Instruction ("pushq", "%rbp", "", "");
    Instruction ("movq", "%rdi", "%rbp", "");
    Instruction ("pushq", "%rbx", "", "");
    Instruction ("subq", "$8", "%rsp", "");
    Instruction ("call", "String..new", "", "");
    Instruction ("movq", "24(%rbp)", "%rdi", "");
    Instruction ("movq", "%rax", "%rbx", "");
    Instruction ("call", "strlen", "", "");
    Instruction ("movq", "24(%r12)", "%rdi", "");
    Instruction ("movq", "%rax", "%r13", "");
    Instruction ("call", "strlen", "", "");
    Instruction ("movq", "24(%rbx)", "%rdi", "");
    Instruction ("leaq", "1(%r13,%rax)", "%rsi", "");
    Instruction ("call", "realloc", "", "");
    Instruction ("movq", "%rax", "24(%rbx)", "");
    Instruction ("movq", "24(%rbp)", "%rsi", "");
    Instruction ("movq", "%rax", "%rdi", "");
    Instruction ("movq", "24(%r12)", "%r12", "");
    Instruction ("call", "stpcpy", "", "");
    Instruction ("movq", "%r12", "%rsi", "");
    Instruction ("movq", "%rax", "%rdi", "");
    Instruction ("call", "strcpy", "", "");
    Instruction ("addq", "$8", "%rsp", "");
    Instruction ("movq", "%rbx", "%rax", "");
    Instruction ("popq", "%rbx", "", "");
    Instruction ("popq", "%rbp", "", "");
    Instruction ("popq", "%r12", "", "");
    Instruction ("popq", "%r13", "", "");
    Instruction ("ret", "", "", "");
    (*Instruction (".size", "String.concat", ".-String.concat", "");*)
  ]

(*let coolgetstr =*)
(*[*)
(*Line "\t.section\t.rodata";*)
(*Line ".LC1:";*)
(*Line "\t.string\t\"\"";*)
(*Line "\t.text";*)
(*Line "\t.globl\tcoolgetstr";*)
(*Line "\t.type\tcoolgetstr, @function";*)
(*Line "coolgetstr:";*)
(*Line ".LFB9:";*)
(*Line "\t.cfi_startproc";*)
(*Instruction ("endbr64", "", "", "");*)
(*Instruction ("pushq", "%rbp", "", "");*)
(*Line "\t.cfi_def_cfa_offset 16";*)
(*Line "\t.cfi_offset 6, -16";*)
(*Instruction ("movq", "%rsp", "%rbp", "");*)
(*Line "\t.cfi_def_cfa_register 6";*)
(*Instruction ("subq", "$16", "%rsp", "");*)
(*Instruction ("movl", "$1", "%esi", "");*)
(*Instruction ("movl", "$40960", "%edi", "");*)
(*Instruction ("call", "calloc@PLT", "", "");*)
(*Instruction ("movq", "%rax", "-8(%rbp)", "");*)
(*Instruction ("movl", "$0", "-16(%rbp)", "");*)
(*Line ".L21:";*)
(*Instruction ("movq", "stdin(%rip)", "%rax", "");*)
(*Instruction ("movq", "%rax", "%rdi", "");*)
(*Instruction ("call", "fgetc@PLT", "", "");*)
(*Instruction ("movl", "%eax", "-12(%rbp)", "");*)
(*Instruction ("cmpl", "$-1", "-12(%rbp)", "");*)
(*Instruction ("je", ".L15", "", "");*)
(*Instruction ("cmpl", "$10", "-12(%rbp)", "");*)
(*Instruction ("jne", ".L16", "", "");*)
(*Line ".L15:";*)
(*Instruction ("cmpl", "$0", "-16(%rbp)", "");*)
(*Instruction ("je", ".L17", "", "");*)
(*Instruction ("leaq", ".LC1(%rip)", "%rax", "");*)
(*Instruction ("jmp", ".L18", "", "");*)
(*Line ".L17:";*)
(*Instruction ("movq", "-8(%rbp)", "%rax", "");*)
(*Instruction ("jmp", ".L18", "", "");*)
(*Line ".L16:";*)
(*Instruction ("cmpl", "$0", "-12(%rbp)", "");*)
(*Instruction ("jne", ".L19", "", "");*)
(*Instruction ("movl", "$1", "-16(%rbp)", "");*)
(*Instruction ("jmp", ".L21", "", "");*)
(*Line ".L19:";*)
(*Instruction ("movq", "-8(%rbp)", "%rax", "");*)
(*Instruction ("movq", "%rax", "%rdi", "");*)
(*Instruction ("call", "coolstrlen", "", "");*)
(*Instruction ("movl", "%eax", "%edx", "");*)
(*Instruction ("movq", "-8(%rbp)", "%rax", "");*)
(*Instruction ("addq", "%rdx", "%rax", "");*)
(*Instruction ("movl", "-12(%rbp)", "%edx", "");*)
(*Instruction ("movb", "%dl", "(%rax)", "");*)
(*Instruction ("jmp", ".L21", "", "");*)
(*Line ".L18:";*)
(*Instruction ("leave", "", "", "");*)
(*Line "\t.cfi_def_cfa 7, 8";*)
(*Instruction ("ret", "", "", "");*)
(*Line "\t.cfi_endproc";*)
(*Line ".LFE9:";*)
(*Line "\t.size\tcoolgetstr, .-coolgetstr";*)
(*]*)

let string_substr =
  [
    Line "String.substr:";
    Instruction ("pushq", "%r13", "", "");
    Instruction ("movq", "%rdx", "%r13", "");
    Instruction ("pushq", "%r12", "", "");
    Instruction ("movq", "%rsi", "%r12", "");
    Instruction ("pushq", "%rbp", "", "");
    Instruction ("movq", "%rdi", "%rbp", "");
    Instruction ("pushq", "%rbx", "", "");
    Instruction ("subq", "$8", "%rsp", "");
    Instruction ("movq", "24(%rdi)", "%rdi", "");
    Instruction ("call", "strlen", "", "");
    Instruction ("movq", "%rax", "%rdx", "");
    Instruction ("movq", "24(%r13)", "%rax", "");
    Instruction ("addq", "24(%r12)", "%rax", "");
    Instruction ("cmpq", "%rax", "%rdx", "");
    Instruction ("jb", ".substr_error", "", "");
    Instruction ("call", "String..new", "", "");
    Instruction ("movq", "24(%rax)", "%rdi", "");
    Instruction ("movq", "%rax", "%rbx", "");
    Instruction ("call", "free", "", "");
    Instruction ("movq", "24(%r13)", "%rsi", "");
    Instruction ("movq", "24(%r12)", "%rdi", "");
    Instruction ("addq", "24(%rbp)", "%rdi", "");
    Instruction ("call", "strndup", "", "");
    Instruction ("movq", "%rax", "24(%rbx)", "");
    Instruction ("addq", "$8", "%rsp", "");
    Instruction ("movq", "%rbx", "%rax", "");
    Instruction ("popq", "%rbx", "", "");
    Instruction ("popq", "%rbp", "", "");
    Instruction ("popq", "%r12", "", "");
    Instruction ("popq", "%r13", "", "");
    Instruction ("ret", "", "", "");
    Line "\t.p2align 4,,10";
    Line "\t.p2align 3";
    Line ".substr_error:";
    Instruction ("xorl", "%esi", "%esi", "");
    Instruction ("movl", "$4", "%edi", "");
    Instruction ("call", "cool_error", "", "");
    (*Instruction (".size", "String.substr", ".-String.substr", "");*)
  ]

(*let concat =*)
(*[*)
(*Line ".globl String.concat";*)
(*Line "String.concat:";*)
(*Line "## method definition";*)
(*Instruction ("pushq", "%rbp", "", "");*)
(*Instruction ("movq", "%rsp", "%rbp", "");*)
(*Instruction ("movq", "16(%rbp)", "%r12", "");*)
(*Line "## stack room for temporaries: 2";*)
(*Instruction ("movq", "$16", "%r14", "");*)
(*Instruction ("subq", "%r14", "%rsp", "");*)
(*Line "## return address handling";*)
(*Line "## fp[3] holds argument s (String)";*)
(*Line "## method body begins";*)
(*Line "## new String";*)
(*Instruction ("pushq", "%rbp", "", "");*)
(*Instruction ("pushq", "%r12", "", "");*)
(*Instruction ("movq", "$String..new", "%r14", "");*)
(*Instruction ("call", "*%r14", "", "");*)
(*Instruction ("popq", "%r12", "", "");*)
(*Instruction ("popq", "%rbp", "", "");*)
(*Instruction ("movq", "%r13", "%r15", "");*)
(*Instruction ("movq", "24(%rbp)", "%r14", "");*)
(*Instruction ("movq", "24(%r14)", "%r14", "");*)
(*Instruction ("movq", "24(%r12)", "%r13", "");*)
(*Line "## guarantee 16-byte alignment before call";*)
(*Instruction ("andq", "$0xFFFFFFFFFFFFFFF0", "%rsp", "");*)
(*Instruction ("movq", "%r13", "%rdi", "");*)
(*Instruction ("movq", "%r14", "%rsi", "");*)
(*Instruction ("call", "coolstrcat", "", "");*)
(*Instruction ("movq", "%rax", "%r13", "");*)
(*Instruction ("movq", "%r13", "24(%r15)", "");*)
(*Instruction ("movq", "%r15", "%r13", "");*)
(*Line ".globl String.concat.end";*)
(*Line "String.concat.end:";*)
(*Line "## method body ends";*)
(*Line "## return address handling";*)
(*Instruction ("movq", "%rbp", "%rsp", "");*)
(*Instruction ("popq", "%rbp", "", "");*)
(*Instruction ("ret", "", "", "");*)
(*Line "## ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;";*)
(*]*)

(*let string_length =*)
(*[*)
(*Line ".globl String.length";*)
(*Line "String.length:";*)
(*Line "## method definition";*)
(*Instruction ("pushq", "%rbp", "", "");*)
(*Instruction ("movq", "%rsp", "%rbp", "");*)
(*Instruction ("movq", "16(%rbp)", "%r12", "");*)
(*Line "## stack room for temporaries: 2";*)
(*Instruction ("movq", "$16", "%r14", "");*)
(*Instruction ("subq", "%r14", "%rsp", "");*)
(*Line "## return address handling";*)
(*Line "## method body begins";*)
(*Line "## new Int";*)
(*Instruction ("pushq", "%rbp", "", "");*)
(*Instruction ("pushq", "%r12", "", "");*)
(*Instruction ("movq", "$Int..new", "%r14", "");*)
(*Instruction ("call", "*%r14", "", "");*)
(*Instruction ("popq", "%r12", "", "");*)
(*Instruction ("popq", "%rbp", "", "");*)
(*Instruction ("movq", "%r13", "%r14", "");*)
(*Instruction ("movq", "24(%r12)", "%r13", "");*)
(*Line "## guarantee 16-byte alignment before call";*)
(*Instruction ("andq", "$0xFFFFFFFFFFFFFFF0", "%rsp", "");*)
(*Instruction ("movq", "%r13", "%rdi", "");*)
(*Instruction ("movl", "$0", "%eax", "");*)
(*Instruction ("call", "coolstrlen", "", "");*)
(*Instruction ("movq", "%rax", "%r13", "");*)
(*Instruction ("movq", "%r13", "24(%r14)", "");*)
(*Instruction ("movq", "%r14", "%r13", "");*)
(*Line ".globl String.length.end";*)
(*Line "String.length.end:";*)
(*Line "## method body ends";*)
(*Line "## return address handling";*)
(*Instruction ("movq", "%rbp", "%rsp", "");*)
(*Instruction ("popq", "%rbp", "", "");*)
(*Instruction ("ret", "", "", "");*)
(*Line "## ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;";*)
(*]*)

(*let string_substr =*)
(*[*)
(*Line ".globl String.substr";*)
(*Line "String.substr:";*)
(*Line "## method definition";*)
(*Instruction ("pushq", "%rbp", "", "");*)
(*Instruction ("movq", "%rsp", "%rbp", "");*)
(*Instruction ("movq", "16(%rbp)", "%r12", "");*)
(*Line "## stack room for temporaries: 2";*)
(*Instruction ("movq", "$16", "%r14", "");*)
(*Instruction ("subq", "%r14", "%rsp", "");*)
(*Line "## return address handling";*)
(*Line "## fp[4] holds argument i (Int)";*)
(*Line "## fp[3] holds argument l (Int)";*)
(*Line "## method body begins";*)
(*Line "## new String";*)
(*Instruction ("pushq", "%rbp", "", "");*)
(*Instruction ("pushq", "%r12", "", "");*)
(*Instruction ("movq", "$String..new", "%r14", "");*)
(*Instruction ("call", "*%r14", "", "");*)
(*Instruction ("popq", "%r12", "", "");*)
(*Instruction ("popq", "%rbp", "", "");*)
(*Instruction ("movq", "%r13", "%r15", "");*)
(*Instruction ("movq", "24(%rbp)", "%r14", "");*)
(*Instruction ("movq", "24(%r14)", "%r14", "");*)
(*Instruction ("movq", "32(%rbp)", "%r13", "");*)
(*Instruction ("movq", "24(%r13)", "%r13", "");*)
(*Instruction ("movq", "24(%r12)", "%r12", "");*)
(*Line "## guarantee 16-byte alignment before call";*)
(*Instruction ("andq", "$0xFFFFFFFFFFFFFFF0", "%rsp", "");*)
(*Instruction ("movq", "%r12", "%rdi", "");*)
(*Instruction ("movq", "%r13", "%rsi", "");*)
(*Instruction ("movq", "%r14", "%rdx", "");*)
(*Instruction ("call", "coolsubstr", "", "");*)
(*Instruction ("movq", "%rax", "%r13", "");*)
(*Instruction ("cmpq", "$0", "%r13", "");*)
(*Instruction ("jne", "l6", "", "");*)
(*Instruction ("movq", "$string7", "%r13", "");*)
(*Line "## guarantee 16-byte alignment before call";*)
(*Instruction ("andq", "$0xFFFFFFFFFFFFFFF0", "%rsp", "");*)
(*Instruction ("movq", "%r13", "%rdi", "");*)
(*Instruction ("call", "cooloutstr", "", "");*)
(*Line "## guarantee 16-byte alignment before call";*)
(*Instruction ("andq", "$0xFFFFFFFFFFFFFFF0", "%rsp", "");*)
(*Instruction ("movl", "$0", "%edi", "");*)
(*Instruction ("call", "exit", "", "");*)
(*Line ".globl l6";*)
(*Line "l6:";*)
(*Instruction ("movq", "%r13", "24(%r15)", "");*)
(*Instruction ("movq", "%r15", "%r13", "");*)
(*Line ".globl String.substr.end";*)
(*Line "String.substr.end:";*)
(*Line "## method body ends";*)
(*Line "## return address handling";*)
(*Instruction ("movq", "%rbp", "%rsp", "");*)
(*Instruction ("popq", "%rbp", "", "");*)
(*Instruction ("ret", "", "", "");*)
(*Line "## ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;";*)
(*Line "## global string constants";*)
(*]*)

let intrinsic_funcs =
  [
    in_int;
    out_int;
    in_string;
    out_string;
    (*cooloutstr;*)
    cool_error;
    (*coolgetstr;*)
    (*coolstrcat;*)
    (*coolstrlen;*)
    (*coolsubstr;*)
    abort;
    copy;
    type_name;
    string_length;
    string_substr;
    concat;
  ]

let handlers =
  [
    Line "\t.p2align 4";
    Instruction (".globl", "lt_handler", "", "");
    Instruction (".type", "lt_handler", "@function", "");
    Line "lt_handler:";
    Instruction ("pushq", "%r12", "", "");
    Instruction ("pushq", "%rbp", "", "");
    Instruction ("movq", "%rsi", "%rbp", "");
    Instruction ("pushq", "%rbx", "", "");
    Instruction ("movq", "%rdi", "%rbx", "");
    Instruction ("call", "Bool..new", "", "");
    Instruction ("movq", "%rax", "%r12", "");
    Instruction ("testq", "%rbx", "%rbx", "");
    Instruction ("je", ".lt_false", "", "");
    Instruction ("testq", "%rbp", "%rbp", "");
    Instruction ("je", ".lt_false", "", "");
    Instruction ("movq", "0(%rbp)", "%rax", "");
    Instruction ("addq", "(%rbx)", "%rax", "");
    Instruction ("leaq", "-2(%rax)", "%rdx", "");
    Instruction ("testq", "$-3", "%rdx", "");
    Instruction ("jne", ".lt_string", "", "");
    Instruction ("movq", "24(%rbp)", "%rax", "");
    Instruction ("cmpq", "%rax", "24(%rbx)", "");
    Instruction ("setl", "%al", "", "");
    Instruction ("movzbl", "%al", "%eax", "");
    Line ".lt_cleanup:";
    Instruction ("movq", "%rax", "24(%r12)", "");
    Instruction ("movq", "%r12", "%rax", "");
    Instruction ("popq", "%rbx", "", "");
    Instruction ("popq", "%rbp", "", "");
    Instruction ("popq", "%r12", "", "");
    Instruction ("ret", "", "", "");
    Line "\t.p2align 4,,10";
    Line "\t.p2align 3";
    Line ".lt_false:";
    Instruction ("xorl", "%eax", "%eax", "");
    Instruction ("movq", "%rax", "24(%r12)", "");
    Instruction ("movq", "%r12", "%rax", "");
    Instruction ("popq", "%rbx", "", "");
    Instruction ("popq", "%rbp", "", "");
    Instruction ("popq", "%r12", "", "");
    Instruction ("ret", "", "", "");
    Line "\t.p2align 4,,10";
    Line "\t.p2align 3";
    Line ".lt_string:";
    Instruction ("cmpq", "$10", "%rax", "");
    Instruction ("jne", ".lt_false", "", "");
    Instruction ("movq", "24(%rbp)", "%rsi", "");
    Instruction ("movq", "24(%rbx)", "%rdi", "");
    Instruction ("call", "strcmp", "", "");
    Instruction ("cltq", "", "", "");
    Instruction ("shrq", "$63", "%rax", "");
    Instruction ("jmp", ".lt_cleanup", "", "");
    Instruction (".size", "lt_handler", ".-lt_handler", "");
    Line "\t.p2align 4";
    Instruction (".globl", "le_handler", "", "");
    Instruction (".type", "le_handler", "@function", "");
    Line "le_handler:";
    Instruction ("pushq", "%r12", "", "");
    Instruction ("pushq", "%rbp", "", "");
    Instruction ("movq", "%rsi", "%rbp", "");
    Instruction ("pushq", "%rbx", "", "");
    Instruction ("movq", "%rdi", "%rbx", "");
    Instruction ("call", "Bool..new", "", "");
    Instruction ("movq", "%rax", "%r12", "");
    Instruction ("cmpq", "%rbp", "%rbx", "");
    Instruction ("je", ".le_equal", "", "");
    Instruction ("testq", "%rbx", "%rbx", "");
    Instruction ("je", ".le_false", "", "");
    Instruction ("testq", "%rbp", "%rbp", "");
    Instruction ("je", ".le_false", "", "");
    Instruction ("movq", "0(%rbp)", "%rax", "");
    Instruction ("addq", "(%rbx)", "%rax", "");
    Instruction ("leaq", "-2(%rax)", "%rdx", "");
    Instruction ("testq", "$-3", "%rdx", "");
    Instruction ("jne", ".le_string", "", "");
    Instruction ("movq", "24(%rbp)", "%rax", "");
    Instruction ("cmpq", "%rax", "24(%rbx)", "");
    Instruction ("setle", "%al", "", "");
    Instruction ("movzbl", "%al", "%eax", "");
    Line ".le_cleanup:";
    Instruction ("movq", "%rax", "24(%r12)", "");
    Instruction ("movq", "%r12", "%rax", "");
    Instruction ("popq", "%rbx", "", "");
    Instruction ("popq", "%rbp", "", "");
    Instruction ("popq", "%r12", "", "");
    Instruction ("ret", "", "", "");
    Line "\t.p2align 4,,10";
    Line "\t.p2align 3";
    Line ".le_equal:";
    Instruction ("movl", "$1", "%eax", "");
    Instruction ("movq", "%rax", "24(%r12)", "");
    Instruction ("movq", "%r12", "%rax", "");
    Instruction ("popq", "%rbx", "", "");
    Instruction ("popq", "%rbp", "", "");
    Instruction ("popq", "%r12", "", "");
    Instruction ("ret", "", "", "");
    Line "\t.p2align 4,,10";
    Line "\t.p2align 3";
    Line ".le_false:";
    Instruction ("xorl", "%eax", "%eax", "");
    Instruction ("jmp", ".le_cleanup", "", "");
    Line "\t.p2align 4,,10";
    Line "\t.p2align 3";
    Line ".le_string:";
    Instruction ("cmpq", "$10", "%rax", "");
    Instruction ("jne", ".le_false", "", "");
    Instruction ("movq", "24(%rbp)", "%rsi", "");
    Instruction ("movq", "24(%rbx)", "%rdi", "");
    Instruction ("call", "strcmp", "", "");
    Instruction ("testl", "%eax", "%eax", "");
    Instruction ("setle", "%al", "", "");
    Instruction ("movzbl", "%al", "%eax", "");
    Instruction ("jmp", ".le_cleanup", "", "");
    Instruction (".size", "le_handler", ".-le_handler", "");
    Line "\t.p2align 4";
    Instruction (".globl", "eq_handler", "", "");
    Instruction (".type", "eq_handler", "@function", "");
    Line "eq_handler:";
    Instruction ("pushq", "%r12", "", "");
    Instruction ("pushq", "%rbp", "", "");
    Instruction ("movq", "%rsi", "%rbp", "");
    Instruction ("pushq", "%rbx", "", "");
    Instruction ("movq", "%rdi", "%rbx", "");
    Instruction ("call", "Bool..new", "", "");
    Instruction ("movq", "%rax", "%r12", "");
    Instruction ("cmpq", "%rbp", "%rbx", "");
    Instruction ("je", ".eq_equal", "", "");
    Instruction ("testq", "%rbx", "%rbx", "");
    Instruction ("je", ".eq_false", "", "");
    Instruction ("testq", "%rbp", "%rbp", "");
    Instruction ("je", ".eq_false", "", "");
    Instruction ("movq", "0(%rbp)", "%rax", "");
    Instruction ("addq", "(%rbx)", "%rax", "");
    Instruction ("leaq", "-2(%rax)", "%rdx", "");
    Instruction ("testq", "$-3", "%rdx", "");
    Instruction ("jne", ".eq_string", "", "");
    Instruction ("movq", "24(%rbp)", "%rax", "");
    Instruction ("cmpq", "%rax", "24(%rbx)", "");
    Instruction ("sete", "%al", "", "");
    Instruction ("movzbl", "%al", "%eax", "");
    Line ".eq_cleanup:";
    Instruction ("movq", "%rax", "24(%r12)", "");
    Instruction ("movq", "%r12", "%rax", "");
    Instruction ("popq", "%rbx", "", "");
    Instruction ("popq", "%rbp", "", "");
    Instruction ("popq", "%r12", "", "");
    Instruction ("ret", "", "", "");
    Line "\t.p2align 4,,10";
    Line "\t.p2align 3";
    Line ".eq_equal:";
    Instruction ("movl", "$1", "%eax", "");
    Instruction ("movq", "%rax", "24(%r12)", "");
    Instruction ("movq", "%r12", "%rax", "");
    Instruction ("popq", "%rbx", "", "");
    Instruction ("popq", "%rbp", "", "");
    Instruction ("popq", "%r12", "", "");
    Instruction ("ret", "", "", "");
    Line "\t.p2align 4,,10";
    Line "\t.p2align 3";
    Line ".eq_false:";
    Instruction ("xorl", "%eax", "%eax", "");
    Instruction ("jmp", ".eq_cleanup", "", "");
    Line "\t.p2align 4,,10";
    Line "\t.p2align 3";
    Line ".eq_string:";
    Instruction ("cmpq", "$10", "%rax", "");
    Instruction ("jne", ".eq_false", "", "");
    Instruction ("movq", "24(%rbp)", "%rsi", "");
    Instruction ("movq", "24(%rbx)", "%rdi", "");
    Instruction ("call", "strcmp", "", "");
    Instruction ("testl", "%eax", "%eax", "");
    Instruction ("sete", "%al", "", "");
    Instruction ("movzbl", "%al", "%eax", "");
    Instruction ("jmp", ".eq_cleanup", "", "");
    Instruction (".size", "eq_handler", ".-eq_handler", "");
  ]

let () =
  (*
  (* Print all tacs *)
  List.iter (fun (f, _, _, _, _) -> print_tac_elems f) tacs;

  (* Default vtables *)
  List.iter print_vtable vtables;

  (* New functions (class initializers) *)
  print_new_funcs new_funcs;

  (* Important default functions *)
  let print_instrinsic_func ifunc =
    List.iter print_asm ifunc;
    Printf.fprintf out_file
      "\t#;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;\n"
  in
  List.iter print_instrinsic_func intrinsic_funcs;

  (* Assembly for methods *)
  List.iter (List.iter print_asm) method_asm;
  Printf.fprintf out_file "\t.section\t.rodata\n";

  (* Assembly Strings *)
  print_string_map ();

  (* Value comparison handlers *)
  List.iter print_asm handlers;

  (* Print start *)
  print_start ()
  *)
  ()
