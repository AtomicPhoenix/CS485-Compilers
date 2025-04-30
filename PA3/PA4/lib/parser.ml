open Print

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
