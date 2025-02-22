type program = cool_class list
(** ast is a list of classes *)

and identifier = { line_num : int; name : string }
(** identifier is a line number and a name *)

and cool_class = {
  typename : identifier;
  inherits : identifier option;
  features : feature list;
}
(** cool class is an identifier of the typename, an identifier of the inherit,
    and a feature list *)
(*and expr = identifier*)

(** A COOL expression *)
and expr = Expression of identifier * sub_expr

and sub_expr =
  | Assignment of identifier * expr
  | Dynamic_Dispatch of expr * identifier * expr list
  | Static_Dispatch of expr * identifier * identifier * expr list
  | Self_Dispatch of identifier * expr list
  | If of expr * expr * expr
  | While of expr * expr
  | Block of expr list
  | New of identifier
  | Isvoid of expr
  | Arith_Operation of arith_operator * expr * expr
  | Comparison_Operation of comparison_operator * expr * expr
  | Not of expr
  | Negate of expr
  | Int_Constant of int
  | String_Constant of string
  | Ident_Expr of identifier
  | Boolean_Constant of bool_val
  | Let_Expr of let_expr * expr

and let_expr =
  | Let_Binding_No_Init of identifier * identifier
      (** line number & "let" (identifier), variable (identifier), type
          (identifier)*)
  | Let_Binding_Init of identifier * identifier * expr
      (** line number & "let" (identifier), variable (identifier), type
          (identifier), and value (exp)*)

(* maybe dead code? *)
(*and attribute =*)
(*{*)
(*name : identifier;*)
(*typename : identifier;*)
(*expression : expr;*)
(*}*)

(*and feat_method =*)
(*{*)
(*methodname : identifier;*)
(*arguments : formal list;*)
(*typename : identifier;*)
(*body : expr;*)
(*}*)
and feature =
  | Attribute of identifier * identifier * expr
      (** name (identifier), type (identifier), and assignment (expr) *)
  (*| Attribute of attribute*)
  | Method of identifier * formal list * identifier * expr
      (** name (identifier), formal list (formal list), type (identifier), and
          body (expression) *)
(*| Method of feat_method*)

and formal = { name : identifier; typename : identifier }
(** name (identifier) and type (identifier) *)

and let_exp =
  | No_init of identifier * identifier
  | Yes_init of identifier * identifier * expr

and case = { lnum : identifier; case_exp : expr; elements : case_el list }
(** line number & "case" (identifier), case expression (exp) and case-elements
    (case_el list) *)

and case_el = { variable : identifier; typename : identifier; elem_body : expr }
(** variable (identifier), type (identifier), and case-element-body (exp) *)

(* expr types *)
(*
and assign =
  {
    var : identifier;
    rhs : expr;
  }
and dynamic_dispatch =
  {
    e : expr;
    typename : identifier;
    methodname : identifier;
    args : expr list;
  }
and static_dispatch =
  {
    e : expr;
    typename : identifier;
    methodname : identifier;
    args : expr list;
  }
and self_dispatch =
  {
    methodname : identifier;
    args : expr list;
  }
and if_stmt =
  {
    predicate : expr;
    body : expr;
    else_body : expr;
  }
and while_stmt =
  {
    predicate : expr;
    body : expr;
  }
and block =
  {
    body : expr list;
  }
and new_stmt =
  {
    classname : identifier;
  }
and isvoid_expr =
  {
    e : expr;
  }
*)
and arith_operator = Plus | Minus | Times | Divide

(*
and arith_oper =
  {
    operator : arith_operator;
    x : expr;
    y : expr;
  }
*)
and comparison_operator = LessThan | LessEqual | Equal
(*
and comparison_oper =
  {
    operator : comparison_operator;
    x : expr;
    y : expr;
  }

and not_expr =
  {
    x : expr;
  }

and negate_expr =
  {
    x : expr;
  }
  
and underscore_ident =
  {
    variable : identifier;
  }
*)

and bool_val = True | False

let file = open_in Sys.argv.(1)

(** Read line returns one line from the AST*)
let read () = input_line file

(*-----------------EVERYTHING BELOW IS PROBABLY BROKEN-----------------------------------*)
let rec get_class () =
  {
    typename = get_ident ();
    inherits = get_inherits ();
    features = get_feature_list ();
  }

and get_inherits () =
  let does_inherit = read () in
  if does_inherit = "no_inherit" then None else Some (get_ident ())

and get_feature_list () =
  List.init (int_of_string (read ())) (fun _ -> get_feature ())

and get_expression_list () =
  List.init (int_of_string (read ())) (fun _ -> get_expression ())
(* let rec get_list_inner len =
    if (len<=0) then []
    else func()k :: get_list_inner (len-1)
  in get_list_inner (int_of_string (read())) *)

and get_ident () : identifier =
  let line = read () in
  let str = read () in
  { line_num = int_of_string line; name = str }

(* TODO: Make this get non-attribute features *)
and get_feature () : feature =
  (*let name = read () in*)
  let attr_name = get_ident () in
  let attr_type = get_ident () in
  (*(name, attr_name, attr_type)*)
  Attribute (attr_name, attr_type, get_expression ())

and get_expression () : expr =
  let base_expr = get_ident () in
  Expression (base_expr, get_sub_expr base_expr.name)

and get_sub_expr = function
  | "assign" -> Assignment (get_ident (), get_expression ())
  | "dyanmic_dispatch" ->
      Dynamic_Dispatch (get_expression (), get_ident (), get_expression_list ())
  | "static_dispatch" ->
      Static_Dispatch
        (get_expression (), get_ident (), get_ident (), get_expression_list ())
  | "self_dispatch" -> Self_Dispatch (get_ident (), get_expression_list ())
  | "if" -> If (get_expression (), get_expression (), get_expression ())
  | "while" -> While (get_expression (), get_expression ())
  | "block" -> Block (get_expression_list ())
  | "new" -> New (get_ident ())
  | "isvoid" -> Isvoid (get_expression ())
  | "plus" -> Arith_Operation (Plus, get_expression (), get_expression ())
  | "minus" -> Arith_Operation (Minus, get_expression (), get_expression ())
  | "times" -> Arith_Operation (Times, get_expression (), get_expression ())
  | "divide" -> Arith_Operation (Divide, get_expression (), get_expression ())
  | "lt" -> Comparison_Operation (LessThan, get_expression (), get_expression ())
  | "le" ->
      Comparison_Operation (LessEqual, get_expression (), get_expression ())
  | "eq" -> Comparison_Operation (Equal, get_expression (), get_expression ())
  | "not" -> Not (get_expression ())
  | "negate" -> Negate (get_expression ())
  | "integer" -> Int_Constant (int_of_string (read ()))
  | "string" -> String_Constant (read ())
  | "_identifier_" -> Ident_Expr (get_ident ())
  | "true" -> Boolean_Constant True
  | "false" -> Boolean_Constant False
  | _ -> raise Not_found

let ast = List.init (int_of_string (read ())) (fun _ -> get_class ())
let class_list = List.map (fun c_class -> c_class.typename) ast
let () = Printf.printf "%d\tNumber of Classes\n" (List.length class_list)

let rec print_ident ident : unit =
  Printf.printf "%s\tIdentifier String\n" ident.name;
  Printf.printf "%d\tIdentifier Num\n" ident.line_num

and print_class c_class : unit =
  print_ident c_class.typename;
  Printf.printf "%d\tNumber of Features\n" (List.length c_class.features)

let () = List.iter print_class ast
