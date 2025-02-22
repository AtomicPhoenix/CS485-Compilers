type program = cool_class list
(** ast is a list of classes *)

and identifier = { line_num : int; name : string }
(** identifier is a line number and a name *)

and cool_class = {
  typename : identifier;
  inherits : identifier option;
  features : feature list;
}

(* cool class is an identifier of the typename, an identifier of the inherit,
    and a feature list *)
(*and expr = identifier*)

(* A COOL expression *)
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

and feature =
  | Attribute of (identifier * identifier * expr option)
      (** name (identifier), type (identifier), and assignment (expr option, to
          cover no init) *)
  | Method of identifier * formal list * identifier * expr
      (** name (identifier), formal list (formal list), type (identifier), and
          body (expression) *)

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

and arith_operator = Plus | Minus | Times | Divide
and comparison_operator = LessThan | LessEqual | Equal
and bool_val = True | False

let file = open_in Sys.argv.(1)

(** Read line returns one line from the AST*)
let read () = input_line file

let rec get_class () : cool_class =
  let ident = get_identifier () in
  let inh = get_inherits () in
  let feats = get_feature_list () in
  { typename = ident; inherits = inh; features = feats }

and get_feature_list () =
  try List.init (int_of_string (read ())) (fun _ -> get_feature ())
  with _ -> raise (Invalid_argument "Get Feature List: Not a num")

and get_expression_list () =
  try List.init (int_of_string (read ())) (fun _ -> get_expression ())
  with _ -> raise (Invalid_argument "Not a num")

and get_formal_list () =
  try List.init (int_of_string (read ())) (fun _ -> get_formal ())
  with _ -> raise (Invalid_argument "Not a num")

and get_class_list () =
  try List.init (int_of_string (read ())) (fun _ -> get_class ())
  with _ -> raise (Invalid_argument "Not a num")

and get_expression () : expr =
  let base_expr = get_identifier () in
  Expression (base_expr, get_sub_expr base_expr.name)

and get_sub_expr = function
  | "assign" ->
      let i = get_identifier () in
      let e = get_expression () in
      Assignment (i, e)
  | "dyanmic_dispatch" ->
      let e = get_expression () in
      let i = get_identifier () in
      let el = get_expression_list () in
      Dynamic_Dispatch (e, i, el)
  | "static_dispatch" ->
      let e = get_expression () in
      let i1 = get_identifier () in
      let i2 = get_identifier () in
      let el = get_expression_list () in
      Static_Dispatch (e, i1, i2, el)
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
  | "block" -> Block (get_expression_list ())
  | "new" -> New (get_identifier ())
  | "isvoid" -> Isvoid (get_expression ())
  | "plus" ->
      let e1 = get_expression () in
      let e2 = get_expression () in
      Arith_Operation (Plus, e1, e2)
  | "minus" ->
      let e1 = get_expression () in
      let e2 = get_expression () in
      Arith_Operation (Minus, e1, e2)
  | "times" ->
      let e1 = get_expression () in
      let e2 = get_expression () in
      Arith_Operation (Times, e1, e2)
  | "divide" ->
      let e1 = get_expression () in
      let e2 = get_expression () in
      Arith_Operation (Divide, e1, e2)
  | "lt" ->
      let e1 = get_expression () in
      let e2 = get_expression () in
      Comparison_Operation (LessThan, e1, e2)
  | "le" ->
      let e1 = get_expression () in
      let e2 = get_expression () in
      Comparison_Operation (LessEqual, e1, e2)
  | "eq" ->
      let e1 = get_expression () in
      let e2 = get_expression () in
      Comparison_Operation (Equal, e1, e2)
  | "not" -> Not (get_expression ())
  | "negate" -> Negate (get_expression ())
  | "integer" -> Int_Constant (int_of_string (read ()))
  | "string" -> String_Constant (read ())
  | "_identifier_" -> Ident_Expr (get_identifier ())
  | "true" -> Boolean_Constant True
  | "false" -> Boolean_Constant False
  | _ -> raise Not_found

and get_identifier () : identifier =
  let r = read () in
  let linenum = int_of_string r in
  let name = read () in
  { line_num = linenum; name }

and get_inherits () : identifier option =
  let does_inherit = read () in
  if does_inherit = "no_inherit" then None else Some (get_identifier ())
(*
and make_elem_list lst num fn = match num with
  | 0 -> lst
  | _ -> make_elem_list (lst@[fn ()]) (num-1) fn
*)

and get_no_init_attribute () =
  let name = get_identifier () in
  let typename = get_identifier () in
  Attribute (name, typename, None)

and get_init_attribute () =
  let name = get_identifier () in
  let typename = get_identifier () in
  let exp = get_expression () in
  Attribute (name, typename, Some exp)

and get_formal () =
  let name = get_identifier () in
  let typename = get_identifier () in
  { name; typename }

and get_method () =
  let name = get_identifier () in
  let formals = get_formal_list () in
  let typename = get_identifier () in
  let body = get_expression () in
  Method (name, formals, typename, body)

and get_feature () =
  let feat_type = read () in
  match feat_type with
  | "attribute_no_init" -> get_no_init_attribute ()
  | "attribute_init" -> get_init_attribute ()
  | "method" -> get_method ()
  | _ -> assert false

let ast = List.init (int_of_string (read ())) (fun _ -> get_class ())
let printf = Printf.printf

let print_class_map =
  printf "class_map\n";
  printf "%d\n" (List.length ast)

let rec get_attribute_count (c_class : cool_class) : int = 5

and print_features (feat : feature) =
  match feat with
  | Attribute (name, typ, assign) -> (
      match assign with
      | None -> printf "no_initializer\n%s\n%s\n" name.name typ.name
      | Some exp ->
          printf "initializer\n%s\n%s\n" name.name typ.name;
          print_expression exp)
  | _ -> ()

and print_attributes (c_class : cool_class) =
  let feats = c_class.features in
  let atrs =
    List.filter
      (function
        | el -> ( match el with Attribute (e1, e2, _) -> true | _ -> false))
      feats
  in
  List.iter print_features atrs

and print_expression (exp : expr) =
  match exp with
  | Expression (id, sub) ->
      print_identifier id;
      print_sub_expr sub

and print_identifier (id : identifier) = printf "%d\n%s\n" id.line_num id.name

and print_sub_expr (sub_exp : sub_expr) =
  match sub_exp with
  | Assignment (id, exp) ->
      print_identifier id;
      print_expression exp
  | Dynamic_Dispatch (exp, id, el) ->
      print_expression exp;
      print_identifier id;
      List.iter print_expression el
  | Static_Dispatch (exp, id1, id2, el) ->
      print_expression exp;
      print_identifier id1;
      print_identifier id2;
      List.iter print_expression el
  | Self_Dispatch (id, el) ->
      print_identifier id;
      List.iter print_expression el
  | If (exp1, exp2, exp3) ->
      print_expression exp1;
      print_expression exp2;
      print_expression exp3
  | While (exp1, exp2) ->
      print_expression exp1;
      print_expression exp2
  | Block el -> List.iter print_expression el
  | New id -> print_identifier id
  | Isvoid exp -> print_expression exp
  | Arith_Operation (typename, exp, exp2) ->
      (match typename with
      | Plus -> ()
      | Minus -> ()
      | Divide -> ()
      | Times -> ());
      print_expression exp;
      print_expression exp2
  | Comparison_Operation (typename, exp, exp2) ->
      (match typename with Equal -> () | LessThan -> () | LessEqual -> ());
      print_expression exp;
      print_expression exp2
  | Not exp -> print_expression exp
  | Negate exp -> print_expression exp
  | Int_Constant i -> printf "%d\n" i
  | String_Constant s -> printf "%s\n" s
  | Ident_Expr s -> print_identifier s
  | Boolean_Constant v -> (
      match v with
      | True -> print_endline "true"
      | False -> print_endline "false")
  | Let_Expr (let_exp1, exp2) ->
      (match let_exp1 with
      | Let_Binding_Init _ -> print_endline "let_binding_no_init"
      | Let_Binding_No_Init _ -> print_endline "let_binding_no_init");
      print_expression exp2

(*
  let print_class (c_class : cool_class) =
    printf "%s" c_class.typename.name;
    printf "%d" (get_attribute_count c_class);
    print_attributes c_class
  in
  List.iter print_class ast *)
