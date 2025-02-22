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

(** name (identifier), formal list (formal list), type (identifier), and body
    (expression) *)

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

let class_map = Hashtbl.create 5

let add_class (c_class : cool_class) =
  let name = c_class.typename.name in
  match Hashtbl.find_opt class_map name with
  | None -> Hashtbl.add class_map name c_class
  | Some _ ->
      Printf.printf "ERROR: %d: Type-Check: class %s redefined"
        c_class.typename.line_num c_class.typename.name;
      exit 1

let default_classes =
  [
    {
      typename = { line_num = 0; name = "Object" };
      inherits = None;
      features = [];
    };
    {
      typename = { line_num = 0; name = "Bool" };
      inherits = Some { line_num = 0; name = "Object" };
      features = [];
    };
    {
      typename = { line_num = 0; name = "String" };
      inherits = Some { line_num = 0; name = "Object" };
      features = [];
    };
    {
      typename = { line_num = 0; name = "Integer" };
      inherits = Some { line_num = 0; name = "Object" };
      features = [];
    };
    {
      typename = { line_num = 0; name = "IO" };
      inherits = Some { line_num = 0; name = "Object" };
      features = [];
    };
  ]

let () = List.iter add_class default_classes
let file = open_in Sys.argv.(1)

(** Read line returns one line from the AST*)
let read () = input_line file

let rec get_class () : cool_class =
  let ident = get_identifier () in
  let inh = get_inherits () in
  let feats = get_feature_list () in
  { typename = ident; inherits = inh; features = feats }

and get_feature_list () =
  List.init (int_of_string (read ())) (fun _ -> get_feature ())

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
  | "integer" -> (
      try Int_Constant (int_of_string (read ()))
      with _ -> raise Division_by_zero)
  | "string" -> String_Constant (read ())
  | "_identifier_" -> Ident_Expr (get_identifier ())
  | "true" -> Boolean_Constant True
  | "false" -> Boolean_Constant False
  | _ -> raise Not_found

and get_identifier () : identifier =
  try
    let r = read () in
    let linenum = int_of_string r in
    let name = read () in
    { line_num = linenum; name }
  with _ -> raise Division_by_zero

and get_inherits () : identifier option =
  let does_inherit = read () in
  if does_inherit = "no_inherits" then None else Some (get_identifier ())

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
  | c ->
      print_endline c;
      assert false

let printf = Printf.printf

let rec print_class_map ast =
  printf "class_map\n";
  printf "%d\n" (List.length ast);
  List.iter
    (fun c_class ->
      printf "%s\n" c_class.typename.name;
      print_attributes c_class)
    ast

and get_all_attributes (c_class : cool_class) =
  let rec get_tree (parent : identifier option) =
    let rec ancestors acc =
      match parent with Some c -> c :: acc | None -> []
    in
    ancestors []
  in
  let parent_tree = c_class.typename :: get_tree c_class.inherits in
  let get_attributes id =
    List.filter
      (function Attribute _ -> true | _ -> false)
      (Hashtbl.find class_map id.name).features
  in
  List.flatten (List.map get_attributes parent_tree)

(* let rec get_attrs(c_class) = List.filter (function Method _ -> true | _ -> false) c_class in *)

and get_features (c_class : cool_class) (predicate : feature -> bool) =
  let selected = List.filter predicate c_class.features in
  printf "%d\n" (List.length selected);
  if List.length selected > 0 then
    let print =
     fun feat ->
      match feat with
      | Attribute (name, typ, assign) -> (
          match assign with
          | None -> printf "no_initializer\n%s\n%s\n" name.name typ.name
          | Some exp ->
              printf "initializer\n%s\n%s\n" name.name typ.name;
              print_init_expression (exp, typ.name))
      | Method (id, fl, id2, exp) ->
          print_endline id.name;
          printf "%d\n" (List.length fl);
          List.iter (fun (f : formal) -> print_endline f.name.name) fl;
          print_endline "TODO: PRINT NAME OF CLASS WHERE METHOD IS DEFINED";
          print_expression exp
    in
    List.iter print selected

and print_parent_attributes c_class =
  let parent_name =
    match c_class.inherits with Some c -> c.name | None -> "i"
  in
  match Hashtbl.find_opt class_map parent_name with
  | Some c -> print_attributes c
  | None -> print_attributes (Hashtbl.find class_map "Object")

and print_implementation_map ast =
  printf "implementation_map\n";
  printf "%d\n" (List.length ast);
  List.iter
    (fun c_class ->
      printf "%s\n" c_class.typename.name;
      print_methods c_class)
    ast

and print_parent_map ast =
  printf "parent_map\n";
  printf "%d\n" (List.length ast - 1);
  let no_object_ast =
    List.filter (fun c_class -> not (c_class.typename.name = "Object")) ast
  in
  List.iter
    (fun c_class ->
      print_endline c_class.typename.name;
      match c_class.inherits with
      | Some inhrt -> print_endline inhrt.name
      | None -> print_endline "Object")
    no_object_ast

and print_annotated_ast ast =
  printf "%d\n" (List.length ast);
  List.iter print_class ast

and print_class c_class =
  print_identifier c_class.typename;
  (match c_class.inherits with
  | Some inhrt -> printf "inherits\n%s\n" inhrt.name
  | None -> print_endline "no_inherits");
  print_features c_class (fun _ -> true)

and print_features (c_class : cool_class) (predicate : feature -> bool) =
  let selected = List.filter predicate c_class.features in
  printf "%d\n" (List.length selected);
  if List.length selected > 0 then
    let print =
     fun feat ->
      match feat with
      | Attribute (name, typ, assign) -> (
          match assign with
          | None -> printf "no_initializer\n%s\n%s\n" name.name typ.name
          | Some exp ->
              printf "initializer\n%s\n%s\n" name.name typ.name;
              print_init_expression (exp, typ.name))
      | Method (id, fl, id2, exp) ->
          print_endline id.name;
          printf "%d\n" (List.length fl);
          List.iter (fun (f : formal) -> print_endline f.name.name) fl;
          print_endline "TODO: PRINT NAME OF CLASS WHERE METHOD IS DEFINED";
          print_expression exp
    in
    List.iter print selected

and print_methods (c_class : cool_class) =
  print_features c_class (function Method _ -> true | _ -> false)

and print_attributes (c_class : cool_class) =
  let attrs = get_all_attributes c_class in
  printf "%d\n" (List.length attrs);
  let print =
   fun feat ->
    match feat with
    | Attribute (name, typ, assign) -> (
        match assign with
        | None -> printf "no_initializer\n%s\n%s\n" name.name typ.name
        | Some exp ->
            printf "initializer\n%s\n%s\n" name.name typ.name;
            print_init_expression (exp, typ.name))
    | Method (id, fl, id2, exp) ->
        print_endline id.name;
        printf "%d\n" (List.length fl);
        List.iter (fun (f : formal) -> print_endline f.name.name) fl;
        print_endline "TODO: PRINT NAME OF CLASS WHERE METHOD IS DEFINED";
        print_expression exp
  in
  List.iter print attrs

and print_expression (exp : expr) =
  match exp with
  | Expression (id, sub) ->
      print_identifier id;
      print_sub_expr sub

and print_init_expression ((exp : expr), (typename : string)) =
  match exp with
  | Expression (id, sub) ->
      printf "%d\n%s\n%s\n" id.line_num typename id.name;
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
;;

let user_classes =
  List.init (int_of_string (read ())) (fun _ -> get_class ())
in
let () = List.iter add_class user_classes in
let ast =
  List.sort
    (fun c_class1 c_class2 ->
      String.compare c_class1.typename.name c_class2.typename.name)
    (user_classes @ default_classes)
in
print_class_map ast
