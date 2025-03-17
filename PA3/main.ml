let file_name = Sys.argv.(1)
let file = open_in file_name
let base_file_name = String.sub file_name 0 (String.length file_name - 8)
let out_file = open_out (base_file_name ^ ".cl-tac")
let read () = input_line file

let read_int () : int =
  let r = read () in
  try int_of_string r
  with c ->
    Printf.printf "%s\n" r;
    Printf.printf "---------------------ERRROR-------------------------\n";
    raise c

let rec parse_ast () = ()

type tac_elem = {
  operand : string;
  arg1 : string;
  arg2 : string;
  result : string;
}

and static_type =
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
  | c ->
      Printf.fprintf out_file "%s" c;
      assert false

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
    | c ->
        Printf.fprintf out_file "%s\n" c;
        assert false
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
    
and ast_method = {
  name : identifier;
  method_formals : formal list;
  type_name : identifier;
  method_expr : expr;
}
*)
let rec parse_class_map () : class_map_elem list =
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
    let features = get_feature_list class_name.name in
    { class_name; inherits; features }
  (* Get a feature (method or attribute) *)
  and get_formal () =
    let name = get_identifier () in

    let formal_type = get_identifier () in
    { name; formal_type }
  and get_formal_list () = List.init (read_int ()) (fun _ -> get_formal ())
  and get_feature (class_name : string) : feature =
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
  and get_feature_list (class_name : string) =
    List.init (read_int ()) (fun _ ->
        let f = get_feature class_name in
        f)
  in
  List.init (read_int ()) (fun _ ->
      let c = get_class () in
      c)

(* -------------------------------------------------------------------TAC CODE----------------------------------------------------------------------- *)

(* 
NOTE: Expression to Three-Address Code
"The traditional approach to converting expressions to three-address code involves a recursive descent traversal of the abstract syntax tree. The recursive descent traversal returns both a three-address code instruction as well as a list of additional instructions that should be prepended to the output."
*)

let var_ctr = ref 0
let label_ctr = ref 0

let rec tac_parse_expressions (ast : annotated_ast_elem list) =
  let get_tac_elem (ast_elem : annotated_ast_elem) =
    List.filter_map
      (fun feat ->
        match feat with
        | Method (id1, fl, id2, exp) ->
            (* Printf.fprintf out_file "Parsing expression: %s\n" exp.id.name; *)
            var_ctr := 0;
            Some
              ( "label " ^ ast_elem.class_name.name ^ "_" ^ id1.name ^ "_0",
                exp_to_tac exp.sub_expr (get_id !var_ctr) ast_elem.class_name.name id1.name)
        | Attribute (id1, id2, exp) -> None)
      ast_elem.features
  in
  List.map get_tac_elem ast |> List.flatten

and get_bool bool_val = match bool_val with True -> "true" | False -> "false"
and get_id n = "t$" ^ string_of_int n

and get_label n class_name method_name =
  method_name ^ "_" ^ class_name ^ "_" ^ string_of_int n

and exp_to_tac (exp : sub_expr) result cname mname : tac_elem list =
  match exp with
  | Assignment (id, exp) ->
      let result = get_id !var_ctr in
      var_ctr := !var_ctr + 1;
      let arg2 = get_id !var_ctr in
      exp_to_tac exp.sub_expr (get_id !var_ctr) cname mname
      @ [ { operand = "="; arg1 = id.name; arg2; result } ]
  (* | Dynamic_Dispatch (exp, id, el) -> ()
  | Static_Dispatch (exp, id1, id2, el) -> () *)
  | Self_Dispatch (id, exp_list) ->
      let result = get_id !var_ctr in
      let arg2 = get_id (!var_ctr + 1) in
      (List.map
         (fun elem ->
           (* Printf.fprintf out_file "Parsing expression: %s\n" elem.id.name;*)
           var_ctr := !var_ctr + 1;
           exp_to_tac elem.sub_expr (get_id !var_ctr) cname mname)
         exp_list
      |> List.flatten)
      @ [ { operand = "call"; arg1 = id.name; arg2; result } ]
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
      var_ctr := !var_ctr + 1;
      let condResult = get_id !var_ctr in
      var_ctr := !var_ctr + 1;
      let thenResult = get_id !var_ctr in
      var_ctr := !var_ctr + 1;
      let elseResult = get_id !var_ctr in
      let cond_tac = exp_to_tac pred_exp.sub_expr condResult cname mname in
      let then_tac = exp_to_tac then_exp.sub_expr thenResult cname mname in
      let else_tac = exp_to_tac else_exp.sub_expr elseResult cname mname in
      var_ctr := !var_ctr + 1;
      let jump_else_value = get_id !var_ctr in
      label_ctr := !label_ctr + 1;
      let then_label= get_label !label_ctr mname cname in
      label_ctr := !label_ctr + 1;
      let else_label = get_label !label_ctr mname cname in
      label_ctr := !label_ctr + 1;
      let join_label = get_label !label_ctr mname cname in
      let true_location = (List.hd (List.rev cond_tac)).result in
      cond_tac
      (* may be possible bug, may need to get the last value of cond_tac instead of result *)
      @ [ { operand = "not"; arg1 = true_location; arg2 = ""; result = jump_else_value } ]
      @ [ { operand = "bt"; arg1 = jump_else_value; arg2 = else_label; result } ]
      @ [ { operand = "bt"; arg1 = true_location; arg2 = then_label; result } ]
      @ [ { operand = "comment"; arg1 = "then branch"; arg2 = ""; result } ]
      @ [ { operand = "label"; arg1 = then_label; arg2 = ""; result } ]
      @ then_tac
      @ [ { operand = "jmp"; arg1 = join_label; arg2 = ""; result } ]
      @ [ { operand = "comment"; arg1 = "else branch"; arg2 = ""; result } ]
      @ [ { operand = "label"; arg1 = else_label; arg2 = ""; result } ]
      @ else_tac
      @ [ { operand = "jmp"; arg1 = join_label; arg2 = ""; result } ]
      @ [ { operand = "comment"; arg1 = "if-join"; arg2 = ""; result } ]
      @ [ { operand = "label"; arg1 = join_label; arg2 = ""; result } ]
  | While (pred_exp, body_exp) -> 
      var_ctr := !var_ctr + 1;
      let pred_result = get_id !var_ctr in
      var_ctr := !var_ctr + 1;
      let body_result = get_id !var_ctr in
      var_ctr := !var_ctr + 1;
      let cond_tac = exp_to_tac pred_exp.sub_expr pred_result cname mname in
      let body_tac = exp_to_tac body_exp.sub_expr body_result cname mname in
      var_ctr := !var_ctr + 1;
      let jump_else_value = get_id !var_ctr in
      label_ctr := !label_ctr + 1;
      let cond_label = get_label !label_ctr cname mname in
      label_ctr := !label_ctr + 1;
      let body_label = get_label !label_ctr cname mname in
      label_ctr := !label_ctr + 1;
      let join_label = get_label !label_ctr cname mname in
      let true_location = (List.hd (List.rev cond_tac)).result in
      [ { operand = "jmp"; arg1 = cond_label; arg2 = ""; result } ]
      @ [ { operand = "comment"; arg1 = "while-pred"; arg2 = ""; result } ]
      @ [ { operand = "label"; arg1 = cond_label; arg2 = ""; result } ]
      @ cond_tac
      @ [ { operand = "not"; arg1 = true_location; arg2 = ""; result = jump_else_value } ]
      @ [ { operand = "bt"; arg1 = jump_else_value; arg2 = join_label; result } ]
      @ [ { operand = "bt"; arg1 = true_location; arg2 = body_label; result } ]
      @ [ { operand = "comment"; arg1 = "while-body"; arg2 = ""; result } ]
      @ [ { operand = "label"; arg1 = body_label; arg2 = ""; result } ]
      @ body_tac
      @ [ { operand = "jmp"; arg1 = cond_label; arg2 = ""; result } ]
      @ [ { operand = "comment"; arg1 = "while-join"; arg2 = ""; result } ]
      @ [ { operand = "label"; arg1 = join_label; arg2 = ""; result } ]
  | Block exp_list ->
      List.map
        (fun elem ->
          (* Printf.fprintf out_file "Parsing expression: %s\n" elem.id.name;*)
          exp_to_tac elem.sub_expr (get_id !var_ctr) cname mname)
        exp_list
      |> List.flatten
  | New id ->
      [ { operand = "new"; arg1 = id.name; arg2 = ""; result = get_id !var_ctr } ]
  | Isvoid exp ->
      var_ctr := !var_ctr + 1;
      let arg1 = get_id !var_ctr in
      exp_to_tac exp.sub_expr arg1 cname mname
      @ [ { operand = "isvoid"; arg1; arg2 = ""; result } ]
  | Minus (exp, exp2) ->
      var_ctr := !var_ctr + 1;
      let arg1 = get_id !var_ctr in
      var_ctr := !var_ctr + 1;
      let arg2 = get_id !var_ctr in
      let exp1 = exp_to_tac exp.sub_expr arg1 cname mname in
      let exp2 = exp_to_tac exp2.sub_expr arg2 cname mname in
      exp1 @ exp2 @ [ { operand = "-"; arg1; arg2; result } ]
  | Divide (exp, exp2) ->
      var_ctr := !var_ctr + 1;
      let arg1 = get_id !var_ctr in
      var_ctr := !var_ctr + 1;
      let arg2 = get_id !var_ctr in
      let exp1 = exp_to_tac exp.sub_expr arg1 cname mname in
      let exp2 = exp_to_tac exp2.sub_expr arg2 cname mname in
      exp1 @ exp2 @ [ { operand = "/"; arg1; arg2; result } ]
  | Plus (exp, exp2) ->
      var_ctr := !var_ctr + 1;
      let arg1 = get_id !var_ctr in
      var_ctr := !var_ctr + 1;
      let arg2 = get_id !var_ctr in
      let exp1 = exp_to_tac exp.sub_expr arg1 cname mname in
      let exp2 = exp_to_tac exp2.sub_expr arg2 cname mname in
      exp1 @ exp2 @ [ { operand = "+"; arg1; arg2; result } ]
  | Times (exp, exp2) ->
      var_ctr := !var_ctr + 1;
      let arg1 = get_id !var_ctr in
      var_ctr := !var_ctr + 1;
      let arg2 = get_id !var_ctr in
      let exp1 = exp_to_tac exp.sub_expr arg1 cname mname in
      let exp2 = exp_to_tac exp2.sub_expr arg2 cname mname in
      exp1 @ exp2 @ [ { operand = "*"; arg1; arg2; result } ]
  | Equal (exp, exp2) ->
      var_ctr := !var_ctr + 1;
      let arg1 = get_id !var_ctr in
      var_ctr := !var_ctr + 1;
      let arg2 = get_id !var_ctr in
      let exp1 = exp_to_tac exp.sub_expr arg1 cname mname in
      let exp2 = exp_to_tac exp2.sub_expr arg2 cname mname in
      exp1 @ exp2 @ [ { operand = "="; arg1; arg2; result } ]
  | LessEqual (exp, exp2) ->
      var_ctr := !var_ctr + 1;
      let arg1 = get_id !var_ctr in
      var_ctr := !var_ctr + 1;
      let arg2 = get_id !var_ctr in
      let exp1 = exp_to_tac exp.sub_expr arg1 cname mname in
      let exp2 = exp_to_tac exp2.sub_expr arg2 cname mname in
      exp1 @ exp2 @ [ { operand = "<="; arg1; arg2; result } ]
  | LessThan (exp, exp2) ->
      var_ctr := !var_ctr + 1;
      let arg1 = get_id !var_ctr in
      var_ctr := !var_ctr + 1;
      let arg2 = get_id !var_ctr in
      let exp1 = exp_to_tac exp.sub_expr arg1 cname mname in
      let exp2 = exp_to_tac exp2.sub_expr arg2 cname mname in
      exp1 @ exp2 @ [ { operand = "<"; arg1; arg2; result } ]
  | Not exp ->
      var_ctr := !var_ctr + 1;
      exp_to_tac exp.sub_expr (get_id !var_ctr) cname mname
      @ [ { operand = "not"; arg1 = get_id !var_ctr; arg2 = ""; result } ]
  | Negate exp ->
      let result = get_id !var_ctr in
      var_ctr := !var_ctr + 1;
      exp_to_tac exp.sub_expr (get_id !var_ctr) cname mname
      @ [ { operand = "~"; arg1 = get_id !var_ctr; arg2 = ""; result } ]
  | Int_Constant i ->
      [ { operand = "int"; arg1 = string_of_int i; arg2 = ""; result } ]
  | String_Constant s ->
      [ { operand = Printf.sprintf "string\n%s" s; arg1 = ""; arg2 = ""; result } ]
  | Ident_Expr s ->
      [ { operand = "var"; arg1 = ""; arg2 = ""; result = get_id !var_ctr } ]
  | Boolean_Constant v ->
      [
        {
          operand = "bool";
          arg1 = get_bool v;
          arg2 = "";
          result = get_id !var_ctr;
        };
      ]
  (* | Let_Expr (binding_list, exp2) -> ()
  | Internal (classname, methodname, methodreturn) -> ()
  | Case (exp, elems) -> () *)
  | _ ->
      [
        {
          operand = "placeholder";
          arg1 = "placeholder";
          arg2 = "placeholder";
          result = "t9999";
        };
      ]

let print_tac_elems ((s, t) : string * tac_elem list) =
  let print_tac_elem t =
    if t.operand = "label" || t.operand = "jmp" || t.operand = "return" || t.operand = "comment" then
      Printf.fprintf out_file "%s %s\n" t.operand t.arg1
    else if t.operand = "bt" then
      Printf.fprintf out_file "bt %s %s\n" t.arg1 t.arg2
    else if t.arg2 = "" && t.arg1 = "" then
      Printf.fprintf out_file "%s <- %s\n" t.result t.operand
    else if t.arg2 = "" then
      Printf.fprintf out_file "%s <- %s %s\n" t.result t.operand t.arg1
    else
      Printf.fprintf out_file "%s <- %s %s %s\n" t.result t.operand t.arg1
        t.arg2
  in
  Printf.fprintf out_file "comment start\n";
  Printf.fprintf out_file "%s\n" s;
  List.iter print_tac_elem t;
  Printf.fprintf out_file "return t$0\n"

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

let () =
  (* let class_map = parse_class_map () in
  let implementation_map = parse_implementation_map () in
  let parent_map = parse_parent_map () in *)
  let _ = parse_class_map () in
  let _ = parse_implementation_map () in
  let _ = parse_parent_map () in
  let annotated_ast = parse_annotated_ast () in
  let tacs = tac_parse_expressions annotated_ast in
  print_tac_elems (List.hd tacs)
