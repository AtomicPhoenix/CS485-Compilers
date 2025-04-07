let file_name = Sys.argv.(1)
let file = open_in file_name
let base_file_name = String.sub file_name 0 (String.length file_name - 8)
let out_file = open_out (base_file_name ^ ".s")
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

type tac_elem = {
  operand : tac_operand;
  arg1 : string;
  arg2 : string;
  result : string;
}

and tac_operand =
  | Assignment
  | Bt
  | Call
  | Comment
  | Label
  | Jmp
  | Case
  | Default
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

(* 
NOTE: Expression to Three-Address Code
"The traditional approach to converting expressions to three-address code involves a recursive descent traversal of the abstract syntax tree. The recursive descent traversal returns both a three-address code instruction as well as a list of additional instructions that should be prepended to the output."
*)

let var_ctr = ref 0
let label_ctr = ref 0
let ret = ref 0
let class_map = Hashtbl.create 32
let letTable = Hashtbl.create 10

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

let operand_to_string (operand : tac_operand) : string =
  match operand with
  | Assignment -> "assignment"
  | Bt -> "bt"
  | Call -> "call"
  | Comment -> "comment"
  | Label -> "label"
  | Jmp -> "jmp"
  | Case -> "case"
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

let print_tac_elem t =
  match t.operand with
  | Label -> Printf.fprintf out_file "label %s\n" t.arg1
  | Jmp -> Printf.fprintf out_file "jmp %s\n" t.arg1
  | Return -> Printf.fprintf out_file "return %s\n" t.arg1
  | Comment -> Printf.fprintf out_file "comment %s\n" t.arg1
  | Bt -> Printf.fprintf out_file "bt %s %s\n" t.arg1 t.arg2
  | Assignment -> Printf.fprintf out_file "%s <- %s\n" t.result t.arg1
  | LetNoInit -> Printf.fprintf out_file "%s <- %s %s\n" t.result t.arg1 t.arg2
  | String_Constant -> Printf.fprintf out_file "%s <- %s\n" t.result t.arg1
  | _ ->
      if t.arg2 = "" && t.arg1 = "" then
        Printf.fprintf out_file "%s <- %s\n" t.result
          (operand_to_string t.operand)
      else if t.arg2 = "" then
        Printf.fprintf out_file "%s <- %s %s\n" t.result
          (operand_to_string t.operand)
          t.arg1
      else
        Printf.fprintf out_file "%s <- %s %s %s\n" t.result
          (operand_to_string t.operand)
          t.arg1 t.arg2

let print_tac_elems (t : tac_elem list) =
  List.iter
    (fun t ->
      if t.operand = Case then (
        Printf.fprintf out_file "";
        exit 1))
    t;
  List.iter print_tac_elem t

let rec parse_tac_expressions (ast : annotated_ast_elem list) :
    (tac_elem list * string * string * int) list =
  let get_tac_elem (ast_elem : annotated_ast_elem) =
    List.filter_map
      (fun (feat, _) ->
        match feat with
        | Method (id1, _, _, exp) ->
            (* Printf.printf "Parsing expression: %s in method %s in class %s\n"
              exp.id.name id1.name ast_elem.class_name.name;  *)
            var_ctr := 0;
            label_ctr := 0;
            let base_lst =
              [
                { operand = Comment; arg1 = "start"; arg2 = ""; result = "" };
                {
                  operand = Label;
                  arg1 = ast_elem.class_name.name ^ "_" ^ id1.name ^ "_0";
                  arg2 = "";
                  result = "";
                };
              ]
            in
            let exp_list =
              exp_to_tac exp.sub_expr (get_id !var_ctr) ast_elem.class_name.name
                id1.name
            in
            let rtrn =
              [ { operand = Return; arg1 = "t$0"; arg2 = ""; result = "" } ]
            in
            let temps = !var_ctr + 1 in
            Some
              ( base_lst @ exp_list @ rtrn,
                ast_elem.class_name.name,
                id1.name,
                temps )
        | Attribute _ -> None)
      (get_all_methods ast_elem)
  in
  List.map get_tac_elem ast |> List.flatten

and get_bool bool_val = match bool_val with True -> "true" | False -> "false"
and get_id n = "t$" ^ string_of_int n

and get_label n class_name method_name =
  class_name ^ "_" ^ method_name ^ "_" ^ string_of_int n

and exp_to_tac (exp : sub_expr) result cname mname : tac_elem list =
  match exp with
  | Assignment (id, exp) ->
      (* Printf.fprintf out_file "Assigning result of %s to %s\n" exp.id.name
        id.name; *)
      let var_id = Hashtbl.find_opt letTable id.name in
      let arg1 = match var_id with Some v -> v | None -> id.name in
      var_ctr := !var_ctr + 1;
      let last_var = exp_to_tac exp.sub_expr arg1 cname mname in
      var_ctr := !var_ctr + 1;
      last_var
      @ [ { operand = Assignment; arg1; arg2 = ""; result = get_id !var_ctr } ]
  | Dynamic_Dispatch (dispatch_exp, method_name, args) ->
      let arg2 =
        if List.length args > 0 then
          String.concat " "
            (List.mapi (fun i _ -> get_id (!var_ctr + 1 + i)) args)
        else ""
      in
      let arg_tacs =
        if List.length args > 0 then
          List.map
            (fun arg ->
              var_ctr := !var_ctr + 1;
              exp_to_tac arg.sub_expr (get_id !var_ctr) cname mname)
            args
          |> List.flatten
        else []
      in
      var_ctr := !var_ctr + 1;
      arg_tacs
      @ exp_to_tac dispatch_exp.sub_expr (get_id !var_ctr) cname mname
      @ [ { operand = Call; arg1 = method_name.name; arg2; result } ]
  | Static_Dispatch (dispatch_exp, _, method_name, args) ->
      let arg2 =
        if List.length args > 0 then
          String.concat " "
            (List.mapi (fun i _ -> get_id (!var_ctr + 1 + i)) args)
        else ""
      in
      let arg_tacs =
        if List.length args > 0 then
          List.map
            (fun arg ->
              var_ctr := !var_ctr + 1;
              exp_to_tac arg.sub_expr (get_id !var_ctr) cname mname)
            args
          |> List.flatten
        else []
      in
      var_ctr := !var_ctr + 1;
      arg_tacs
      @ exp_to_tac dispatch_exp.sub_expr (get_id !var_ctr) cname mname
      @ [ { operand = Call; arg1 = method_name.name; arg2; result } ]
  | Self_Dispatch (id, args) ->
      let arg2 =
        if List.length args > 0 then
          String.concat " "
            (List.mapi (fun i _ -> get_id (!var_ctr + 1 + i)) args)
        else ""
      in
      (List.map
         (fun elem ->
           (* Printf.fprintf out_file "Parsing expression: %s\n" elem.id.name;*)
           var_ctr := !var_ctr + 1;
           exp_to_tac elem.sub_expr (get_id !var_ctr) cname mname)
         args
      |> List.flatten)
      @ [ { operand = Call; arg1 = id.name; arg2; result } ]
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
      ret := !var_ctr;
      var_ctr := !var_ctr + 1;
      let condResult = get_id !var_ctr in
      let cond_tac = exp_to_tac pred_exp.sub_expr condResult cname mname in
      let then_tac = exp_to_tac then_exp.sub_expr (get_id !ret) cname mname in
      let else_tac = exp_to_tac else_exp.sub_expr (get_id !ret) cname mname in
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
          };
        ]
      @ [ { operand = Bt; arg1 = jump_else_value; arg2 = else_label; result } ]
      (* @ [ { operand = Bt; arg1 = true_location; arg2 = then_label; result } ] *)
      @ [ { operand = Comment; arg1 = "then branch"; arg2 = ""; result } ]
      @ [ { operand = Label; arg1 = then_label; arg2 = ""; result } ]
      @ then_tac
      @ [ { operand = Jmp; arg1 = join_label; arg2 = ""; result } ]
      @ [ { operand = Comment; arg1 = "else branch"; arg2 = ""; result } ]
      @ [ { operand = Label; arg1 = else_label; arg2 = ""; result } ]
      @ else_tac
      @ [ { operand = Jmp; arg1 = join_label; arg2 = ""; result } ]
      @ [ { operand = Comment; arg1 = "if-join"; arg2 = ""; result } ]
      @ [ { operand = Label; arg1 = join_label; arg2 = ""; result } ]
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
      let join_label = get_label !label_ctr cname mname in
      label_ctr := !label_ctr + 1;
      let body_label = get_label !label_ctr cname mname in
      let true_location = (List.hd (List.rev cond_tac)).result in
      [ { operand = Jmp; arg1 = cond_label; arg2 = ""; result } ]
      @ [ { operand = Comment; arg1 = "while-pred"; arg2 = ""; result } ]
      @ [ { operand = Label; arg1 = cond_label; arg2 = ""; result } ]
      @ cond_tac
      @ [
          {
            operand = Not;
            arg1 = true_location;
            arg2 = "";
            result = jump_else_value;
          };
        ]
      @ [ { operand = Bt; arg1 = jump_else_value; arg2 = join_label; result } ]
      @ [ { operand = Bt; arg1 = true_location; arg2 = body_label; result } ]
      @ [ { operand = Comment; arg1 = "while-body"; arg2 = ""; result } ]
      @ [ { operand = Label; arg1 = body_label; arg2 = ""; result } ]
      @ body_tac
      @ [ { operand = Jmp; arg1 = cond_label; arg2 = ""; result } ]
      @ [ { operand = Comment; arg1 = "while-join"; arg2 = ""; result } ]
      @ [ { operand = Label; arg1 = join_label; arg2 = ""; result } ]
      @ [ { operand = Default; arg1 = "Object"; arg2 = ""; result } ]
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
          exp_to_tac elem.sub_expr elem_result cname mname)
        exp_list
      |> List.flatten
  | New id ->
      [ { operand = New; arg1 = id.name; arg2 = ""; result = get_id !var_ctr } ]
  | Isvoid exp ->
      var_ctr := !var_ctr + 1;
      let arg1 = get_id !var_ctr in
      exp_to_tac exp.sub_expr arg1 cname mname
      @ [ { operand = Isvoid; arg1; arg2 = ""; result } ]
  | Minus (exp, exp2) ->
      var_ctr := !var_ctr + 1;
      let arg1 = get_id !var_ctr in
      var_ctr := !var_ctr + 1;
      let arg2 = get_id !var_ctr in
      let exp1 = exp_to_tac exp.sub_expr arg1 cname mname in
      let exp2 = exp_to_tac exp2.sub_expr arg2 cname mname in
      exp1 @ exp2 @ [ { operand = Minus; arg1; arg2; result } ]
  | Divide (exp, exp2) ->
      var_ctr := !var_ctr + 1;
      let arg1 = get_id !var_ctr in
      var_ctr := !var_ctr + 1;
      let arg2 = get_id !var_ctr in
      let exp1 = exp_to_tac exp.sub_expr arg1 cname mname in
      let exp2 = exp_to_tac exp2.sub_expr arg2 cname mname in
      exp1 @ exp2 @ [ { operand = Divide; arg1; arg2; result } ]
  | Plus (exp, exp2) ->
      var_ctr := !var_ctr + 1;
      let arg1 = get_id !var_ctr in
      var_ctr := !var_ctr + 1;
      let arg2 = get_id !var_ctr in
      let exp1 = exp_to_tac exp.sub_expr arg1 cname mname in
      let exp2 = exp_to_tac exp2.sub_expr arg2 cname mname in
      exp1 @ exp2 @ [ { operand = Plus; arg1; arg2; result } ]
  | Times (exp, exp2) ->
      var_ctr := !var_ctr + 1;
      let arg1 = get_id !var_ctr in
      var_ctr := !var_ctr + 1;
      let arg2 = get_id !var_ctr in
      let exp1 = exp_to_tac exp.sub_expr arg1 cname mname in
      let exp2 = exp_to_tac exp2.sub_expr arg2 cname mname in
      exp1 @ exp2 @ [ { operand = Times; arg1; arg2; result } ]
  | Equal (exp, exp2) ->
      var_ctr := !var_ctr + 1;
      let arg1 = get_id !var_ctr in
      var_ctr := !var_ctr + 1;
      let arg2 = get_id !var_ctr in
      let exp1 = exp_to_tac exp.sub_expr arg1 cname mname in
      let exp2 = exp_to_tac exp2.sub_expr arg2 cname mname in
      exp1 @ exp2 @ [ { operand = Equal; arg1; arg2; result } ]
  | LessEqual (exp, exp2) ->
      var_ctr := !var_ctr + 1;
      let arg1 = get_id !var_ctr in
      var_ctr := !var_ctr + 1;
      let arg2 = get_id !var_ctr in
      let exp1 = exp_to_tac exp.sub_expr arg1 cname mname in
      let exp2 = exp_to_tac exp2.sub_expr arg2 cname mname in
      exp1 @ exp2 @ [ { operand = LessEqual; arg1; arg2; result } ]
  | LessThan (exp, exp2) ->
      var_ctr := !var_ctr + 1;
      let arg1 = get_id !var_ctr in
      var_ctr := !var_ctr + 1;
      let arg2 = get_id !var_ctr in
      let exp1 = exp_to_tac exp.sub_expr arg1 cname mname in
      let exp2 = exp_to_tac exp2.sub_expr arg2 cname mname in
      exp1 @ exp2 @ [ { operand = LessThan; arg1; arg2; result } ]
  | Not exp ->
      var_ctr := !var_ctr + 1;
      exp_to_tac exp.sub_expr (get_id !var_ctr) cname mname
      @ [ { operand = Not; arg1 = get_id !var_ctr; arg2 = ""; result } ]
  | Negate exp ->
      let result = get_id !var_ctr in
      var_ctr := !var_ctr + 1;
      exp_to_tac exp.sub_expr (get_id !var_ctr) cname mname
      @ [ { operand = Negate; arg1 = get_id !var_ctr; arg2 = ""; result } ]
  | Int_Constant i ->
      [ { operand = Int_Constant; arg1 = string_of_int i; arg2 = ""; result } ]
  | String_Constant s ->
      [
        {
          operand = String_Constant;
          arg1 = Printf.sprintf "%s" s;
          arg2 = "";
          result;
        };
      ]
  | Ident_Expr s -> (
      match Hashtbl.find_opt letTable s.name with
      | Some t ->
          (* Printf.fprintf out_file "Retrieved variable %s as temp %s\n" s.name t; *)
          [ { operand = Ident_Expr t; arg1 = ""; arg2 = ""; result } ]
      | None ->
          [ { operand = Ident_Expr s.name; arg1 = ""; arg2 = ""; result } ])
  | Boolean_Constant v ->
      [ { operand = Boolean_Constant; arg1 = get_bool v; arg2 = ""; result } ]
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
            | Some value -> exp_to_tac value.sub_expr result cname mname
            | None ->
                [
                  {
                    operand = LetNoInit;
                    arg1 = "default";
                    arg2 = let_type.name;
                    result;
                  };
                ])
          binding_list
        |> List.flatten
      in
      b @ exp_to_tac exp.sub_expr result cname mname
  | Case _ -> [ { operand = Case; arg1 = ""; arg2 = ""; result = "" } ]
  | Internal _ ->
      Printf.fprintf out_file
        "Something is fundamentally wrong (We should not be parsing Internal \
         to TAC)";
      assert false
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

let print_methods (ast_elem : annotated_ast_elem) =
  Printf.printf "%s\n" ast_elem.class_name.name;
  List.iter
    (fun (feat, _) ->
      match feat with
      | Method (name, _, _, _) -> Printf.printf "\t%s\n" name.name
      | Attribute _ -> ())
    (get_all_methods ast_elem)

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
  | Bt | Call | Jmp | Case | Default | Return -> true
  | _ -> false

let tac_to_cfg (tacs, class_name, method_name, temp_count) :
    cfg * string * string * int =
  let rec create_cfg (tac_list : tac_elem list) acc cfg =
    match tac_list with
    | tac :: tail -> (
        match is_break_point tac with
        | true -> create_cfg tail [] ([ List.rev (tac :: acc) ] @ cfg)
        | false -> create_cfg tail (tac :: acc) cfg)
    | [] -> [ List.rev acc ] @ cfg
  in
  (List.rev (create_cfg tacs [] []), class_name, method_name, temp_count)

and print_cfg bbl = List.iter (List.iter print_tac_elem) bbl
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

let var_locations = Hashtbl.create 32

(* let string_map = Hashtbl.create 32*)
let string_map = Hashtbl.create 32
let class_id_map = Hashtbl.create 32
let class_vtable_map = Hashtbl.create 32
let string_counter = ref 5
let class_tag_ctr = ref 9
let class_map = parse_class_map ()
let implementation_map = parse_implementation_map ()
let parent_map = parse_parent_map ()
let vtable_list : vtable list ref = ref []

let create_vtable (itm : implementation_map_elem) : vtable =
  let name = itm.name in
  let funcs =
    List.map
      (fun (meth : imp_method) ->
        { type_name = meth.type_name; method_name = meth.name })
      itm.methods
  in
  string_counter := !string_counter + 1;
  Hashtbl.add string_map itm.name !string_counter;
  { name_id = name; name_string_id = !string_counter; methods = funcs }

let create_vtables () =
  let tables = List.map create_vtable implementation_map in
  List.iter
    (fun i ->
      Hashtbl.add class_vtable_map i.name_id i;
      vtable_list := !vtable_list @ [ i ])
    tables

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
          (*{ type_name = "Object"; method_name = "abort" };*)
          (*{ type_name = "Object"; method_name = "copy" };*)
          (*{ type_name = "Object"; method_name = "type_name" };*)
        ];
    };
    {
      name_id = "IO";
      name_string_id = 1;
      methods =
        [
          { type_name = "IO"; method_name = ".new" };
          (*{ type_name = "Object"; method_name = "abort" };*)
          (*{ type_name = "Object"; method_name = "copy" };*)
          (*{ type_name = "Object"; method_name = "type_name" };*)
          { type_name = "IO"; method_name = "in_int" };
          (*{ type_name = "IO"; method_name = "in_string" };*)
          { type_name = "IO"; method_name = "out_int" };
          (*{ type_name = "IO"; method_name = "out_string" };*)
        ];
    };
    {
      name_id = "Int";
      name_string_id = 2;
      methods =
        [
          { type_name = "Int"; method_name = ".new" };
          (*{ type_name = "Object"; method_name = "abort" };*)
          (*{ type_name = "Object"; method_name = "copy" };*)
          (*{ type_name = "Object"; method_name = "type_name" };*)
        ];
    };
    {
      name_id = "Object";
      name_string_id = 3;
      methods =
        [
          { type_name = "Object"; method_name = ".new" };
          (*{ type_name = "Object"; method_name = "abort" };*)
          (*{ type_name = "Object"; method_name = "copy" };*)
          (*{ type_name = "Object"; method_name = "type_name" };*)
        ];
    };
    {
      name_id = "String";
      name_string_id = 4;
      methods =
        [
          { type_name = "String"; method_name = ".new" };
          (*{ type_name = "Object"; method_name = "abort" };*)
          (*{ type_name = "Object"; method_name = "copy" };*)
          (*{ type_name = "Object"; method_name = "type_name" };*)
          (*{ type_name = "String"; method_name = "concat" };*)
          (*{ type_name = "String"; method_name = "length" };*)
          (*{ type_name = "String"; method_name = "substr" };*)
        ];
    };
  ]

let get_class_attributes attrs =
  let get_attribute i (attr : ast_attribute) =
    let name = attr.name in
    let index = i in
    let typename = attr.type_name in
    let expr = attr.attr_expr in
    { field_name = name; index; type_name = typename; expression = expr }
  in
  List.mapi get_attribute attrs

let make_asm_class (c : class_map_elem) =
  let tag = !class_tag_ctr in
  class_tag_ctr := !class_tag_ctr + 1;
  Hashtbl.add class_id_map c.name tag;
  (* 8 bytes/64 bits since every attribute is a pointer *)
  let siz = List.length c.attrs in
  let class_vtable = Hashtbl.find class_vtable_map c.name in
  let attrs = get_class_attributes c.attrs in
  {
    class_tag = tag;
    object_size = siz;
    vtable = class_vtable;
    attributes = attrs;
  }

(*let abort =*)
(*[*)
(*Line "\t.globl\tObject.abort";*)
(*Line "Object.abort:";*)
(*Instruction ("pushq", "%rbp", "", "");*)
(*Instruction ("movq", "%rsp", "%rbp", "");*)
(*Instruction ("movq", "16(%rbp)", "%r12", "");*)
(*Instruction ("movq", "$16", "%r14", "");*)
(*Instruction ("subq", "%r14", "%rsp", "");*)
(*Instruction ("movq", "$string8", "%r13", "");*)
(*Instruction ("andq", "$0xFFFFFFFFFFFFFFF0", "%rsp", "");*)
(*Instruction ("movq", "%r13", "%rdi", "");*)
(*Instruction ("call", "cooloutstr", "", "");*)
(*Instruction ("andq", "$0xFFFFFFFFFFFFFFF0", "%rsp", "");*)
(*Instruction ("movl", "$0", "%edi", "");*)
(*Instruction ("call", "exit", "", "");*)
(*Line "Object.abort.end:";*)
(*Instruction ("movq", "%rbp", "%rsp", "");*)
(*Instruction ("popq", "%rbp", "", "");*)
(*Instruction ("ret", "", "", "");*)
(*]*)

(*let copy =*)
(*[*)
(*Line "\t.p2align 4";*)
(*Line "\t.globl\tObject.copy";*)
(*Line "Object.copy:";*)
(*Instruction ("pushq", "%rbp", "", "");*)
(*Instruction ("movq", "%rsp", "%rbp", "");*)
(*Instruction ("movq", "16(%rbp)", "%r12", "");*)
(*Instruction ("movq", "$16", "%r14", "");*)
(*Instruction ("subq", "%r14", "%rsp", "");*)
(*Instruction ("movq", "8(%r12)", "%r14", "");*)
(*Instruction ("andq", "$0xFFFFFFFFFFFFFFF0", "%rsp", "");*)
(*Instruction ("movq", "$8", "%rsi", "");*)
(*Instruction ("movq", "%r14", "%rdi", "");*)
(*Instruction ("call", "calloc", "", "");*)
(*Instruction ("movq", "%rax", "%r13", "");*)
(*Instruction ("pushq", "%r13", "", "");*)
(*Line "\t.globl\tObject.copy.end";*)
(*Line "Object.copy.end:";*)
(*Instruction ("movq", "%rbp", "%rsp", "");*)
(*Instruction ("popq", "%rbp", "", "");*)
(*Instruction ("ret", "", "", "");*)
(*]*)

(*let objecttypename =*)
(*[*)
(*Line "Object.type_name:";*)
(*Line "\t.globl\tObject.type_name";*)
(*Instruction ("pushq", "%rbp", "", "");*)
(*Instruction ("movq", "%rsp", "%rbp", "");*)
(*Instruction ("movq", "16(%rbp)", "%r12", "");*)
(*Instruction ("movq", "$16", "%r14", "");*)
(*Instruction ("subq", "%r14", "%rsp", "");*)
(*Instruction ("pushq", "%rbp", "", "");*)
(*Instruction ("pushq", "%r12", "", "");*)
(*Instruction ("movq", "$String..new", "%r14", "");*)
(*Instruction ("call", "*%r14", "", "");*)
(*Instruction ("popq", "%r12", "", "");*)
(*Instruction ("popq", "%rbp", "", "");*)
(*Instruction ("movq", "16(%r12)", "%r14", "");*)
(*Instruction ("movq", "0(%r14)", "%r14", "");*)
(*Instruction ("movq", "%r14", "24(%r13)", "");*)
(*Line "Object.type_name.end:";*)
(*Instruction ("movq", "%rbp", "%rsp", "");*)
(*Instruction ("popq", "%rbp", "", "");*)
(*Instruction ("ret", "", "", "");*)
(*]*)

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
    Instruction ("movq", "8(%rsp)", "%rax", "");
    Instruction ("movl", "$2147483648", "%edx", "");
    Instruction ("movl", "$4294967295", "%ecx", "");
    Instruction ("addq", "%rax", "%rdx", "");
    Instruction ("cmpq", "%rdx", "%rcx", "");
    Instruction ("movl", "$0", "%edx", "");
    Instruction ("cmovb", "%rdx", "%rax", "");
    Instruction ("movq", "%rax", "24(%rbx)", "");
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
    Instruction ("subq", "$8", "%rsp", "");
    Instruction ("movq", "24(%rsi)", "%rsi", "");
    Instruction ("movq", "%rdi", "%rbx", "");
    Instruction ("xorl", "%eax", "%eax", "");
    Instruction ("movq", "$percent.d", "%rdi", "");
    Instruction ("call", "printf", "", "");
    Instruction ("movq", "%rbx", "%rax", "");
    Instruction ("addq", "$8", "%rsp", "");
    Instruction ("popq", "%rbx", "", "");
    Instruction ("ret", "", "", "");
    (*Instruction (".size", "IO.out_int", ".-IO.out_int", "");*)
  ]

(*let out_string =*)
(*[*)
(*Line "\t.p2align 4";*)
(*Line "\t.globl\tIO.out_string";*)
(*Line "IO.out_string:";*)
(*Instruction ("pushq", "%rbp", "", "");*)
(*Instruction ("movq", "%rsp", "%rbp", "");*)
(*Instruction ("movq", "16(%rbp)", "%r12", "");*)
(*Instruction ("movq $16, %r14", "", "", "");*)
(*Instruction ("subq %r14, %rsp", "", "", "");*)
(*Instruction ("movq 24(%rbp), %r14", "", "", "");*)
(*Instruction ("movq 24(%r14), %r13", "", "", "");*)
(*Instruction ("andq $0xFFFFFFFFFFFFFFF0, %rsp", "", "", "");*)
(*Instruction ("movq %r13, %rdi", "", "", "");*)
(*Instruction ("call cooloutstr", "", "", "");*)
(*Instruction ("movq %r12, %r13", "", "", "");*)
(*Instruction ("movq %rbp, %rsp", "", "", "");*)
(*Instruction ("popq %rbp", "", "", "");*)
(*Instruction ("ret", "", "", "");*)
(*]*)

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

let intrinsic_funcs = [ in_int; out_int (*; out_string; cooloutstr *) ]

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
    Instruction ("setge", "%al", "", "");
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
    Instruction ("setg", "%dl", "", "");
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
    Instruction ("setne", "%dl", "", "");
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
  Hashtbl.add var_locations var_name fp_offset

let get_var_addr (var_name : string) : string =
  match Hashtbl.find_opt var_locations var_name with
  | Some addr -> Printf.sprintf "-%d(%%rbp)" addr
  | None -> var_name

let pusha =
  [
    Instruction ("pushq", "%rax", "", "");
    Instruction ("pushq", "%rbx", "", "");
    Instruction ("pushq", "%rbp", "", "");
    Instruction ("pushq", "%rdi", "", "");
    Instruction ("pushq", "%rsi", "", "");
    Instruction ("pushq", "%rcx", "", "");
    Instruction ("pushq", "%rdx", "", "");
    Instruction ("pushq", "%r8", "", "");
    Instruction ("pushq", "%r9", "", "");
    Instruction ("pushq", "%r10", "", "");
    Instruction ("pushq", "%r11", "", "");
    Instruction ("pushq", "%r12", "", "");
    Instruction ("pushq", "%r13", "", "");
    Instruction ("pushq", "%r14", "", "");
    (*Instruction ("pushq", "%r15", "", "");*)
  ]

let popa =
  [
    (*Instruction ("popq", "%r15", "", "");*)
    Instruction ("popq", "%r14", "", "");
    Instruction ("popq", "%r13", "", "");
    Instruction ("popq", "%r12", "", "");
    Instruction ("popq", "%r11", "", "");
    Instruction ("popq", "%r10", "", "");
    Instruction ("popq", "%r9", "", "");
    Instruction ("popq", "%r8", "", "");
    Instruction ("popq", "%rdx", "", "");
    Instruction ("popq", "%rcx", "", "");
    Instruction ("popq", "%rsi", "", "");
    Instruction ("popq", "%rdi", "", "");
    Instruction ("popq", "%rbp", "", "");
    Instruction ("popq", "%rbx", "", "");
    Instruction ("popq", "%rax", "", "");
  ]

(** Method to convert a TAC element to assembly code *)
let tac_to_as (tac : tac_elem) cur_method =
  match tac.operand with
  (****************** TODO ******************)
  | Assignment ->
      add_var_addr tac.result;
      let result = get_var_addr tac.result in
      let arg1 = get_var_addr tac.arg1 in
      [
        Line ("\t#Assignment start");
        Instruction ("movq", arg1, "%rax", "");
        Instruction ("movq", "%rax", result, "");
        Line ("\t#Assignment end");
      ]
  | Bt ->
      let arg1 = get_var_addr tac.arg1 in
      [
        Line ("\t#Branch True start");
        Instruction ("movq", arg1, "%rax", "");
        Instruction ("testq", "%rax", "%rax", "");
        Instruction ("jne", tac.arg2, "", "");
        Line ("\t#Branch True end");
      ]
  | Call ->
      add_var_addr tac.result;
      let result = get_var_addr tac.result in
      if tac.arg2 = "" then
            [Line ("\t#Call w/ args start");] @
        pusha
        @ [
            (*Instruction ("andq", "$0xFFFFFFFFFFFFFFF0", "%rsp", "");*)
            Instruction ("call", "IO." ^ tac.arg1, "", "");
            Instruction ("movq", "%rax", result, "");
          ]
        @ popa @
            [Line ("\t#Call w/ args end");]
      else
        (*let args = String.split_on_char ' ' tac.arg2 in*)
        (*let arglist =*)
        (*List.fold_left*)
        (*(fun acc itm -> acc @ [ Instruction ("pushq", itm, "", "") ])*)
        (*[] args*)
        let arglist =
          [ Instruction ("movq", get_var_addr tac.arg2, "%rsi", "") ]
        in
            [Line ("\t#Call w/ args start");] @
        pusha @ arglist
        @ [
            (*Instruction ("andq", "$0xFFFFFFFFFFFFFFF0", "%rsp", "");*)
            Instruction ("call", "IO." ^ tac.arg1, "", "");
            Instruction ("movq", "%rax", result, "");
          ]
        @ popa @
            [Line ("\t#Call w/ args end");]
      (* Push all variables onto stack *)
      (* Push all onto stack *)
      (*[Instruction{instruction = "callq"; arg1 = Some tac.arg1; arg2 = ""; arg3 = ""}]*)
      (*Printf.fprintf out_file "\tcallq %s\n" tac.arg1*)
  | Comment ->
            [Line ("\t#Comment start")] @
      [ Line ("#" ^ tac.arg1) ] @
            [Line ("\t#Comment end")]
      (*Printf.sprintf out_file "\t%s\n" ("#" ^ tac.arg1)*)
  | Label -> [Line ("\t#Label")] @[ Line (Printf.sprintf "%s:" tac.arg1) ]
  | Jmp -> [Line ("\t#Jump")] @[ Instruction ("jmp", tac.arg1, "", "") ]
  (* | Case *)
  (* | Default *)
  (****************** TODO ******************)
  | Return ->
      [
        (*Instruction ("movq", "%rbp", "%rsp", "");*)
        (*Instruction ("popq", "%rbp", "%rsp", "");*)
        (*Instruction ("ret", "", "", "");*)
            Line ("\t#Return start");
        Instruction ("jmp", "." ^ cur_method ^ ".end", "", "");
            Line ("\t#Return end");
      ]
  | LetNoInit ->
      add_var_addr tac.result;
      let result = get_var_addr tac.result in
      [
        (*Instruction ("movq", "$0", result, "");*)
            Line ("\t#Let No Init start");
        Instruction ("pushq", "%rbp", "", "");
        Instruction ("pushq", "%rax", "", "");
        Instruction ("call", tac.arg2 ^ "..new", "", "");
        Instruction ("movq", "%rax", "%r10", "");
        Instruction ("popq", "%rax", "", "");
        Instruction ("popq", "%rbp", "", "");
        Instruction ("movq", "%r10", result, "");
            Line ("\t#Let No Init end");
      ]
  (****************** TODO ******************)
  | Ident_Expr s ->
      add_var_addr tac.result;
      let result = get_var_addr tac.result in
      [
            Line ("\t#Ident Expr start");
        Instruction ("movq", get_var_addr s, "%rax", "");
        Instruction ("movq", "%rax", result, "");
            Line ("\t#Ident Expr end");
      ]
  (*| New *)
  (* | Isvoid *)
  | Plus ->
      let arg1 = get_var_addr tac.arg1 in
      let arg2 = get_var_addr tac.arg2 in
      add_var_addr tac.result;
      let result = get_var_addr tac.result in

      [
            Line ("\t#Plus start");
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
            Line ("\t#Plus end");
      ]
  | Minus ->
      let arg1 = get_var_addr tac.arg1 in
      let arg2 = get_var_addr tac.arg2 in
      add_var_addr tac.result;
      let result = get_var_addr tac.result in

      [
            Line ("\t#Minus start");
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
            Line ("\t#Minus end");
      ]
  | Divide ->
      let arg1 = get_var_addr tac.arg1 in
      let arg2 = get_var_addr tac.arg2 in
      add_var_addr tac.result;
      let result = get_var_addr tac.result in

      [
            Line ("\t#Divide start");
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
            Line ("\t#Divide end");
      ]
  | Times ->
      let arg1 = get_var_addr tac.arg1 in
      let arg2 = get_var_addr tac.arg2 in
      add_var_addr tac.result;
      let result = get_var_addr tac.result in
      [
            Line ("\t#Times start");
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
            Line ("\t#Times end");
      ]
  | LessThan ->
      let arg1 = get_var_addr tac.arg1 in
      let arg2 = get_var_addr tac.arg2 in
      add_var_addr tac.result;
      let result = get_var_addr tac.result in

      [
            Line ("\t#Less Than start");
        Instruction ("pushq", "%rdi", "", "");
        Instruction ("pushq", "%rsi", "", "");
        Instruction ("movq", arg1, "%rdi", "");
        Instruction ("movq", arg2, "%rsi", "");
        Instruction ("call", "lt_handler", "", "");
        Instruction ("popq", "%rsi", "", "");
        Instruction ("popq", "%rdi", "", "");
        Instruction ("movq", "%rax", result, "");
            Line ("\t#Less Than end");
      ]
  | LessEqual ->
      let arg1 = get_var_addr tac.arg1 in
      let arg2 = get_var_addr tac.arg2 in
      add_var_addr tac.result;
      let result = get_var_addr tac.result in

      [
            Line ("\t#Less Equal start");
        Instruction ("pushq", "%rdi", "", "");
        Instruction ("pushq", "%rsi", "", "");
        Instruction ("movq", arg1, "%rdi", "");
        Instruction ("movq", arg2, "%rsi", "");
        Instruction ("call", "le_handler", "", "");
        Instruction ("popq", "%rsi", "", "");
        Instruction ("popq", "%rdi", "", "");
        Instruction ("movq", "%rax", result, "");
            Line ("\t#Less Equal end");
      ]
  | Equal ->
      let arg1 = get_var_addr tac.arg1 in
      let arg2 = get_var_addr tac.arg2 in
      add_var_addr tac.result;
      let result = get_var_addr tac.result in

      [
            Line ("\t#Equal start");
        Instruction ("pushq", "%rdi", "", "");
        Instruction ("pushq", "%rsi", "", "");
        Instruction ("movq", arg1, "%rdi", "");
        Instruction ("movq", arg2, "%rsi", "");
        Instruction ("call", "eq_handler", "", "");
        Instruction ("popq", "%rsi", "", "");
        Instruction ("popq", "%rdi", "", "");
        Instruction ("movq", "%rax", result, "");
            Line ("\t#Equal end");
      ]
  | Not ->
      let arg1 = get_var_addr tac.arg1 in
      add_var_addr tac.result;
      let result = get_var_addr tac.result in

      [
            Line ("\t#Not start");
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
            Line ("\t#Not end");
      ]
  | Negate ->
      let arg1 = get_var_addr tac.arg1 in
      add_var_addr tac.result;
      let result = get_var_addr tac.result in

      [
            Line ("\t#Negate start");
        Instruction ("movq", arg1, "%rax", "");
        Instruction ("movq", "24(%rax)", "%rax", "");
        Instruction ("notq", "%rax", "", "");
        Instruction ("pushq", "%rbp", "", "");
        Instruction ("pushq", "%rax", "", "");
        Instruction ("call", "Int..new", "", "");
        Instruction ("movq", "%rax", "%r10", "");
        Instruction ("popq", "%rax", "", "");
        Instruction ("popq", "%rbp", "", "");
        Instruction ("movq", "%rax", "24(%r10)", "");
        Instruction ("movq", "%r10", result, "");
            Line ("\t#Negate end");
      ]
  | Int_Constant ->
      add_var_addr tac.result;
      let result = get_var_addr tac.result in
      [
            Line ("\t#iconst start");
        Instruction ("call", "Int..new", "", "");
        Instruction ("movq", "$" ^ tac.arg1, "24(%rax)", "");
        Instruction ("movq", "%rax", result, "");
            Line ("\t#iconst end");
      ]
      (****************** TODO ******************)
  | String_Constant -> (
      add_var_addr tac.result;
      let result = get_var_addr tac.result in
      match Hashtbl.find_opt string_map tac.arg1 with
      | Some str_id ->
          [
            Line ("\t#sconst start");
            Instruction ("call", "String..new", "", "");
            Instruction
              ("movq", "$string" ^ string_of_int str_id, "24(%rax)", "");
            Instruction ("movq", "%rax", result, "");
            Line ("\t#sconst end");
            (*Instruction ("movq", "$.string" ^ string_of_int str_id, result, "");*)
          ]
      | None ->
          string_counter := !string_counter + 1;
          Hashtbl.add string_map tac.arg1 !string_counter;

          [
            Line ("\t#sconst start");
            Instruction ("call", "String..new", "", "");
            Instruction
              ("movq", "$string" ^ string_of_int !string_counter, "24(%rax)", "");
            Instruction ("movq", "%rax", result, "");
            Line ("\t#sconst end");
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
            Line ("\t#bconst start");
          Instruction ("call", "Bool..new", "", "");
          Instruction ("movq", "$1", "24(%rax)", "");
          Instruction ("movq", "%rax", result, "");
            Line ("\t#bconst end");
        ]
      else
        [
            Line ("\t#bconst start");
          Instruction ("call", "Bool..new", "", "");
          Instruction ("movq", "$0", "24(%rax)", "");
          Instruction ("movq", "%rax", result, "");
            Line ("\t#bconst end");
        ]
  | _ -> assert false

let tac_list_to_asm lst = List.map tac_to_as lst

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

let () =
  let vtables = create_default_vtables () @ !vtable_list in
  let print_vtable (table : vtable) =
    let name = table.name_id in
    let strid = table.name_string_id in
    Printf.fprintf out_file ".globl %s..vtable\n" name;
    Printf.fprintf out_file "%s..vtable:\n" name;
    Printf.fprintf out_file "\t.quad string%d\n" strid;
    List.iter
      (fun (func : vtable_func) ->
        Printf.fprintf out_file "\t.quad %s.%s\n" func.type_name
          func.method_name)
      table.methods;
    Printf.fprintf out_file
      "\t#;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;\n"
  in
  List.iter print_vtable vtables;
  let print_new_funcs funcs =
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
  in
  print_new_funcs new_funcs;
  List.iter
    (fun func ->
      List.iter (fun f -> print_asm f) func;
      Printf.fprintf out_file
        "\t#;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;\n")
    intrinsic_funcs;

  (* let class_map = parse_class_map () in
  let implementation_map = parse_implementation_map () in
  let parent_map = parse_parent_map () in  *)
  let annotated_ast = parse_annotated_ast () in
  List.iter add_class default_classes;
  List.iter add_class annotated_ast;

  (* A List of basic blocks *)
  (* A list of list of tac elems *)
  let tacs = parse_tac_expressions annotated_ast in
  (* List.iter (fun (f, _, _, _) -> print_tac_elems f) tacs;*)

  (* A cfg list *)
  (* A list of list of basic blocks *)
  (* A tac_elem list list list *)
  let cfg_list = List.map tac_to_cfg tacs in

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
        @ (List.map (fun tac -> tac_to_as tac method_name) method_tac
          |> List.flatten)
          (* @ [Line (Printf.sprintf "\t.size\t%s, .-%s" name name)]*)
        @ get_end_method_boilerplate method_name stack_space)
      cfg_list
  in
  List.iter (List.iter print_asm) method_asm;
  Printf.fprintf out_file "\t.section\t.rodata\n";
  Hashtbl.iter
    (fun k v ->
      Printf.fprintf out_file "string%d:\n\t.string\t\"%s\"\n" v k)
    string_map;
  Printf.fprintf out_file
    "\t.globl empty.string\nempty.string:\n\t.string\t\"\"\n";
  Printf.fprintf out_file
    "\t.globl percent.ld\npercent.ld:\n\t.string\t\"%%ld\"\n";
  Printf.fprintf out_file
    "\t.globl percent.d\npercent.d:\n\t.string\t\"%%d\"\n";
  Printf.fprintf out_file "\t.text\n";
  List.iter print_asm handlers;
  Printf.fprintf out_file
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

(* basic_block_to_ast *)

(* 
  
let cfg_to_asm = ()
  let class_name = "TODO" in
  let method_name = "TODO" in


*)
