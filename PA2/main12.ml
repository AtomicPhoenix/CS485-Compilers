let file_name = Sys.argv.(1)
let file = open_in file_name
let base_file_name = String.sub file_name 0 (String.length file_name - 7)
let out_file = open_out (base_file_name ^ ".cl-type")
let read () = input_line file

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
  | Let_Expr of (identifier * identifier * expr option) list * expr
  | Case of expr * case_el list

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
let method_map = Hashtbl.create 50

let unpack_method (feat : feature) =
  match feat with
  | Method (id1, formal_list, id2, exp) -> (id1, formal_list, id2, exp)
  | _ -> assert false

(* Check if method has already been defined by parents & if so check if it is a valid override *)
let check_redefined (method_signature, parent_name, method_name) =
  let c_name, c_formals, c_type, c_exp = unpack_method method_signature in
  match Hashtbl.find_opt method_map (parent_name, method_name) with
  (* No Parent with same method name found *)
  | None -> () (* Parent found w/ same method name *)
  | Some meth ->
      let p_name, p_formals, p_type, p_exp = unpack_method meth in
      if p_type.name != c_type.name then (
        Printf.fprintf out_file
          "Type Error: Method %s overriden and method type redefined from %s \
           to %s\n"
          method_name p_type.name c_type.name;
        exit 1);
      (* Check if amount of formals is the same *)
      if List.length p_formals != List.length c_formals then
        Printf.fprintf out_file
          "Type Error: Method %s in class %s overrides method from parent %s \
           and had incorrect amount of formals"
          method_name c_name.name p_name.name;

      (* Checks if type of formals is the same *)
      let p_formal_types =
        List.map (fun (form : formal) -> form.typename.name) p_formals
      in
      let c_formal_types =
        List.map (fun (form : formal) -> form.typename.name) c_formals
      in
      assert (p_formal_types = c_formal_types)

let rec get_ancestors (name : string) acc =
  let c = Hashtbl.find_opt class_map name in
  match c with
  | Some c_class -> (
      let acc = c_class :: acc in
      match c_class.inherits with
      | Some parent ->
          List.iter
            (fun c_class ->
              if c_class.typename.name = parent.name then assert false)
            acc;
          get_ancestors parent.name acc
      | None ->
          if c_class.typename.name <> "Object" then
            c_class :: get_ancestors "Object" acc
          else c_class :: acc)
  | None -> []

let rec add_method (class_name : string) (method_signature : feature) =
  match method_signature with
  | Method (id, _, _, _) -> (
      let method_name = id.name in
      (* Check if method has already been defined within this class *)
      match Hashtbl.find_opt method_map (class_name, method_name) with
      | None ->
          (* Add Method to method_map *)
          Hashtbl.add method_map (class_name, method_name) method_signature
      | Some _ ->
          (* ERROR: Method has already been defined within this class *)
          let id1, formal_list, id2, exp = unpack_method method_signature in
          Printf.fprintf out_file
            "ERROR: %d: Type-Check: Method %s redefined in Class %s"
            id1.line_num method_name class_name;
          exit 1)
  | _ -> ()

and add_all_methods () =
  let classes = Hashtbl.fold (fun _ v acc -> v :: acc) class_map [] in
  let add_class_methods (c_class : cool_class) =
    List.iter
      (fun feat ->
        add_method c_class.typename.name feat;
        ())
      c_class.features
  in
  List.iter add_class_methods classes

and check_all_methods () =
  let rec check ((class_name, method_name), method_signature) =
    (* Check if method has already been defined by parents & if so check if it is a valid override *)
    let ancestry_tree = get_ancestors class_name [] in
    List.iter
      (fun ancestor ->
        check_redefined (method_signature, ancestor.typename.name, method_name))
      ancestry_tree
  in
  let methods =
    Hashtbl.fold (fun (k1, k2) v acc -> ((k1, k2), v) :: acc) method_map []
  in
  List.iter (fun ((k1, k2), v) -> check ((k1, k2), v)) methods

and add_class (c_class : cool_class) =
  let name = c_class.typename.name in
  match Hashtbl.find_opt class_map name with
  | None -> Hashtbl.add class_map name c_class
  | Some _ ->
      Printf.fprintf out_file "ERROR: %d: Type-Check: class %s redefined"
        c_class.typename.line_num c_class.typename.name;
      exit 1

and check_class_cycle () =
  let classes = Hashtbl.fold (fun _ v acc -> v :: acc) class_map [] in
  List.iter
    (fun c ->
      let _ = get_ancestors c.typename.name [] in
      ())
    classes

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
      typename = { line_num = 0; name = "Int" };
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

let rec get_class () : cool_class =
  let ident = get_identifier () in
  let inh = get_inherits () in
  let feats = get_feature_list ident.name in
  { typename = ident; inherits = inh; features = feats }

and read_int () : int =
  let r = read () in
  try int_of_string r
  with c ->
    print_endline r;
    raise c

and get_feature_list (class_name : string) =
  List.init (read_int ()) (fun _ -> get_feature class_name)

and get_expression_list () =
  List.init (read_int ()) (fun _ -> get_expression ())

and get_formal_list () = List.init (read_int ()) (fun _ -> get_formal ())
and get_class_list () = List.init (read_int ()) (fun _ -> get_class ())

and get_expression () : expr =
  let base_expr = get_identifier () in
  Expression (base_expr, get_sub_expr base_expr.name)

and get_sub_expr name =
  match name with
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
  | "integer" -> Int_Constant (read_int ())
  | "string" -> String_Constant (read ())
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
  | name ->
      print_string name;
      raise Not_found

and get_case_element () =
  let var = get_identifier () in
  let typ = get_identifier () in
  let exp = get_expression () in
  { variable = var; typename = typ; elem_body = exp }

and get_base_let_list () = List.init (read_int ()) (fun _ -> get_base_let ())

and get_case_element_list () =
  try List.init (read_int ()) (fun _ -> get_case_element ())
  with _ -> raise (Invalid_argument "Not a num")

and get_base_let () =
  let binding = read () in
  match binding with
  | "let_binding_no_init" -> get_no_init_binding ()
  | "let_binding_init" -> get_init_binding ()
  | c ->
      print_endline c;
      assert false

and get_no_init_binding () =
  let name = get_identifier () in
  let typename = get_identifier () in
  (name, typename, None)

and get_init_binding () =
  let name = get_identifier () in
  let typename = get_identifier () in
  let exp1 = get_expression () in
  (name, typename, Some exp1)

and get_identifier () : identifier =
  let linenum = read_int () in
  let name = read () in
  { line_num = linenum; name }

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

and get_method (class_name : string) =
  let name = get_identifier () in
  let formals = get_formal_list () in
  let typename = get_identifier () in
  let body = get_expression () in
  let new_method = Method (name, formals, typename, body) in
  new_method

and get_feature (class_name : string) =
  let feat_type = read () in
  match feat_type with
  | "attribute_no_init" -> get_no_init_attribute ()
  | "attribute_init" -> get_init_attribute ()
  | "method" -> get_method class_name
  | c ->
      print_endline c;
      assert false

let rec print_class_map ast =
  Printf.fprintf out_file "class_map\n";
  Printf.fprintf out_file "%d\n" (List.length ast);
  List.iter
    (fun c_class ->
      Printf.fprintf out_file "%s\n" c_class.typename.name;
      print_attributes c_class)
    ast

and get_all_attributes (c_class : cool_class) =
  let parent_tree = get_ancestors c_class.typename.name [] in
  let get_attributes id =
    List.filter
      (function Attribute _ -> true | _ -> false)
      (Hashtbl.find class_map id.typename.name).features
  in
  List.flatten (List.map get_attributes parent_tree)
(* let rec get_attrs(c_class) = List.filter (function Method _ -> true | _ -> false) c_class in *)

and get_features (c_class : cool_class) (predicate : feature -> bool) =
  let selected = List.filter predicate c_class.features in
  Printf.fprintf out_file "%d\n" (List.length selected);
  if List.length selected > 0 then
    let print =
     fun feat ->
      match feat with
      | Attribute (name, typ, assign) -> (
          match assign with
          | None ->
              Printf.fprintf out_file "no_initializer\n%s\n%s\n" name.name
                typ.name
          | Some exp ->
              Printf.fprintf out_file "initializer\n%s\n%s\n" name.name typ.name;
              print_init_expression (exp, typ.name))
      | Method (id, fl, id2, exp) ->
          print_endline id.name;
          Printf.fprintf out_file "%d\n" (List.length fl);
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
  Printf.fprintf out_file "implementation_map\n";
  Printf.fprintf out_file "%d\n" (List.length ast);
  List.iter
    (fun c_class ->
      Printf.fprintf out_file "%s\n" c_class.typename.name;
      print_methods c_class)
    ast

and print_parent_map ast =
  Printf.fprintf out_file "parent_map\n";
  Printf.fprintf out_file "%d\n" (List.length ast - 1);
  let no_object_ast =
    List.filter (fun c_class -> c_class.typename.name <> "Object") ast
  in
  List.iter
    (fun c_class ->
      print_endline c_class.typename.name;
      match c_class.inherits with
      | Some inhrt -> print_endline inhrt.name
      | None -> print_endline "Object")
    no_object_ast

and print_annotated_ast ast =
  Printf.fprintf out_file "%d\n" (List.length ast);
  List.iter print_class ast

and print_class c_class =
  print_identifier c_class.typename;
  (match c_class.inherits with
  | Some inhrt -> Printf.fprintf out_file "inherits\n%s\n" inhrt.name
  | None -> print_endline "no_inherits");
  print_features c_class (fun _ -> true)

and print_features (c_class : cool_class) (predicate : feature -> bool) =
  let selected = List.filter predicate c_class.features in
  Printf.fprintf out_file "%d\n" (List.length selected);
  if List.length selected > 0 then
    let print =
     fun feat ->
      match feat with
      | Attribute (name, typ, assign) -> (
          match assign with
          | None ->
              Printf.fprintf out_file "no_initializer\n%s\n%s\n" name.name
                typ.name
          | Some exp ->
              Printf.fprintf out_file "initializer\n%s\n%s\n" name.name typ.name;
              print_init_expression (exp, typ.name))
      | Method (id, fl, id2, exp) ->
          print_endline id.name;
          Printf.fprintf out_file "%d\n" (List.length fl);
          List.iter (fun (f : formal) -> print_endline f.name.name) fl;
          print_endline "TODO: PRINT NAME OF CLASS WHERE METHOD IS DEFINED";
          print_expression exp
    in
    List.iter print selected

and print_methods (c_class : cool_class) =
  print_features c_class (function Method _ -> true | _ -> false)

and print_attributes (c_class : cool_class) =
  let attrs = get_all_attributes c_class in
  Printf.fprintf out_file "%d\n" (List.length attrs);
  let print =
   fun feat ->
    match feat with
    | Attribute (name, typ, assign) -> (
        match assign with
        | None ->
            Printf.fprintf out_file "no_initializer\n%s\n%s\n" name.name
              typ.name
        | Some exp ->
            Printf.fprintf out_file "initializer\n%s\n%s\n" name.name typ.name;
            print_init_expression (exp, typ.name))
    | Method (id, fl, id2, exp) ->
        print_endline id.name;
        Printf.fprintf out_file "%d\n" (List.length fl);
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
      Printf.fprintf out_file "%d\n%s\n" id.line_num id.name;
      print_sub_expr sub

and print_identifier (id : identifier) =
  Printf.fprintf out_file "%d\n%s\n" id.line_num id.name

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
  | Int_Constant i -> Printf.fprintf out_file "%d\n" i
  | String_Constant s -> Printf.fprintf out_file "%s\n" s
  | Ident_Expr s -> print_identifier s
  | Boolean_Constant v -> (
      match v with
      | True -> print_endline "true"
      | False -> print_endline "false")
  | Let_Expr (binding_list, exp2) ->
      let print_binding (id1, id2, exp) =
        match exp with
        | Some ex ->
            print_endline "let_binding_init";
            print_identifier id1;
            print_identifier id2;
            print_expression ex
        | None ->
            print_endline "let_binding_no_init";
            print_identifier id1;
            print_identifier id2
      in
      List.iter print_binding binding_list;
      print_expression exp2
  | Case (exp, elems) ->
      print_expression exp;
      Printf.fprintf out_file "%d\n" (List.length elems);
      let print_case_element (case_elem : case_el) =
        print_identifier case_elem.variable;
        print_identifier case_elem.typename;
        print_expression case_elem.elem_body
      in
      List.iter print_case_element elems
;;

let user_classes = List.init (read_int ()) (fun _ -> get_class ()) in
let () = List.iter add_class user_classes in
let ast =
  List.sort
    (fun c_class1 c_class2 ->
      String.compare c_class1.typename.name c_class2.typename.name)
    (user_classes @ default_classes)
in
check_class_cycle ();
add_all_methods ();
check_all_methods ();
print_class_map ast
