let file_name = Sys.argv.(1)
let file = open_in file_name
let base_file_name = String.sub file_name 0 (String.length file_name - 7)
let out_file = open_out (base_file_name ^ ".cl-type")
let read () = input_line file

(* The static type of a COOL Expression *)
type static_type =
  | Class of string (* "Int" or "Object" *)
  | SELF_TYPE of string (* "Self_Type_c" *)

let type_to_str t =
  match t with
  | Class x -> x
  | SELF_TYPE c -> String.concat " " [ "SELF_TYPE"; c ]

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
and expr = {
  id : identifier;
  sub_expr : sub_expr;
  mutable static_type : static_type option;
}

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

and bool_val = True | False

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

exception T of string

let class_map = Hashtbl.create 64
let method_map = Hashtbl.create 64
let (objEnv : (string, static_type) Hashtbl.t) = Hashtbl.create 64
let list_is_empty l = List.compare_length_with l 0 = 0

let print_typecheck_error line error =
  Printf.printf "ERROR: %d: Type-Check: %s\n" line error;
  exit 1

let unpack_method (feat : feature) =
  match feat with
  | Method (id1, formal_list, id2, exp) -> (id1, formal_list, id2, exp)
  | _ -> assert false

(* Check if method has already been defined by parents & if so check if it is a valid override *)
let check_redefined (method_signature, parent_name, method_name, class_name) =
  let c_name, c_formals, c_type, c_exp = unpack_method method_signature in
  match Hashtbl.find_opt method_map (parent_name, method_name) with
  (* No Parent with same method name found *)
  | None -> () (* Parent found w/ same method name *)
  | Some meth ->
      let p_name, p_formals, p_type, p_exp = unpack_method meth in
      if p_type.name <> c_type.name then
        (*Printf.printf*)
        (*"Type Error: Method %s overriden and method type redefined from %s \*)
           (*to %s\n"*)
        (*method_name p_type.name c_type.name;*)
        print_typecheck_error c_name.line_num
          (Printf.sprintf
             "class %s redefines method %s and changes return type (from %s to \
              %s)"
             class_name method_name p_type.name c_type.name);

      (* Check if amount of formals is the same *)
      if List.length p_formals <> List.length c_formals then
        (*Printf.printf*)
        (*"Type Error: Method %s in class %s overrides method from parent %s \*)
           (*and had incorrect amount of formals"*)
        (*method_name c_name.name p_name.name;*)
        print_typecheck_error c_name.line_num
          (Printf.sprintf
             "class %s redefines method %s and changes number of formals)"
             class_name method_name);

      (* Checks if type of formals is the same *)
      let p_formal_types =
        List.map (fun (form : formal) -> form.typename.name) p_formals
      in
      let c_formal_types =
        List.map (fun (form : formal) -> form.typename.name) c_formals
      in
      (*assert (p_formal_types = c_formal_types)*)
      if p_formal_types <> c_formal_types then
        print_typecheck_error c_name.line_num
          (Printf.sprintf "Method %s redefined in %s" method_name class_name)

let rec get_ancestors (name : string) acc =
  let c = Hashtbl.find_opt class_map name in
  match c with
  | Some c_class -> (
      let acc = c_class :: acc in
      match c_class.inherits with
      | Some parent ->
          List.iter
            (fun c_class ->
              if c_class.typename.name = parent.name then
                print_typecheck_error 0
                  (Printf.sprintf "Inheritence cycle for %s"
                     c_class.typename.name);
              if
                parent.name = "Bool" || parent.name = "String"
                || parent.name = "Int" || parent.name = "SELF_TYPE"
                || parent.name = "void" || parent.name = ""
              then
                print_typecheck_error c_class.typename.line_num
                  (Printf.sprintf "Class %s inherits uninheritable class "
                     c_class.typename.name))
            acc;
          get_ancestors parent.name acc
      | None ->
          if c_class.typename.name <> "Object" then
            Hashtbl.find class_map "Object" :: acc
          else acc)
  | None -> []

and lub child parent =
  let ancestors = get_ancestors child [] in
  match List.find_opt (fun f -> f.typename.name = parent) ancestors with
  | Some _ -> true
  | None -> false

(* TODO: Implement join better this is all garbage*)
and get_join (class_list : string list) =
  let rec find_first_shared lst1 lst2 =
    match lst1 with
    | [] -> None (* No shared value found *)
    | x :: xs -> if List.mem x lst2 then Some x else find_first_shared xs lst2
  in
  let find_first_shared_multiple lists =
    match lists with
    | [] -> None (* No lists provided *)
    | first :: rest ->
        let rec find_in_rest lst rest_lists =
          match rest_lists with
          | [] -> None
          | l :: ls -> (
              match find_first_shared lst l with
              | Some x -> Some x
              | None -> find_in_rest lst ls)
        in
        find_in_rest first rest
  in
  let lists = List.map (fun f -> get_ancestors f []) class_list in
  let opt = find_first_shared_multiple lists in
  match opt with None -> Class "Object" | Some v -> Class v.typename.name

let check_duplicate_formals (lst : formal list) method_name class_name =
  let rec aux seen = function
    | [] -> None
    | x :: xs -> if List.mem x seen then Some x else aux (x :: seen) xs
  in
  match aux [] lst with
  | None -> ()
  | Some c ->
      print_typecheck_error c.name.line_num
        (Printf.sprintf
           "Type-Check: Duplicate formal parameter %s redefined in Method %s \
            Class %s"
           c.name.name method_name class_name)

let rec add_method (class_name : string) (method_signature : feature) =
  match method_signature with
  | Method (id, fl, _, _) -> (
      check_duplicate_formals fl id.name class_name;
      let method_name = id.name in
      (* Check if method has already been defined within this class *)
      match Hashtbl.find_opt method_map (class_name, method_name) with
      | None ->
          (* Add Method to method_map *)
          Hashtbl.add method_map (class_name, method_name) method_signature
      | Some _ ->
          (* ERROR: Method has already been defined within this class *)
          let id1, formal_list, id2, exp = unpack_method method_signature in
          print_typecheck_error id1.line_num
            (Printf.sprintf "Type-Check: Method %s redefined in Class %s"
               method_name class_name))
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
        check_redefined
          (method_signature, ancestor.typename.name, method_name, class_name))
      ancestry_tree
  in
  let methods =
    Hashtbl.fold (fun (k1, k2) v acc -> ((k1, k2), v) :: acc) method_map []
  in
  List.iter (fun ((k1, k2), v) -> check ((k1, k2), v)) methods

and add_all_attributes feature_list (c_class : cool_class) =
  (* Printf.printf ("Adding up to %d Attributes for class %s\n") (List.length feature_list) c_class.typename.name; *)
  List.iter
    (fun feat ->
      match feat with
      | Attribute (name, attr_type, _) ->
          let typ =
            if attr_type.name = "SELF_TYPE" then SELF_TYPE c_class.typename.name
            else Class attr_type.name
          in
          Hashtbl.add objEnv name.name typ
      | _ -> ())
    feature_list

and remove_all_attributes feature_list =
  List.iter
    (fun feat ->
      match feat with
      | Attribute (name, _, _) -> Hashtbl.remove objEnv name.name
      | _ -> ())
    feature_list

and add_all_formals (formal_list : formal list) (c_class : cool_class) =
  List.iter
    (fun (formal : formal) ->
      let typ =
        if formal.typename.name = "SELF_TYPE" then
          SELF_TYPE c_class.typename.name
        else Class formal.typename.name
      in
      Hashtbl.add objEnv formal.name.name typ)
    formal_list

and remove_all_formals (formal_list : formal list) (c_class : cool_class) =
  List.iter
    (fun (formal : formal) -> Hashtbl.remove objEnv formal.name.name)
    formal_list

and add_class (c_class : cool_class) =
  let name = c_class.typename.name in
  match Hashtbl.find_opt class_map name with
  | None -> Hashtbl.add class_map name c_class
  | Some _ ->
      print_typecheck_error c_class.typename.line_num
        (Printf.sprintf "class %s redefined" c_class.typename.name)

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
      features =
        [
          Method
            ( { line_num = 0; name = "abort" },
              [],
              { line_num = 0; name = "Object" },
              {
                id = { name = "string"; line_num = 0 };
                sub_expr = String_Constant "";
                static_type = None;
              } );
          Method
            ( { line_num = 0; name = "type_name" },
              [],
              { line_num = 0; name = "String" },
              {
                id = { name = "string"; line_num = 0 };
                sub_expr = String_Constant "";
                static_type = None;
              } );
          Method
            ( { line_num = 0; name = "copy" },
              [],
              { line_num = 0; name = "SELF_TYPE" },
              {
                id = { name = "string"; line_num = 0 };
                sub_expr = String_Constant "";
                static_type = None;
              } );
        ];
    };
    {
      typename = { line_num = 0; name = "Bool" };
      inherits = Some { line_num = 0; name = "Object" };
      features = [];
    };
    {
      typename = { line_num = 0; name = "String" };
      inherits = Some { line_num = 0; name = "Object" };
      features =
        [
          Method
            ( { line_num = 0; name = "length" },
              [],
              { line_num = 0; name = "Int" },
              {
                id = { name = "string"; line_num = 0 };
                sub_expr = String_Constant "";
                static_type = None;
              } );
          Method
            ( { line_num = 0; name = "concat" },
              [
                {
                  name = { name = "s"; line_num = 0 };
                  typename = { name = "String"; line_num = 0 };
                };
              ],
              { line_num = 0; name = "String" },
              {
                id = { name = "string"; line_num = 0 };
                sub_expr = String_Constant "";
                static_type = None;
              } );
          Method
            ( { line_num = 0; name = "substr" },
              [
                {
                  name = { name = "i"; line_num = 0 };
                  typename = { name = "Int"; line_num = 0 };
                };
                {
                  name = { name = "l"; line_num = 0 };
                  typename = { name = "Int"; line_num = 0 };
                };
              ],
              { line_num = 0; name = "String" },
              {
                id = { name = "string"; line_num = 0 };
                sub_expr = String_Constant "";
                static_type = None;
              } );
        ];
    };
    {
      typename = { line_num = 0; name = "Int" };
      inherits = Some { line_num = 0; name = "Object" };
      features = [];
    };
    {
      typename = { line_num = 0; name = "IO" };
      inherits = Some { line_num = 0; name = "Object" };
      features =
        [
          Method
            ( { line_num = 0; name = "out_string" },
              [
                {
                  name = { name = "x"; line_num = 0 };
                  typename = { name = "String"; line_num = 0 };
                };
              ],
              { line_num = 0; name = "SELF_TYPE" },
              {
                id = { name = "string"; line_num = 0 };
                sub_expr = String_Constant "";
                static_type = None;
              } );
          Method
            ( { line_num = 0; name = "out_int" },
              [
                {
                  name = { name = "x"; line_num = 0 };
                  typename = { name = "Int"; line_num = 0 };
                };
              ],
              { line_num = 0; name = "SELF_TYPE" },
              {
                id = { name = "string"; line_num = 0 };
                sub_expr = String_Constant "";
                static_type = None;
              } );
          Method
            ( { line_num = 0; name = "in_string" },
              [],
              { line_num = 0; name = "String" },
              {
                id = { name = "string"; line_num = 0 };
                sub_expr = String_Constant "";
                static_type = None;
              } );
          Method
            ( { line_num = 0; name = "in_int" },
              [],
              { line_num = 0; name = "Int" },
              {
                id = { name = "string"; line_num = 0 };
                sub_expr = String_Constant "";
                static_type = None;
              } );
        ];
    };
  ]

let () = List.iter add_class default_classes

let rec get_class () : cool_class =
  let ident = get_identifier () in
  let inh = get_inherits () in
  let feats = get_feature_list ident.name in
  if ident.name = "SELF_TYPE" then
    print_typecheck_error ident.line_num
      "SELF_TYPE can not be used as a class name";
  { typename = ident; inherits = inh; features = feats }

and read_int () : int =
  let r = read () in
  try int_of_string r
  with c ->
    Printf.fprintf out_file "%s\n" r;
    raise c

and get_feature_list (class_name : string) =
  List.init (read_int ()) (fun _ -> get_feature class_name)

and get_expression_list () =
  List.init (read_int ()) (fun _ -> get_expression ())

and get_formal_list () = List.init (read_int ()) (fun _ -> get_formal ())
and get_class_list () = List.init (read_int ()) (fun _ -> get_class ())

and get_expression () : expr =
  let id = get_identifier () in
  { id; sub_expr = get_sub_expr id; static_type = None }

and get_sub_expr name =
  match name.name with
  | "assign" ->
      let i = get_identifier () in
      let e = get_expression () in
      if i.name = "self" then
        print_typecheck_error i.line_num
          "self can not be used as a variable name"
      else Assignment (i, e)
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
      if typename.name = "SELF_TYPE" then
        print_typecheck_error typename.line_num
          "obj does not conform to SELF_TYPE in static dispatch";
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
      if List.length res = 0 then
        print_typecheck_error name.line_num "empty block somehow wth";
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
  | name ->
      print_string name;
      raise (T ("Not found:" ^ name))

and get_case_element () =
  let var = get_identifier () in
  let typ = get_identifier () in
  let exp = get_expression () in
  if typ.name = "SELF_TYPE" || typ.name = "self" then
    print_typecheck_error typ.line_num
      "SELF_TYPE can not be used as an identifier";
  if var.name = "SELF_TYPE" || var.name = "self" then
    print_typecheck_error var.line_num
      "SELF_TYPE can not be used as an identifier";
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
      Printf.fprintf out_file "%s\n" c;
      assert false

and check_type (name, typ, linenum) =
  if name = typ then
    print_typecheck_error linenum
      (Printf.sprintf "%s can not be used in this context" typ)

and get_no_init_binding () =
  let name = get_identifier () in
  let typename = get_identifier () in
  check_type (typename.name, "SELF_TYPE", typename.line_num);
  check_type (typename.name, "self", typename.line_num);
  check_type (name.name, "self", name.line_num);
  check_type (typename.name, "void", typename.line_num);
  (name, typename, None)

and get_init_binding () =
  let name = get_identifier () in
  let typename = get_identifier () in
  let exp1 = get_expression () in
  check_type (typename.name, "SELF_TYPE", typename.line_num);
  check_type (typename.name, "self", typename.line_num);
  check_type (name.name, "self", name.line_num);
  check_type (typename.name, "void", typename.line_num);
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

  if typename.name = "SELF_TYPE" || typename.name = "self" then
    print_typecheck_error typename.line_num
      "SELF_TYPE can not be used as an attribute";
  if name.name = "self" then
    print_typecheck_error name.line_num "SELF can not be used as an attribute";
  Attribute (name, typename, None)

and get_init_attribute () =
  let name = get_identifier () in
  let typename = get_identifier () in
  let exp = get_expression () in
  if typename.name = "SELF_TYPE" || typename.name = "self" then
    print_typecheck_error typename.line_num
      "SELF_TYPE can not be used as an attribute";
  if name.name = "self" then
    print_typecheck_error name.line_num "SELF can not be used as an attribute";
  Attribute (name, typename, Some exp)

and get_formal () =
  let name = get_identifier () in
  let typename = get_identifier () in
  if typename.name = "SELF_TYPE" then
    print_typecheck_error typename.line_num
      "SELF_TYPE/self can not be used as a formal";
  if name.name = "self" then
    print_typecheck_error name.line_num
      "SELF_TYPE/self can not be used as a formal";
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
      Printf.fprintf out_file "%s\n" c;
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
          Printf.fprintf out_file "%s\n" id.name;
          Printf.fprintf out_file "%d\n" (List.length fl);
          List.iter
            (fun (f : formal) -> Printf.fprintf out_file "%s\n" f.name.name)
            fl;
          Printf.fprintf out_file "%s" c_class.typename.name;
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
      Printf.fprintf out_file "%s\n" c_class.typename.name;
      match c_class.inherits with
      | Some inhrt -> Printf.fprintf out_file "%s\n" inhrt.name
      | None -> Printf.fprintf out_file "Object")
    no_object_ast

and print_annotated_ast ast =
  Printf.fprintf out_file "%d\n" (List.length ast);
  List.iter print_class ast

and print_class c_class =
  print_identifier c_class.typename;
  (match c_class.inherits with
  | Some inhrt -> Printf.fprintf out_file "inherits\n%s\n" inhrt.name
  | None -> Printf.fprintf out_file "no_inherits");
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
          Printf.fprintf out_file "%s\n" id.name;
          Printf.fprintf out_file "%d\n" (List.length fl);
          List.iter
            (fun (f : formal) -> Printf.fprintf out_file "%s\n" f.name.name)
            fl;
          Printf.fprintf out_file "%s" c_class.typename.name;
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
        Printf.fprintf out_file "%s\n" id.name;
        Printf.fprintf out_file "%d\n" (List.length fl);
        List.iter
          (fun (f : formal) -> Printf.fprintf out_file "%s\n" f.name.name)
          fl;
        Printf.fprintf out_file
          "TODO: PRINT NAME OF CLASS WHERE METHOD IS DEFINED";
        print_expression exp
  in
  List.iter print attrs

and print_expression (exp : expr) =
  print_identifier exp.id;
  print_sub_expr exp.sub_expr

and print_init_expression ((exp : expr), (typename : string)) =
  Printf.fprintf out_file "%d\n%s\n" exp.id.line_num exp.id.name;
  print_sub_expr exp.sub_expr

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
      Printf.fprintf out_file "%d\n" (List.length el);
      List.iter print_expression el
  | Static_Dispatch (exp, id1, id2, el) ->
      print_expression exp;
      print_identifier id1;
      print_identifier id2;
      Printf.fprintf out_file "%d\n" (List.length el);
      List.iter print_expression el
  | Self_Dispatch (id, el) ->
      print_identifier id;
      Printf.fprintf out_file "%d\n" (List.length el);
      List.iter print_expression el
  | If (exp1, exp2, exp3) ->
      print_expression exp1;
      print_expression exp2;
      print_expression exp3
  | While (exp1, exp2) ->
      print_expression exp1;
      print_expression exp2
  | Block el ->
      Printf.fprintf out_file "%d\n" (List.length el);
      List.iter print_expression el
  | New id -> print_identifier id
  | Isvoid exp -> print_expression exp
  | Plus (exp, exp2) ->
      print_expression exp;
      print_expression exp2
  | Minus (exp, exp2) ->
      print_expression exp;
      print_expression exp2
  | Divide (exp, exp2) ->
      print_expression exp;
      print_expression exp2
  | Times (exp, exp2) ->
      print_expression exp;
      print_expression exp2
  | Equal (exp, exp2) ->
      print_expression exp;
      print_expression exp2
  | LessEqual (exp, exp2) ->
      print_expression exp;
      print_expression exp2
  | LessThan (exp, exp2) ->
      print_expression exp;
      print_expression exp2
  | Not exp -> print_expression exp
  | Negate exp -> print_expression exp
  | Int_Constant i -> Printf.fprintf out_file "%d\n" i
  | String_Constant s -> Printf.fprintf out_file "%s\n" s
  | Ident_Expr s -> print_identifier s
  | Boolean_Constant v ->
      ( (*
      match v with
      | True -> Printf.fprintf out_file "true"
      | False -> Printf.fprintf out_file "false"*) )
  | Let_Expr (binding_list, exp2) ->
      let print_binding (id1, id2, exp) =
        match exp with
        | Some ex ->
            Printf.fprintf out_file "let_binding_init";
            print_identifier id1;
            print_identifier id2;
            print_expression ex
        | None ->
            Printf.fprintf out_file "let_binding_no_init";
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

let validate_main () =
  (* Check that there's a class called Main *)
  if not (Hashtbl.mem class_map "Main") then
    print_typecheck_error 0 "class Main not found";
  (* Check that there's a method named main with zero formal params*)
  if
    not
      (let main_class = Hashtbl.find class_map "Main" in
       main_class.features
       |> List.exists (fun feat ->
              match feat with
              | Method (nm, fm, tp, bd) ->
                  nm.name = "main" && List.length fm = 0
              | Attribute _ -> false))
  then print_typecheck_error 0 "class Main method main not found";
  (* check that main has 0 parameters *)
  if
    (* TODO: convert to method map *)
    (*let main_class = Hashtbl.find class_map "Main" in
    match
      main_class.features
      |> List.find (function
           | Method (nm, fm, tp, bd) ->
               nm.name = "main"
               && List.length fm = 0
               && (tp.name = "Object" || tp.name = "SELF_TYPE")
           | Attribute _ -> false)
    with
    | Method (nm, fm, tp, bd) -> not (list_is_empty fm)
    | _ -> false*)
    let res = Hashtbl.find_opt method_map ("Main", "main") in
    match res with None -> true | _ -> false
  then print_typecheck_error 0 "class Main method main w/ 0 params not found"

(** Check if any classes inherit from an unbound class *)
let check_unknown_class_inherit () =
  class_map
  |> Hashtbl.iter (fun _ v ->
         match v.inherits with
         | None -> ()
         | Some w ->
             if not (Hashtbl.mem class_map w.name) then
               print_typecheck_error v.typename.line_num
                 (Printf.sprintf "class %s inherits from unknown class %s"
                    v.typename.name w.name))

let rec check_attributes class_name attributes =
  check_valid_attribute_names class_name attributes;
  check_redefined_attributes class_name attributes

and unpack_attribute (feat : feature) =
  match feat with
  | Method _ -> assert false
  | Attribute (attr_name, attr_type, attr_assign) -> (attr_name, attr_type)

and check_valid_attribute_names class_name attributes =
  let find_duplicate lst =
    let unpacked_list = List.map (fun f -> unpack_attribute f) attributes in
    let names = Hashtbl.create (List.length lst) in
    List.iter
      (fun f ->
        match f with
        | attr_name, attr_type -> (
            match Hashtbl.find_opt names attr_name.name with
            | Some _ ->
                print_typecheck_error attr_name.line_num
                  (Printf.sprintf "Attribute %s redefined in class %s"
                     attr_name.name class_name)
            | None -> Hashtbl.add names attr_name.name attr_name))
      unpacked_list
  in
  find_duplicate attributes;

  List.iter
    (function
      | Method _ -> ()
      | Attribute (attr_name, attr_type, attr_assign) -> (
          if attr_type.name = "SELF_TYPE" || attr_type.name = "self" then
            print_typecheck_error attr_name.line_num
              (Printf.sprintf "In class %s, attribute %s has invalid type %s"
                 class_name attr_name.name attr_type.name);
          match Hashtbl.find_opt class_map attr_type.name with
          | Some n -> ()
          | None ->
              print_typecheck_error attr_name.line_num
                (Printf.sprintf
                   "Attribute %s cannot be of non-existant type Class %s "
                   attr_name.name class_name)))
    attributes

(** [check_redefined_attributes class_name attributes] checks if any attributes
    in [class_name] are redefined *)
and check_redefined_attributes class_name (attributes : feature list) =
  let attrs = Hashtbl.create 10 in
  List.iter
    (function
      | Method _ -> ()
      | Attribute (n, _, _) ->
          (*Printf.printf "%s" n.name;*)
          if Hashtbl.mem attrs n.name then
            print_typecheck_error n.line_num
              (Printf.sprintf "class %s redefines attribute %s" class_name
                 n.name)
          else Hashtbl.add attrs n.name n)
    attributes

let get_method_if_exists (class_name, method_name) metadata =
  let ancestors = get_ancestors class_name [] in
  let method_signatures =
    List.filter
      (fun signature_opt ->
        match
          Hashtbl.find_opt method_map (signature_opt.typename.name, method_name)
        with
        | Some m -> true
        | None -> false)
      ancestors
  in
  if List.length method_signatures < 1 then
    print_typecheck_error metadata.line_num
      (Printf.sprintf "Couldnt find method %s in class %s" method_name
         class_name)
  else
    Hashtbl.find method_map
      ((List.hd method_signatures).typename.name, method_name)

let check_dispatches dispatches class_name =
  List.iter
    (fun dispatch ->
      match dispatch with
      | Dynamic_Dispatch (exp, meth, args) ->
          let m_id, m_formals, m_id2, m_exp =
            unpack_method (get_method_if_exists (class_name, meth.name) meth)
          in
          if List.length args <> List.length m_formals then assert false
      | Static_Dispatch (exp, typename, meth, args) ->
          let m_id, m_formals, m_id2, m_exp =
            unpack_method (get_method_if_exists (class_name, meth.name) meth)
          in
          if List.length args <> List.length m_formals then assert false
      | _ -> raise (Invalid_argument "Something is fundamentally wrong"))
    dispatches

let self_bad lnum =
  print_typecheck_error lnum "self can't be used in this way :("

let rec check_expr expr (cur_class : cool_class) =
  match expr.sub_expr with
  | Assignment (id, exp) ->
      if id.name = "self" then self_bad id.line_num;
      check_expr exp cur_class
  | Dynamic_Dispatch (expr, id, exprlist) ->
      check_expr expr cur_class;
      List.iter (fun e -> check_expr e cur_class) exprlist
  | Static_Dispatch (expr, typename, methodname, exprlist) ->
      ( (*
          check_expr expr cur_class;
          List.iter (fun e -> check_expr e cur_class) exprlist;
          if typename.name = "SELF_TYPE" then self_bad typename.line_num;
          let res =
            Hashtbl.find_opt method_map (typename.name, methodname.name)
          in
          match res with
          | None ->
              print_typecheck_error methodname.line_num
                "bad method name in static dispatch :("
          | Some _ -> ()*) )
  | Self_Dispatch (meth, args) ->
      ( (*
          List.iter (fun e -> check_expr e cur_class) args;
          let res =
            Hashtbl.find_opt method_map (cur_class.typename.name, meth.name)
          in
          match res with
          | None ->
              print_typecheck_error meth.line_num
                "bad method name in self dispatch :("
          | Some _ -> ()*) )
  | If (pred, thn, els) ->
      check_expr pred cur_class;
      check_expr thn cur_class;
      check_expr els cur_class
  | Block exps -> List.iter (fun x -> check_expr x cur_class) exps
  | New id ->
      if id.name = "self" then self_bad id.line_num;
      ()
  | Isvoid exp -> check_expr exp cur_class
  | Plus (x, y)
  | Minus (x, y)
  | Divide (x, y)
  | Times (x, y)
  | LessEqual (x, y)
  | LessThan (x, y)
  | Equal (x, y)
  | While (x, y) ->
      check_expr x cur_class;
      check_expr y cur_class
  | Not x -> check_expr x cur_class
  | Negate x -> check_expr x cur_class
  | Ident_Expr id -> ()
  | Let_Expr (letlist, body) ->
      List.iter
        (fun (var, typ, exp) ->
          if var.name = "self" then self_bad var.line_num;
          let res = Hashtbl.find_opt class_map typ.name in
          (match res with
          | None -> print_typecheck_error typ.line_num "bad type name in let :("
          | Some _ -> ());
          check_expr_opt exp cur_class)
        letlist;
      check_expr body cur_class
  | Case (exp, case_el_list) ->
      check_expr exp cur_class;
      process_caselist case_el_list cur_class
  | _ -> ()

and process_caselist ellist cur_class =
  List.iter
    (fun el ->
      if el.variable.name = "self" then self_bad el.variable.line_num;
      let res = Hashtbl.find_opt class_map el.typename.name in
      (match res with
      | None ->
          print_typecheck_error el.typename.line_num "bad type name in let :("
      | Some _ -> ());
      check_expr el.elem_body cur_class)
    ellist

and check_expr_opt expropt cur_class =
  match expropt with None -> () | Some expr -> check_expr expr cur_class

let check_feature feat cur_class =
  match feat with
  | Attribute (_, tp, assign) ->
      (*check_attr_type tp;*)
      check_expr_opt assign cur_class
  | Method (_, _, _, body) -> check_expr body cur_class

let rec get_type expr (c_class : cool_class) : static_type =
  match expr.sub_expr with
  | Assignment (id, exp) -> (
      (* O(id) = T *)
      let var_id_opt = Hashtbl.find_opt objEnv id.name in
      match var_id_opt with
      | None ->
          print_typecheck_error expr.id.line_num
            (Printf.sprintf "Assignment on undeclared variable %s" id.name)
      | Some t1 ->
          (* O, M, C |- e1 =: T' *)
          let t2 = get_type exp c_class in
          (* T' <= T *)
          if not (lub (type_to_str t2) (type_to_str t1)) then
            print_typecheck_error expr.id.line_num
              (Printf.sprintf
                 "Assignment on variable %s has type %s, does not conform to \
                  type %s"
                 id.name (type_to_str t2) (type_to_str t1))
          (* O. M, C |- Id <-- e1: T' *)
            else t2
      (* TODO: Dynamic_Dispatch *))
  | Dynamic_Dispatch (exp, meth, exprlist) ->
      (* e, method, args*)
      let class_name = get_type exp c_class in
      let m_id, m_formals, m_type, m_exp =
        unpack_method
          (get_method_if_exists (type_to_str class_name, meth.name) meth)
      in
      if List.length exprlist <> List.length m_formals then
        print_typecheck_error expr.id.line_num
          (Printf.sprintf "Argument mismatch, expected %d args, recieved %d"
             (List.length m_formals) (List.length exprlist));
      List.iter2
        (fun i (j : formal) ->
          let t1 = Class j.typename.name in
          let t2 = get_type i c_class in
          if t1 == t2 then
            print_typecheck_error expr.id.line_num
              (Printf.sprintf
                 "Argument mismatch for argument %s, expected type %s, \
                  recieved type %s"
                 j.name.name j.typename.name (type_to_str t2)))
        exprlist m_formals;
      (*
        O, M, C |- e0 : T0
        O, M, C |- e1 : T1
        ...
        O, M, C |- en : Tn
        T0' = {  C if T0 = SELF TYPEC
              {  T0 otherwise
        M (T0', f ) = (T1', ..., Tn', T(n+1)')
        Ti ≤ Ti' 1 ≤ i ≤ n
        Tn+1 = { T0 if T(n+1)' = SELF TYPE
               { T(n+1)' otherwise 
        O, M, C |- e0.f (e1,.., en) : Tn+1
      *)
      Class m_type.name
  | Static_Dispatch (exp, typename, meth, args) ->
      let class_name = get_type exp c_class in
      let m_id, m_formals, m_type, m_exp =
        unpack_method
          (get_method_if_exists (type_to_str class_name, meth.name) meth)
      in
      if List.length args <> List.length m_formals then assert false;
      (* TODO: Static_Dispatch *)
      Class m_type.name
  | Self_Dispatch (meth, args) ->
      (* TODO: Self_Dispatch *)
      (*let class_name = get_type expr c_class in*)
      let m_id, m_formals, m_type, m_exp =
        unpack_method
          (get_method_if_exists (c_class.typename.name, meth.name) meth)
      in
      if List.length args <> List.length m_formals then assert false;
      (* TODO: Static_Dispatch *)
      Class m_type.name
  | If (pred, thn, els) ->
      let t1 = get_type pred c_class in
      (* O, M, C |- e1 : Bool *)
      if t1 <> Class "Bool" then
        print_typecheck_error expr.id.line_num
          (Printf.sprintf "Predicate must be of type Bool, not type %s"
             (type_to_str t1))
      else
        (* O, M, C |- e2 : T2 *)
        (* O, M, C |- e3 : T3 *)
        let t2 = get_type thn c_class in
        let t3 = get_type els c_class in
        (* O, M, C |- if e1 then e2 else e3 fi : T2 U T3 *)
        get_join [ type_to_str t2; type_to_str t3 ]
  | While (cond, body) ->
      (* O,M,C |- e1 : Bool *)
      (* O,M,C |- e2 : Type2 *)
      (* O,M,C |- while e1 loop e2 pool : Object *)
      if get_type cond c_class <> Class "Bool" then
        print_typecheck_error expr.id.line_num
          (Printf.sprintf "Loop conditional must be of type Bool, not type %s"
             (type_to_str (get_type cond c_class)))
      else Class "Object"
  | Block exps ->
      (*Class "Object"*)
      (* TODO: Confirms all Block expressions are objects I can't find a source in the CRM *)
      (*List.iter (fun x -> get_type x c_class) exps*)
      let rec check_block exprs class_context prev_type =
        match exprs with
        | [] -> prev_type
        | hd :: tl -> check_block tl class_context (get_type hd class_context)
      in
      check_block exps c_class (Class "Object")
  | New id -> (
      if
        (* T' = { SELF_TYPEc if T = SELF_TYPE*)
        (*      {         T otherwise        *)
        id.name = "SELF_TYPE"
      then SELF_TYPE c_class.typename.name
      else
        let var_opt = Hashtbl.find_opt class_map id.name in
        match var_opt with
        | None ->
            print_typecheck_error expr.id.line_num
              (Printf.sprintf "Cannot create new variable of undeclared type %s"
                 id.name)
        | Some v -> Class v.typename.name (* O, M, C |- new T : T' *))
  | Isvoid exp -> Class "Bool"
  | Plus (x, y) | Minus (x, y) | Divide (x, y) | Times (x, y) ->
      let xtype = get_type x c_class in
      if xtype <> Class "Int" then
        print_typecheck_error expr.id.line_num
          (Printf.sprintf "Cannot perform arithmetic with type %s"
             (type_to_str xtype));
      let ytype = get_type y c_class in
      if ytype <> Class "Int" then
        print_typecheck_error expr.id.line_num
          (Printf.sprintf "Cannot perform arithmetic with type %s"
             (type_to_str ytype));
      Class "Int"
  | Equal (x, y) ->
      let xtype = get_type x c_class in
      let ytype = get_type y c_class in
      if ytype <> xtype then
        print_typecheck_error expr.id.line_num
          (Printf.sprintf
             "Cannot perform equality comparison with varying types %s and %s"
             (type_to_str xtype) (type_to_str ytype));
      Class "Bool"
  | LessThan (x, y) | LessEqual (x, y) ->
      let xtype = get_type x c_class in
      if xtype <> Class "Int" then
        print_typecheck_error expr.id.line_num
          (Printf.sprintf "Cannot perform comparison with type %s"
             (type_to_str xtype));
      let ytype = get_type y c_class in
      if ytype <> Class "Int" then
        print_typecheck_error expr.id.line_num
          (Printf.sprintf "Cannot perform comparison with type %s"
             (type_to_str ytype));
      Class "Bool"
  | Not x ->
      let xtype = get_type x c_class in
      if xtype <> Class "Bool" then
        print_typecheck_error expr.id.line_num
          (Printf.sprintf "Cannot perform boolean negation with type %s"
             (type_to_str xtype))
      else Class "Bool"
  | Negate x ->
      let xtype = get_type x c_class in
      if xtype <> Class "Int" then
        print_typecheck_error expr.id.line_num
          (Printf.sprintf "Cannot perform integer negation with type %s"
             (type_to_str xtype))
      else Class "Int"
  | Ident_Expr id -> (
      if id.name = "self" then Class c_class.typename.name
      else
        let ident = Hashtbl.find_opt objEnv id.name in
        match ident with
        | None ->
            print_typecheck_error expr.id.line_num
              (Printf.sprintf "Undeclared variable %s" id.name)
        | Some v -> v)
  | Let_Expr (letlist, expr) -> (
      (* COOL REFERENCE MANUAL: 
         Typing a multiple let
         let x1 : T1 [← e1], x2 : T2 [← e2], . . . , xn : Tn [← en] in e
         is defined to be the same as typing
         let x1 : T1 [← e1] in (let x2 : T2 [← e2], . . . , xn : Tn [← en] in e ) 
      *)
      (* 
        Ok so basically we typecheck the outermost type and then the next and so on so forth until the let list is empty at which point we type check the actual expression
      *)
      match letlist with
      | [] -> get_type expr c_class
      | (varname, typename, expr_opt) :: tail -> (
          match expr_opt with
          | None ->
              (* Let No Init*)
              (* T0' = { SELF_TYPEc if T0 = SELF_TYPE*)
              (*       {         T0 otherwise        *)
              (* O, M, C |- e1 : T1 *)
              (* O, M, C |- let x : T0 <- e1 in e2 : T2 *)
              let t0 =
                if typename.name = "SELF_TYPE" then
                  SELF_TYPE c_class.typename.name
                else Class typename.name
              in
              Hashtbl.add objEnv varname.name t0;
              let next_let =
                {
                  id = expr.id;
                  sub_expr = Let_Expr (tail, expr);
                  static_type = None;
                }
              in
              let t1 = get_type next_let c_class in
              Hashtbl.remove objEnv varname.name;
              t1
          | Some inner_expr ->
              (* Let-Init*)
              (* T0' = { SELF_TYPEc if T0 = SELF_TYPE*)
              (*       {         T0 otherwise        *)
              (* O, M, C |- e1 : T1 *)
              (* T1 <= T0' *)
              (* O[T0'/x], M, C |- e2 : T2 *)
              (* O, M, C |- let x : T0 <- e1 in e2 : T2 *)
              let t0 =
                if typename.name = "SELF_TYPE" then
                  SELF_TYPE c_class.typename.name
                else Class typename.name
              in
              let t1 = get_type inner_expr c_class in
              if not (lub (type_to_str t1) (type_to_str t0)) then
                print_typecheck_error expr.id.line_num
                  (Printf.sprintf
                     "Variable %s of type %s cannot have type %s assigned to it"
                     varname.name typename.name (type_to_str t1))
              else (
                Hashtbl.add objEnv varname.name t0;
                let next_let =
                  {
                    id = expr.id;
                    sub_expr = Let_Expr (tail, expr);
                    static_type = None;
                  }
                in
                let t2 = get_type next_let c_class in
                Hashtbl.remove objEnv varname.name;
                t2)))
  | Case (exp, case_el_list) ->
      (* TODO: Verify whatever this is is correct *)
      (*
        O, M, C |- e0 : T0
        O[T1/x1], M, C |- e1 : T1'
        ...
        O[Tn/xn], M, C |- en : Tn'
        O, M, C |- case e0 of x1 : T1 -> e1; ... xn : Tn ⇒ en; esac : ⊔1 <= i <= n Ti'
      *)

      (* NOTE: Variables declared on each branch of a case must have distinct type *)
      let type_list =
        List.map
          (fun (case_element : case_el) -> case_element.typename.name)
          case_el_list
      in
      let rec aux seen = function
        | [] -> ()
        | x :: xs ->
            if List.mem x seen then
              print_typecheck_error expr.id.line_num
                (Printf.sprintf "Two cases may not be type %s" x)
            else aux (x :: seen) xs
      in
      aux [] type_list;
      (* Finish checking for distinct types *)
      (* Typecheck each expr *)
      List.iter
        (fun (case_element : case_el) ->
          let t1 = get_type case_element.elem_body c_class in
          if t1 <> Class case_element.typename.name then
            print_typecheck_error expr.id.line_num
              (Printf.sprintf
                 "Case element must return type %s, returns type %s"
                 case_element.typename.name (type_to_str t1)))
        case_el_list;
      let (static_type_list : string list) =
        List.map
          (fun (case_element : case_el) -> case_element.typename.name)
          case_el_list
      in

      (* Return join of all types *)
      get_join static_type_list
      (* These three should be fine as we type check them on initial parsing *)
  | Int_Constant int_val -> Class "Int"
  | Boolean_Constant bool_val -> Class "Bool"
  | String_Constant str_val -> Class "String"

let traverse_tree_for_errors ast =
  check_class_cycle ();
  add_all_methods ();
  check_all_methods ();
  validate_main ();
  check_unknown_class_inherit ();
  List.iter
    (fun cls -> check_attributes cls.typename.name (get_all_attributes cls))
    ast;
  List.iter
    (fun cls -> List.iter (fun feat -> check_feature feat cls) cls.features)
    ast;
  List.iter
    (fun cls ->
      add_all_attributes cls.features cls;
      List.iter
        (fun feat ->
          match feat with
          | Attribute (id, cool_type, Some init_expr) ->
              let t1 = get_type init_expr cls in
              if not (lub (type_to_str t1) cool_type.name) then
                print_typecheck_error id.line_num
                  (Printf.sprintf
                     "Attribute assignment %s does not conform to attribute \
                      type %s"
                     (type_to_str t1) cool_type.name)
          | Attribute (id, cool_type, _) -> ()
          | Method (id, formal_list, typename, expr) ->
              add_all_formals formal_list cls;
              let t1 = get_type expr cls in
              if
                not
                  (lub (type_to_str t1) typename.name
                  || typename.name != "SELF_TYPE")
              then
                print_typecheck_error id.line_num
                  (Printf.sprintf
                     "Method return %s does not conform to method type %s for \
                      method %s"
                     (type_to_str t1) typename.name id.name);
              remove_all_formals formal_list cls)
        cls.features;
      remove_all_attributes cls.features)
    ast
(* Step 1: Get all function dispatches by parsing ast*)
(* let classes = Hashtbl.fold (fun _ v acc -> v :: acc) class_map [] in *)

(* Step 2: Check that each dispatch is defined for the class it is executed in *)
;;

let user_classes = List.init (read_int ()) (fun _ -> get_class ()) in
let () = List.iter add_class user_classes in
let ast =
  List.sort
    (fun c_class1 c_class2 ->
      String.compare c_class1.typename.name c_class2.typename.name)
    (user_classes @ default_classes)
in
(* check_ispatches (); *)
traverse_tree_for_errors ast;
print_class_map ast
