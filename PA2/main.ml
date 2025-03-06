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
  (*| SELF_TYPE c -> String.concat " " [ "SELF_TYPE"; c ]*)
  | SELF_TYPE c -> c

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
  | Internal of
      string * string * static_type (* Classname, Methodname, Returntype *)

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

and case_el = { variable : identifier; typename : identifier; elem_body : expr }
(** variable (identifier), type (identifier), and case-element-body (exp) *)

exception T of string

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
                id = { name = "Object"; line_num = 0 };
                sub_expr = Internal ("Object", "abort", Class "Object");
                static_type = None;
              } );
          Method
            ( { line_num = 0; name = "copy" },
              [],
              { line_num = 0; name = "SELF_TYPE" },
              {
                id = { name = "SELF_TYPE"; line_num = 0 };
                sub_expr = Internal ("Object", "copy", SELF_TYPE "Object");
                static_type = None;
              } );
          Method
            ( { line_num = 0; name = "type_name" },
              [],
              { line_num = 0; name = "String" },
              {
                id = { name = "String"; line_num = 0 };
                sub_expr = Internal ("Object", "type_name", Class "String");
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
            ( { line_num = 0; name = "concat" },
              [
                {
                  name = { name = "s"; line_num = 0 };
                  typename = { name = "String"; line_num = 0 };
                };
              ],
              { line_num = 0; name = "String" },
              {
                id = { name = "String"; line_num = 0 };
                sub_expr = Internal ("String", "concat", Class "String");
                static_type = None;
              } );
          Method
            ( { line_num = 0; name = "length" },
              [],
              { line_num = 0; name = "Int" },
              {
                id = { name = "Int"; line_num = 0 };
                sub_expr = Internal ("String", "length", Class "Int");
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
                id = { name = "String"; line_num = 0 };
                sub_expr = Internal ("String", "substr", Class "String");
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
            ( { line_num = 0; name = "in_int" },
              [],
              { line_num = 0; name = "Int" },
              {
                id = { name = "Int"; line_num = 0 };
                sub_expr = Internal ("IO", "in_int", Class "Int");
                (*static_type = Some (Class "Int");*)
                static_type = None;
              } );
          Method
            ( { line_num = 0; name = "in_string" },
              [],
              { line_num = 0; name = "String" },
              {
                id = { name = "String"; line_num = 0 };
                sub_expr = Internal ("IO", "in_string", Class "String");
                (*static_type = Some (Class "String");*)
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
                id = { name = "SELF_TYPE"; line_num = 0 };
                sub_expr = Internal ("IO", "out_int", SELF_TYPE "IO");
                (*static_type = Some (SELF_TYPE "IO");*)
                static_type = None;
              } );
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
                id = { name = "SELF_TYPE"; line_num = 0 };
                sub_expr = Internal ("IO", "out_string", SELF_TYPE "IO");
                (*static_type = Some (SELF_TYPE "IO");*)
                static_type = None;
              } );
        ];
    };
  ]

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
        List.sort compare
          (List.map (fun (form : formal) -> form.typename.name) p_formals)
      in
      let c_formal_types =
        List.sort compare
          (List.map (fun (form : formal) -> form.typename.name) c_formals)
      in
      (*assert (p_formal_types = c_formal_types)*)
      if p_formal_types <> c_formal_types then
        print_typecheck_error c_name.line_num
          (Printf.sprintf
             "Arguments do not match up to formals for method \"%s\" in Class \
              \"%s\""
             method_name class_name)

(** Creates a list of all ancestors of a cool class*)
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

(** checks if a class is a child of another class (helper function to is_subtype
*)
let is_child child parent =
  let rec contains_string value str_list =
    match str_list with
    | [] -> false
    | head :: tail -> if head = value then true else contains_string value tail
  in

  let child_ancestors =
    List.map (fun f -> f.typename.name) (get_ancestors child [])
  in
  contains_string parent child_ancestors

(** Checks if a class is a subtype of another (<= in our lecture notes *)
let is_subtype (child : static_type) (parent : static_type) =
  match (child, parent) with
  | SELF_TYPE _, SELF_TYPE _ -> true
  | SELF_TYPE styp, Class ctyp -> is_child styp ctyp
  | Class _, SELF_TYPE _ -> false
  | Class ctyp1, Class ctyp2 -> is_child ctyp1 ctyp2

(* Finds least upper bound (lub) for a list of classes. Return the lub as a static type *)
let lub class_list : static_type =
  (*
    1. lub(SELF_TYPEc, SELF_TYPEc) = SELF_TYPEc
    2. lub(SELF_TYPEc , T) = lub(C, T)
    - this is the best we can do because SELF_TYPE C ≤ C
    3. lub(T, SELF_TYPEc) = lub(C, T)
    4. lub(T, T’) defined the same as before
  *)
  (* Find the least common ancestor of two classes *)
  let rec get_common_ancestor child parent =
    let child_ancestors = List.rev (get_ancestors child []) in
    let parent_ancestors = List.rev (get_ancestors parent []) in
    match
      (* returns the first element of the list child_ancestors that is also in parent_ancestors *)
      List.find_opt (fun x -> List.mem x parent_ancestors) child_ancestors
    with
    | Some lca -> Some lca
    | None -> None
  in

  (* Static type can either be SELF_TYPE or a Class *)
  let rec get_lub typ1 typ2 =
    match (typ1, typ2) with
    | SELF_TYPE styp1, SELF_TYPE styp2 -> SELF_TYPE styp1
    | SELF_TYPE styp, Class ctyp -> get_lub (Class styp) (Class ctyp)
    | Class ctyp, SELF_TYPE styp -> get_lub (Class styp) (Class ctyp)
    | Class ctyp1, Class ctyp2 -> (
        match get_common_ancestor ctyp1 ctyp2 with
        | Some lca -> Class lca.typename.name
        | None -> Class "Object" (* Fallback to Top (Object) *))
  in
  let ret =
    match class_list with
    | [] ->
        failwith
          "ERROR in lub: No classes passed to function. Either that or \
           something has gone seriously wrong :("
    | hd :: tl -> List.fold_left get_lub hd tl
  in
  ret

(* Checks for duplicate formals in a list of formals. *)
let check_duplicate_formals (lst : formal list) method_name class_name =
  (* Function to find duplicate identifiers for simplicity's sake *)
  let find_duplicate_identifier (idents : identifier list) : identifier option =
    let rec check seen = function
      | [] -> None
      | ({ line_num; name } as ident) :: rest ->
          if List.mem name seen then Some ident else check (name :: seen) rest
    in
    check [] idents
  in
  (* Get list of identifiers *)
  let ids = List.map (fun (f : formal) -> f.name) lst in
  match find_duplicate_identifier ids with
  | None -> ()
  | Some c ->
      print_typecheck_error c.line_num
        (Printf.sprintf
           "Type-Check: Duplicate formal parameter %s redefined in Method %s \
            Class %s"
           c.name method_name class_name)

(* Add a method to the method map after checking parts of it *)
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

(** add all methods within the class map to the method map *)
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

(** Check all methods in the method map *)
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

(** Add all formals of a method to the object environment *)
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

(** remove all formals of a method from the object environment *)
and remove_all_formals (formal_list : formal list) (c_class : cool_class) =
  List.iter
    (fun (formal : formal) -> Hashtbl.remove objEnv formal.name.name)
    formal_list

(** Add a class to the class map *)
and add_class (c_class : cool_class) =
  let name = c_class.typename.name in
  match Hashtbl.find_opt class_map name with
  | None -> Hashtbl.add class_map name c_class
  | Some _ ->
      print_typecheck_error c_class.typename.line_num
        (Printf.sprintf "class %s redefined" c_class.typename.name)

(** Small function to check if a class is a cycle *)
and check_class_cycle () =
  let classes = Hashtbl.fold (fun _ v acc -> v :: acc) class_map [] in
  List.iter
    (fun c ->
      let _ = get_ancestors c.typename.name [] in
      ())
    classes

(* add all of the classes to the class map *)
let () = List.iter add_class default_classes

(******* AST PARSING *******)

(** Read in a class from the AST*)
let rec get_class () : cool_class =
  let ident = get_identifier () in
  let inh = get_inherits () in
  let feats = get_feature_list ident.name in
  if ident.name = "SELF_TYPE" then
    print_typecheck_error ident.line_num
      "SELF_TYPE can not be used as a class name";
  { typename = ident; inherits = inh; features = feats }

(** helper function to read an int from a file *)
and read_int () : int =
  let r = read () in
  try int_of_string r
  with c ->
    Printf.fprintf out_file "%s\n" r;
    raise c

(** read in a list of features for a class*)
and get_feature_list (class_name : string) =
  List.init (read_int ()) (fun _ -> get_feature class_name)

(** read in a list of expressions for a class *)
and get_expression_list () =
  List.init (read_int ()) (fun _ -> get_expression ())

and get_formal_list () = List.init (read_int ()) (fun _ -> get_formal ())
and get_class_list () = List.init (read_int ()) (fun _ -> get_class ())

(** Read in an expression *)
and get_expression () : expr =
  let id = get_identifier () in
  { id; sub_expr = get_sub_expr id; static_type = None }

(** Get the variable part of an expression (the non-identifier) *)
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

(** Read in a case element *)
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

(** Read in an uninitialized let binding *)
and get_no_init_binding () =
  let name = get_identifier () in
  let typename = get_identifier () in
  check_type (typename.name, "SELF_TYPE", typename.line_num);
  check_type (typename.name, "self", typename.line_num);
  check_type (name.name, "self", name.line_num);
  check_type (typename.name, "void", typename.line_num);
  (name, typename, None)

(** Read in a initialized let binding *)
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

(** get a non (default) initialized attribute *)
and get_no_init_attribute () =
  let name = get_identifier () in
  let typename = get_identifier () in

  if typename.name = "SELF_TYPE" || typename.name = "self" then
    print_typecheck_error typename.line_num
      "SELF_TYPE can not be used as an attribute";
  if name.name = "self" then
    print_typecheck_error name.line_num "SELF can not be used as an attribute";
  Attribute (name, typename, None)

(** get a initialized attribute *)
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

(** Get a feature (method or attribute) *)
and get_feature (class_name : string) =
  let feat_type = read () in
  match feat_type with
  | "attribute_no_init" -> get_no_init_attribute ()
  | "attribute_init" -> get_init_attribute ()
  | "method" -> get_method class_name
  | c ->
      Printf.fprintf out_file "%s\n" c;
      assert false

(***** PRINTING & PROCESSING *****)
let rec print_class_map ast =
  Printf.fprintf out_file "class_map\n";
  Printf.fprintf out_file "%d\n" (List.length ast);
  List.iter
    (fun c_class ->
      Printf.fprintf out_file "%s\n" c_class.typename.name;
      print_attributes c_class)
    ast

(** Get a list of all attributes for a class *)
and get_all_attributes (c_class : cool_class) =
  let parent_tree = get_ancestors c_class.typename.name [] in
  let get_attributes id =
    List.filter
      (function Attribute _ -> true | _ -> false)
      (Hashtbl.find class_map id.typename.name).features
  in
  List.flatten (List.map get_attributes parent_tree)

(** Add all attributes of a class to the object environment*)
and add_all_attributes (c_class : cool_class) =
  let attributes = get_all_attributes c_class in
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
    attributes

(** Remove all attributes of a class to the object environment*)
and remove_all_attributes c_class =
  let attributes = get_all_attributes c_class in
  List.iter
    (fun feat ->
      match feat with
      | Attribute (name, _, _) -> Hashtbl.remove objEnv name.name
      | _ -> ())
    attributes

(** Get a feature form a class and print it to a file *)
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

(** Print the parent attributes of a class to a file *)
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
  (* 
    Output parent_map\n
    Output the number of parent-child inheritance relations and then \n. This number is equal to the number of classes minus one (since Object has no parent).
    Output each child class in turn (in ascending alphabetical order):
        Output the name of the child class and then \n.
        Output the name of the child class’s parent and then \n.
   *)
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
      | None -> Printf.fprintf out_file "Object\n")
    no_object_ast

and print_annotated_ast ast =
  (* 
    With two exceptions, the annotated AST format is identical to the normal AST format described above for the .cl-ast file.
    The first change involves expressions. To output an Expression:
        Output the line number of the expression and then a newline (as in the .cl-ast format).
        Output the name of type associated with the expression and then a newline. For example, the expression 3+x is associated with the type Int. This is not required for any of the checkpoints for PA2, only in the final version of PA2.
        Output the name of the expression and then a newline and then any subparts (as in the .cl-ast format).
    The second change is a new kind of expression, internal, used to represent the bodies of predefined methods. Internal expressions are those that are handled by the run-time system — you might think of them as part of the standard library. You output Internal Expressions (including the type annotation, as above) as follows:
        0 \n type \n internal \n Class.method \n
        The valid kinds of internal expressions (i.e., the values for Class.method) are:
            IO.in_int IO.in_string IO.out_int IO.out_string Object.abort Object.copy Object.type_name String.concat String.length String.substr
            They are formally defined in the Cool Reference Manual.
     *)
  Printf.fprintf out_file "%d\n" (List.length ast);
  List.iter print_class ast

(** Print all information about a class in the context of the annoted AST *)
and print_class c_class =
  print_identifier c_class.typename;
  (match c_class.inherits with
  | Some inhrt -> Printf.fprintf out_file "inherits\n%s\n" inhrt.name
  | None -> Printf.fprintf out_file "no_inherits");
  print_features c_class (fun _ -> true)

(** Print all features (Methods & Attributes) of a given class *)
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

(** Print all methods of the class (implementation map) *)
and print_methods (c_class : cool_class) =
  (* Generate a list of ancestors *)
  let ancestors = get_ancestors c_class.typename.name [] in
  (* Gets all methods of ancestors in ancestry order -> alphabetical order *)
  (* Compare features sorts all methods first by class (starting with inherited methods) then alphabetically within each class *)
  (* let compare_features ((f1 : feature), _) ((f2 : feature), _) =
    let get_name = function
      | Attribute (id, _, _) -> id.name
      | Method (id, _, _, _) -> id.name
    in
    String.compare (get_name f1) (get_name f2)
  in *)

  (*A1 B1 C1 D1 E1 F1 G1 A2 B2 H -> B2 A2 C1 D1 E1 F1 G1 H *)

  let remove_duplicates lst =
    let rec aux seen acc = function
      | ((Method (id, _, _, _), c_class | Attribute (id, _, _), c_class) as elem)
        :: tail ->
          let name = id.name in
          if List.mem name seen then
            (* Move overridden method to the front *)
            aux seen
              (elem
              :: List.filter
                   (fun (Method (id1, _, _, _), _ | Attribute (id1, _, _), _) ->
                     id1.name <> name)
                   acc)
              tail
          else
            (* Keep the method normally *)
            aux (name :: seen) (elem :: acc) tail
      | [] -> acc
    in
    aux [] [] lst
  in
  let all_methods =
    List.map
      (fun c_class ->
        List.filter_map
          (fun feat ->
            match feat with
            | Method (id, f1, id2, exp) ->
                Some (Method (id, f1, id2, exp), c_class)
            | _ -> None)
          c_class.features)
      ancestors
    |> List.flatten |> remove_duplicates
  in
  let print_method (meth, c_class) =
    match meth with
    | Method (varname, fl, typename, exp) ->
        Printf.fprintf out_file "%s\n" varname.name;
        Printf.fprintf out_file "%d\n" (List.length fl);
        List.iter
          (fun (f : formal) -> Printf.fprintf out_file "%s\n" f.name.name)
          fl;
        Printf.fprintf out_file "%s\n" c_class.typename.name;
        (match exp.static_type with
        | None -> print_identifier exp.id
        (* Printf.fprintf out_file "No type"; *)
        | Some v ->
            let typename =
              match v with SELF_TYPE v -> "SELF_TYPE" | Class v -> v
            in
            Printf.fprintf out_file "%d\n%s\n%s\n" exp.id.line_num typename
              exp.id.name);
        print_sub_expr exp.sub_expr
    | _ -> ()
  in
  (* Printf.printf "Printing all methods of class %s:\n" c_class.typename.name;
  List.iter
    (fun (f, c) ->
      match f with
      | Method (id, _, _, _) ->
          Printf.printf "%s from %s\n" id.name c.typename.name
      | _ -> ())
    (List.rev all_methods);*)
  Printf.fprintf out_file "%d\n" (List.length all_methods);
  List.iter print_method (List.rev all_methods)

(** Print all attributes of a class *)
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
    | _ -> ()
  in
  List.iter print attrs

and print_expression (exp : expr) =
  (match exp.static_type with
  | None -> print_identifier_without_type exp.id
  | Some v -> print_identifier_with_type exp.id v);
  print_sub_expr exp.sub_expr

and print_init_expression ((exp : expr), (typename : string)) =
  (match exp.static_type with
  | None -> print_identifier_without_type exp.id
  | Some v -> print_identifier_with_type exp.id v);
  print_sub_expr exp.sub_expr

and print_identifier (id : identifier) =
  Printf.fprintf out_file "%d\n%s\n" id.line_num id.name

and print_identifier_with_type (id : identifier) s_type =
  let typename =
    match s_type with SELF_TYPE v -> "SELF_TYPE" | Class v -> v
  in
  Printf.fprintf out_file "%d\n%s\n%s\n" id.line_num typename id.name

and print_identifier_without_type (id : identifier) =
  let typename = "No type found :c" in
  Printf.fprintf out_file "%d\n%s\n%s\n" id.line_num typename id.name

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
            Printf.fprintf out_file "let_binding_init\n";
            print_identifier id1;
            print_identifier id2;
            print_expression ex
        | None ->
            Printf.fprintf out_file "let_binding_no_init\n";
            print_identifier id1;
            print_identifier id2
      in
      Printf.fprintf out_file "%d\n" (List.length binding_list);
      List.iter print_binding binding_list;
      print_expression exp2
  | Internal (classname, methodname, methodreturn) ->
      Printf.fprintf out_file "internal\n%s.%s\n" classname methodname
  | Case (exp, elems) ->
      print_expression exp;
      Printf.fprintf out_file "%d\n" (List.length elems);
      let print_case_element (case_elem : case_el) =
        print_identifier case_elem.variable;
        print_identifier case_elem.typename;
        print_expression case_elem.elem_body
      in
      List.iter print_case_element elems

(** Validate various requirements of the Main class and main method *)
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

(** Typecheck expressions and in the process get the static type of each class *)
let rec get_type expr (c_class : cool_class) : static_type =
  match expr.sub_expr with
  | Assignment (id, assign_exp) -> (
      (* O(id) = T *)
      let var_id_opt = Hashtbl.find_opt objEnv id.name in
      match var_id_opt with
      | None ->
          print_typecheck_error expr.id.line_num
            (Printf.sprintf "Assignment on undeclared variable %s" id.name)
      | Some t1 ->
          (* O, M, C |- e1 =: T' *)
          let t2 = get_type assign_exp c_class in
          (* T' <= T *)
          if not (is_subtype t2 t1) then
            print_typecheck_error expr.id.line_num
              (Printf.sprintf
                 "Assignment on variable %s has type %s, does not conform to \
                  type %s"
                 id.name (type_to_str t2) (type_to_str t1))
            (* O. M, C |- Id <-- e1: T' *)
          else
            expr.static_type <- Some t2;
          t2)
  | Dynamic_Dispatch (exp, meth, exprlist) ->
      (* e, method, args*)
      let class_name = type_to_str (get_type exp c_class) in
      let m_id, m_formals, m_type, m_exp =
        unpack_method (get_method_if_exists (class_name, meth.name) meth)
      in
      if List.length exprlist <> List.length m_formals then
        print_typecheck_error expr.id.line_num
          (Printf.sprintf "Argument mismatch, expected %d args, recieved %d"
             (List.length m_formals) (List.length exprlist));
      List.iter2
        (fun i (j : formal) ->
          let t1 = Class j.typename.name in
          let t2 = get_type i c_class in
          if not (is_subtype t2 t1) then
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
      if m_type.name <> "SELF_TYPE" then (
        let t = Class m_type.name in
        expr.static_type <- Some t;
        t)
      else
        let t = Class class_name in
        expr.static_type <- Some t;
        t
  | Static_Dispatch (exp, typename, meth, args) ->
      let class_name = type_to_str (get_type exp c_class) in
      let m_id, m_formals, m_type, m_exp =
        unpack_method (get_method_if_exists (class_name, meth.name) meth)
      in
      if not (is_subtype (Class class_name) (Class typename.name)) then
        print_typecheck_error expr.id.line_num
          (Printf.sprintf
             "Static_Dispatch error: Class %s cannot call upon method of class \
              %s\n"
             class_name typename.name)
      else if List.length args <> List.length m_formals then
        print_typecheck_error expr.id.line_num
          (Printf.sprintf "wrong number of actual arguments (%d vs %d)"
             (List.length m_formals) (List.length args));
      List.iter2
        (fun i (j : formal) ->
          let t1 = Class j.typename.name in
          let t2 = get_type i c_class in
          if not (is_subtype t2 t1) then
            print_typecheck_error expr.id.line_num
              (Printf.sprintf
                 "Argument mismatch for argument %s, expected type %s, \
                  recieved type %s"
                 j.name.name j.typename.name (type_to_str t2)))
        args m_formals;
      if m_type.name <> "SELF_TYPE" then (
        let t = Class m_type.name in
        expr.static_type <- Some t;
        t)
      else
        let t = Class class_name in
        expr.static_type <- Some t;
        t
  | Self_Dispatch (meth, args) ->
      let m_id, m_formals, m_type, m_exp =
        unpack_method
          (get_method_if_exists (c_class.typename.name, meth.name) meth)
      in
      if List.length args <> List.length m_formals then
        print_typecheck_error expr.id.line_num
          (Printf.sprintf "wrong number of actual arguments (%d vs %d)"
             (List.length m_formals) (List.length args));
      List.iter2
        (fun i (j : formal) ->
          let t1 = Class j.typename.name in
          let t2 = get_type i c_class in
          if not (is_subtype t2 t1) then
            print_typecheck_error expr.id.line_num
              (Printf.sprintf
                 "Argument mismatch for argument %s, expected type %s, \
                  recieved type %s"
                 j.name.name j.typename.name (type_to_str t2)))
        args m_formals;
      if m_type.name <> "SELF_TYPE" then (
        let t = Class m_type.name in
        expr.static_type <- Some t;
        t)
      else
        let t = SELF_TYPE c_class.typename.name in
        expr.static_type <- Some t;
        t
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
        let t = lub [ t2; t3 ] in
        expr.static_type <- Some t;
        t
  | While (cond, body) ->
      (* O,M,C |- e1 : Bool *)
      (* O,M,C |- e2 : Type2 *)
      (* O,M,C |- while e1 loop e2 pool : Object *)
      if get_type cond c_class <> Class "Bool" then
        print_typecheck_error expr.id.line_num
          (Printf.sprintf "Loop conditional must be of type Bool, not type %s"
             (type_to_str (get_type cond c_class)))
      else
        let _ = get_type body c_class in
        let t = Class "Object" in
        expr.static_type <- Some t;
        t
  | Block exps ->
      if List.length exps < 1 then
        print_typecheck_error expr.id.line_num
          (Printf.sprintf "Empty Block Expressions");
      let t_list = List.map (fun f -> get_type f c_class) exps in
      let t = List.hd (List.rev t_list) in
      expr.static_type <- Some t;
      t
  | New id -> (
      if
        (* T' = { SELF_TYPEc if T = SELF_TYPE*)
        (*      {         T otherwise        *)
        id.name = "SELF_TYPE"
      then (
        let t = SELF_TYPE c_class.typename.name in
        expr.static_type <- Some t;
        t)
      else
        let var_opt = Hashtbl.find_opt class_map id.name in
        match var_opt with
        | None ->
            print_typecheck_error expr.id.line_num
              (Printf.sprintf "Cannot create new variable of undeclared type %s"
                 id.name)
        | Some v ->
            let t = Class v.typename.name in
            expr.static_type <- Some t;
            t (* O, M, C |- new T : T' *))
  | Isvoid exp ->
      let _ = get_type exp c_class in
      ignore (get_type exp c_class);
      let t = Class "Bool" in
      expr.static_type <- Some t;
      t
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
      let t = Class "Int" in
      expr.static_type <- Some t;
      t
  | Equal (x, y) ->
      let xtype = type_to_str (get_type x c_class) in
      let ytype = type_to_str (get_type y c_class) in
      if ytype <> xtype then
        print_typecheck_error expr.id.line_num
          (Printf.sprintf
             "Cannot perform equality comparison with varying types %s and %s"
             xtype ytype);
      let t = Class "Bool" in
      expr.static_type <- Some t;
      t
  | LessThan (x, y) | LessEqual (x, y) ->
      let xtype = get_type x c_class in
      let ytype = get_type y c_class in
      if xtype = Class "Int" && ytype <> Class "Int" then
        print_typecheck_error expr.id.line_num
          (Printf.sprintf "Cannot perform comparison with type %s"
             (type_to_str xtype))
      else if xtype = Class "String" && ytype <> Class "String" then
        print_typecheck_error expr.id.line_num
          (Printf.sprintf "Cannot perform comparison with type %s"
             (type_to_str xtype))
      else if xtype = Class "Bool" && ytype <> Class "Bool" then
        print_typecheck_error expr.id.line_num
          (Printf.sprintf "Cannot perform comparison with type %s"
             (type_to_str xtype));
      let t = Class "Bool" in
      expr.static_type <- Some t;
      t
  | Not x ->
      let xtype = get_type x c_class in
      if xtype <> Class "Bool" then
        print_typecheck_error expr.id.line_num
          (Printf.sprintf "Cannot perform boolean negation with type %s"
             (type_to_str xtype))
      else
        let t = Class "Bool" in
        expr.static_type <- Some t;
        t
  | Negate x ->
      let xtype = get_type x c_class in
      if xtype <> Class "Int" then
        print_typecheck_error expr.id.line_num
          (Printf.sprintf "Cannot perform integer negation with type %s"
             (type_to_str xtype))
      else
        let t = Class "Int" in
        expr.static_type <- Some t;
        t
  | Ident_Expr id -> (
      if id.name = "self" then (
        let t = SELF_TYPE c_class.typename.name in
        expr.static_type <- Some t;
        t)
      else
        let ident = Hashtbl.find_opt objEnv id.name in
        match ident with
        | None ->
            print_typecheck_error expr.id.line_num
              (Printf.sprintf "Undeclared variable %s" id.name)
        | Some t ->
            expr.static_type <- Some t;
            t)
  | Let_Expr (letlist, let_exp) ->
      List.iter
        (fun (varname, typename, expr_opt) ->
          match expr_opt with
          | None ->
              let t0 =
                if typename.name = "SELF_TYPE" then
                  SELF_TYPE c_class.typename.name
                else Class typename.name
              in
              Hashtbl.add objEnv varname.name t0
          | Some inner_expr ->
              let t0 =
                if typename.name = "SELF_TYPE" then
                  SELF_TYPE c_class.typename.name
                else Class typename.name
              in
              let t1 = get_type inner_expr c_class in
              if not (is_subtype t1 t0) then
                print_typecheck_error expr.id.line_num
                  (Printf.sprintf
                     "Variable %s of type %s cannot have type %s assigned to it"
                     varname.name typename.name (type_to_str t1))
              else Hashtbl.add objEnv varname.name t0)
        letlist;
      let t = get_type let_exp c_class in
      List.iter
        (fun (varname, typename, expr_opt) ->
          Hashtbl.remove objEnv varname.name)
        letlist;
      expr.static_type <- Some t;
      t
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
      let static_type_list =
        List.map
          (fun (case_element : case_el) ->
            Hashtbl.add objEnv case_element.variable.name
              (Class case_element.typename.name);
            let t = get_type case_element.elem_body c_class in
            Hashtbl.remove objEnv case_element.variable.name;
            t)
          case_el_list
      in
      ignore (get_type exp c_class);
      (* Return join of all types *)
      let t = lub static_type_list in
      expr.static_type <- Some t;
      t
  | Internal (classname, methodname, methodreturn) -> methodreturn
  (* These three should be fine as we type check them on initial parsing *)
  | Int_Constant int_val ->
      let t = Class "Int" in
      expr.static_type <- Some t;
      t
  | Boolean_Constant bool_val ->
      let t = Class "Bool" in
      expr.static_type <- Some t;
      t
  | String_Constant str_val ->
      let t = Class "String" in
      expr.static_type <- Some t;
      t

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
    (fun cls ->
      add_all_attributes cls;
      List.iter
        (fun feat ->
          match feat with
          | Attribute (id, cool_type, Some init_expr) ->
              let t1 = get_type init_expr cls in
              if not (is_subtype t1 (Class cool_type.name)) then
                print_typecheck_error id.line_num
                  (Printf.sprintf
                     "Attribute assignment %s does not conform to attribute \
                      type %s"
                     (type_to_str t1) cool_type.name)
          | Attribute (id, cool_type, _) -> ()
          | Method (id, formal_list, typename, expr) ->
              add_all_formals formal_list cls;
              let t1 = get_type expr cls in
              let t2 =
                match typename.name with
                | "SELF_TYPE" -> SELF_TYPE cls.typename.name
                | _ -> Class typename.name
              in
              if not (is_subtype t1 t2) then (
                let get_typename t =
                  match t with
                  | Class v -> "Class: " ^ v
                  | SELF_TYPE v -> "SELF_TYPE: " ^ v
                in
                Printf.fprintf out_file "-----------------";
                print_sub_expr expr.sub_expr;
                Printf.fprintf out_file "-----------------";
                print_typecheck_error id.line_num
                  (Printf.sprintf
                     "Method return %s does not conform to method type %s for \
                      method %s for expression type %s"
                     (get_typename t1) (get_typename t2) id.name expr.id.name));
              remove_all_formals formal_list cls)
        cls.features;
      remove_all_attributes cls)
    ast
;;

let user_classes = List.init (read_int ()) (fun _ -> get_class ()) in
let () = List.iter add_class user_classes in
let ast =
  List.sort
    (fun c_class1 c_class2 ->
      String.compare c_class1.typename.name c_class2.typename.name)
    (user_classes @ default_classes)
in
traverse_tree_for_errors ast;
 print_class_map ast; 
print_implementation_map ast;
 print_parent_map ast 
(* print_annotated_ast ast *)
