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
and expression = int * expr
(** A COOL expression *)
and expr =
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
  | Attribute of identifier * identifier * expr option
      (** name (identifier), type (identifier), and assignment (expr option, to cover no init) *)
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

(*-----------------EVERYTHING BELOW IS PROBABLY BROKEN-----------------------------------*)
(* let rec get_class () = (get_ident (), get_feature_list ())
and get_feature_list () = get_list get_feature
and get_list func = List.init (int_of_string (read ())) (fun _ -> func ())

(* TODO: Make this get non-attribute features *)
and get_feature () : feature =
  (*let name = read () in*)
  let attr_name = get_ident () in
  let attr_type = get_ident () in
  Attribute (attr_name, attr_type) *)

(* and get_ident () : identifier =
  let line = read () in
  Printf.printf "line: %s\n" line;
  let lnum = int_of_string_opt line in
  let str = read () in
  Printf.printf "str: %s\n" str;
  (Option.get lnum, str)

let ast = List.init (int_of_string (read ())) (fun _ -> get_class ())
let class_list = List.map (fun ((_, name), _) -> name) ast
let () = Printf.printf "%d\tNumber of Classes\n" (List.length class_list)

let print_ident (a, b) : unit =
  Printf.printf "%s\tIdentifier String\n" b;
  Printf.printf "%d\tIdentifier Num\n" a

let print_class (iden, featureList) : unit =
  print_ident iden;
  Printf.printf "%d\tNumber of Features\n" (List.length featureList)

let () = List.iter print_class ast *)

let get_identifier () : identifier =
  let linenum = int_of_string (read ()) in
  let name = read () in
  {line_num=linenum; name=name;}
let get_inherits () : identifier option =
  let does_inherit = read () in
  if does_inherit = "no_inherit" then
    None
  else
    Some (get_identifier ())
let rec make_elem_list lst num fn = match num with
  | 0 -> lst
  | _ -> make_elem_list (lst@[fn ()]) (num-1) fn


let get_no_init_attribute () =
  let name = get_identifier () in
  let typename = get_identifier () in
  Attribute (name,typename,None)
let get_init_attribute () =
  let name = get_identifier () in
  let typename = get_identifier () in
  let exp = get_expression () in
  Attribute (name,typename,exp)

let get_formal () =
  let name = get_identifier () in
  let typename = get_identifier () in
  {name=name; typename=typename}

let get_formals () =
  let formal_num = int_of_string (read ()) in
  make_elem_list [] formal_num get_formal
let get_method () =
  let name = get_identifier () in
  let formals = get_formals () in
  let typename = get_identifier () in
  let body = get_expression () in
  Method (name, formals, typename, body)
let get_feature () =
  let feat_type = read () in
  match feat_type with
  | "attribute_no_init" -> get_no_init_attribute ()
  | "attribute_init" -> get_init_attribute ()
  | "method" -> get_method ()
  | _ -> assert false
  
let get_feature_list () =
  let feat_num = int_of_string (read ()) in
  make_elem_list [] feat_num get_feature
  
let get_class () : cool_class =
  let name = get_identifier () in
  let inherits = get_inherits () in
  let feats = get_feature_list () in
  {typename=name; inherits=inherits; features=feats;}


let get_program () = begin
  let class_num = int_of_string (read ()) in
  let ast = make_elem_list [] class_num get_class in
  ast

end
