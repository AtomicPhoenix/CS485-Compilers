(** ast is a list of classes **)
type ast = cool_class list
(** identifier is a line number and a name**)
and identifier = int * string
(** cool class is an identifier and a feature list **)
and cool_class = identifier * feature list
and feature =

(** name (identifier) and type (identifier)  **)
  | Attribute of identifier * identifier
(** name (identifier), formal list (formal list), type (identifier), and body (expression)  **)
  | Method of identifier * formal list * identifier * exp

(** Line number and name **)
and exp = identifier

(** name (identifier) and type (identifier) **)
and formal = identifier * identifier

and let_exp =
(** line number & "let" (identifier), variable (identifier), type (identifier)**)
        | No_init of identifier * identifier
(** line number & "let" (identifier), variable (identifier), type (identifier), and value (exp)**)
        | Yes_init of identifier * identifier * exp

(** line number & "case" (identifier), case expression (exp) and case-elements (case_el list)  **)
and case = identifier * exp * case_el list

(** variable (identifier), type (identifier), and case-element-body (exp) **)
and case_el = identifier * identifier * exp

let file = open_in Sys.argv.(1)

(** Read line returns one line from the AST**)
let read() = input_line file

let rec get_class() = 
        (get_ident(), get_feature_list())
and
get_feature_list() = get_list get_feature
and
get_list func = 
        List.init (int_of_string (read())) (fun _ -> func())
        (** let rec get_list_inner len =
                if (len<=0) then []
                else func()k :: get_list_inner (len-1)
        in get_list_inner (int_of_string (read())) **)
and 
get_feature() = 
        let name = read() in
        let attr_name = get_ident() in
        let attr_type = get_ident() in
        (name, attr_name, attr_type)
                
and      
get_ident() =
     let line = read() in
        let str = read() in
        ( line, str )
let ast = List.init (int_of_string (read())) (fun _ -> get_class())
let class_list = List.map (fun ((_, name), _) -> name) (ast)
let () = Printf.printf "%d \t Number of Classes\n" (List.length (class_list))
