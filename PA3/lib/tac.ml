open Print
open Parser

(* 
NOTE: Expression to Three-Address Code
"The traditional approach to converting expressions to three-address code involves a recursive descent traversal of the abstract syntax tree. The recursive descent traversal returns both a three-address code instruction as well as a list of additional instructions that should be prepended to the output."
*)
type tac_elem = {
  operand : tac_operand;
  arg1 : string;
  arg2 : string;
  result : string;
  line : int;
  static_type : static_type option;
}

and tac_operand =
  | Assignment
  | Bt
  | Call
  | Comment
  | Label
  | Jmp
  | Default
  | Case of string
  | VoidCase
  | EmptyCase
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
  | ClassId

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
  | Case jump -> Printf.sprintf "jump to :%s after comparison of" jump
  | VoidCase -> "VoidCase"
  | EmptyCase -> "EmptyCase"
  | Comment -> "comment"
  | Label -> "label"
  | Jmp -> "jmp"
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
  | ClassId -> "classId"

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
  | Case c -> Printf.fprintf out_file "Cmp %s, %s -> jump to %s" t.arg1 t.arg2 c
  | VoidCase -> Printf.fprintf out_file "VoidCase: %s" t.arg1
  | EmptyCase -> Printf.fprintf out_file "EmptyCase: %s" t.arg1
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

let get_tac_elem_commented t =
  match t.operand with
  | Label -> Printf.sprintf "#;label %s" t.arg1
  | Jmp -> Printf.sprintf "#;jmp %s" t.arg1
  | Return -> Printf.sprintf "#;return %s" t.arg1
  | Comment -> Printf.sprintf "#;comment %s" t.arg1
  | Bt -> Printf.sprintf "#;bt %s %s" t.arg1 t.arg2
  | Assignment -> Printf.sprintf "#;%s <- %s" t.result t.arg1
  | LetNoInit -> Printf.sprintf "#;%s <- %s %s" t.result t.arg1 t.arg2
  | String_Constant -> Printf.sprintf "#;%s <- %s" t.result t.arg1
  | Case c -> Printf.sprintf "#Cmp %s, %s -> jump to %s" t.arg1 t.arg2 c
  | VoidCase -> Printf.sprintf "#VoidCase: %s" t.arg1
  | EmptyCase -> Printf.sprintf "#EmptyCase: %s" t.arg1
  | _ ->
      if t.arg2 = "" && t.arg1 = "" then
        Printf.sprintf "#;%s <- %s" t.result (operand_to_string t.operand)
      else if t.arg2 = "" then
        Printf.sprintf "#;%s <- %s %s" t.result
          (operand_to_string t.operand)
          t.arg1
      else
        Printf.sprintf "#;%s <- %s %s %s" t.result
          (operand_to_string t.operand)
          t.arg1 t.arg2

let print_tac_elem_commented t =
  Printf.fprintf out_file "%s\n" (get_tac_elem_commented t)

let print_tac_elems (t : tac_elem list) = List.iter print_tac_elem_commented t

let rec ast_to_tac (ast : annotated_ast_elem list) :
    (tac_elem list * string * string * ast_formal list * int) list =
  let get_tac_elem (ast_elem : annotated_ast_elem) =
    List.filter_map
      (fun (feat, _) ->
        match feat with
        | Method (method_name, arguments, _, exp) ->
            (* Printf.printf "Parsing expression: %s in method %s in class %s\n"
              exp.id.name id1.name ast_elem.class_name.name;  *)
            var_ctr := 0;
            label_ctr := 0;
            let base_lst =
              [
                {
                  operand = Comment;
                  arg1 = "start";
                  arg2 = "";
                  result = "";
                  line = exp.id.line_num;
                  static_type = exp.static_type;
                };
                {
                  operand = Label;
                  arg1 =
                    ast_elem.class_name.name ^ "_" ^ method_name.name ^ "_0";
                  arg2 = "";
                  result = "";
                  line = exp.id.line_num;
                  static_type = exp.static_type;
                };
              ]
            in
            let exp_list =
              exp_to_tac exp (get_id !var_ctr) ast_elem.class_name.name
                method_name.name
            in
            let rtrn =
              [
                {
                  operand = Return;
                  arg1 = get_id !ret;
                  arg2 = "";
                  result = "";
                  line = 0;
                  static_type = exp.static_type;
                };
              ]
            in
            let temps = !var_ctr + 1 in
            Some
              ( base_lst @ exp_list @ rtrn,
                ast_elem.class_name.name,
                method_name.name,
                arguments,
                temps )
        | Attribute _ -> None)
      (get_all_methods ast_elem)
  in
  List.map get_tac_elem ast |> List.flatten

and get_bool bool_val = match bool_val with True -> "true" | False -> "false"
and get_id n = "t$" ^ string_of_int n

and get_label n class_name method_name =
  class_name ^ "_" ^ method_name ^ "_" ^ string_of_int n

and exp_to_tac (exp : expr) result cname mname : tac_elem list =
  match exp.sub_expr with
  | Assignment (id, exp) ->
      (* Printf.fprintf out_file "Assigning result of %s to %s\n" exp.id.name
        id.name; *)
      let var_id = Hashtbl.find_opt letTable id.name in
      let arg1 = match var_id with Some v -> v | None -> id.name in
      (*var_ctr := !var_ctr + 1;*)
      let last_var = exp_to_tac exp arg1 cname mname in
      (*var_ctr := !var_ctr + 1;*)
      last_var
      (*@ [ { operand = Assignment; arg1; arg2 = ""; result = get_id !var_ctr } ]*)
  | Dynamic_Dispatch (dispatch_exp, method_name, args) ->
      let arg_tacs =
        if List.length args > 0 then
          List.map
            (fun arg ->
              var_ctr := !var_ctr + 1;
              let id = get_id !var_ctr in
              (id, exp_to_tac arg id cname mname))
            args
        else []
      in
      var_ctr := !var_ctr + 1;
      let arg2 = String.concat " " (List.map (fun (s, _) -> s) arg_tacs) in
      (List.map (fun (_, v) -> v) arg_tacs |> List.flatten)
      @ exp_to_tac dispatch_exp (get_id !var_ctr) cname mname
      @ [
          {
            operand = Call;
            arg1 = method_name.name;
            arg2;
            result;
            line = exp.id.line_num;
            static_type = exp.static_type;
          };
        ]
  | Static_Dispatch (dispatch_exp, _, method_name, args) ->
      let arg_tacs =
        if List.length args > 0 then
          List.map
            (fun arg ->
              var_ctr := !var_ctr + 1;
              let id = get_id !var_ctr in
              (id, exp_to_tac arg id cname mname))
            args
        else []
      in
      var_ctr := !var_ctr + 1;
      let arg2 = String.concat " " (List.map (fun (s, _) -> s) arg_tacs) in
      (List.map (fun (_, v) -> v) arg_tacs |> List.flatten)
      @ exp_to_tac dispatch_exp (get_id !var_ctr) cname mname
      @ [
          {
            operand = Call;
            arg1 = method_name.name;
            arg2;
            result;
            line = exp.id.line_num;
            static_type = exp.static_type;
          };
        ]
  | Self_Dispatch (id, args) ->
      let arg_tacs =
        if List.length args > 0 then
          List.map
            (fun arg ->
              var_ctr := !var_ctr + 1;
              let id = get_id !var_ctr in
              (id, exp_to_tac arg id cname mname))
            args
        else []
      in
      var_ctr := !var_ctr + 1;
      let arg2 = String.concat " " (List.map (fun (s, _) -> s) arg_tacs) in
      (List.map (fun (_, v) -> v) arg_tacs |> List.flatten)
      @ [
          {
            operand = Comment;
            arg1 = "self";
            arg2 = "";
            result = "%rdi";
            line = exp.id.line_num;
            static_type = Some (SELF_TYPE cname);
          };
          {
            operand = Call;
            arg1 = id.name;
            arg2;
            result;
            line = exp.id.line_num;
            static_type = exp.static_type;
          };
        ]
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
      (*ret := !var_ctr;*)
      var_ctr := !var_ctr + 1;
      let condResult = get_id !var_ctr in
      let cond_tac = exp_to_tac pred_exp condResult cname mname in
      let then_tac = exp_to_tac then_exp result cname mname in
      let else_tac = exp_to_tac else_exp result cname mname in
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
            line = exp.id.line_num;
            static_type = exp.static_type;
          };
          {
            operand = Bt;
            arg1 = jump_else_value;
            arg2 = else_label;
            result;
            line = exp.id.line_num;
            static_type = exp.static_type;
          };
          (* @ [ { operand = Bt; arg1 = true_location; arg2 = then_label; result; line=exp.id.line_num} ] *)
          {
            operand = Comment;
            arg1 = "then branch";
            arg2 = "";
            result;
            line = exp.id.line_num;
            static_type = exp.static_type;
          };
          {
            operand = Label;
            arg1 = then_label;
            arg2 = "";
            result;
            line = exp.id.line_num;
            static_type = exp.static_type;
          };
        ]
      @ then_tac
      @ [
          {
            operand = Jmp;
            arg1 = join_label;
            arg2 = "";
            result;
            line = exp.id.line_num;
            static_type = exp.static_type;
          };
          {
            operand = Comment;
            arg1 = "else branch";
            arg2 = "";
            result;
            line = exp.id.line_num;
            static_type = exp.static_type;
          };
          {
            operand = Label;
            arg1 = else_label;
            arg2 = "";
            result;
            line = exp.id.line_num;
            static_type = exp.static_type;
          };
        ]
      @ else_tac
      @ [
          {
            operand = Jmp;
            arg1 = join_label;
            arg2 = "";
            result;
            line = exp.id.line_num;
            static_type = exp.static_type;
          };
          {
            operand = Comment;
            arg1 = "if-join";
            arg2 = "";
            result;
            line = exp.id.line_num;
            static_type = exp.static_type;
          };
          {
            operand = Label;
            arg1 = join_label;
            arg2 = "";
            result;
            line = exp.id.line_num;
            static_type = exp.static_type;
          };
        ]
  | While (pred_exp, body_exp) ->
      var_ctr := !var_ctr + 1;
      let pred_result = get_id !var_ctr in
      var_ctr := !var_ctr + 1;
      let body_result = get_id !var_ctr in
      var_ctr := !var_ctr + 1;
      let cond_tac = exp_to_tac pred_exp pred_result cname mname in
      let body_tac = exp_to_tac body_exp body_result cname mname in
      var_ctr := !var_ctr + 1;
      let jump_else_value = get_id !var_ctr in
      label_ctr := !label_ctr + 1;
      let cond_label = get_label !label_ctr cname mname in
      label_ctr := !label_ctr + 1;
      let join_label = get_label !label_ctr cname mname in
      label_ctr := !label_ctr + 1;
      let body_label = get_label !label_ctr cname mname in
      let true_location = (List.hd (List.rev cond_tac)).result in
      [
        {
          operand = Jmp;
          arg1 = cond_label;
          arg2 = "";
          result;
          line = exp.id.line_num;
          static_type = exp.static_type;
        };
        {
          operand = Comment;
          arg1 = "while-pred";
          arg2 = "";
          result;
          line = exp.id.line_num;
          static_type = exp.static_type;
        };
        {
          operand = Label;
          arg1 = cond_label;
          arg2 = "";
          result;
          line = exp.id.line_num;
          static_type = exp.static_type;
        };
      ]
      @ cond_tac
      @ [
          {
            operand = Not;
            arg1 = true_location;
            arg2 = "";
            result = jump_else_value;
            line = exp.id.line_num;
            static_type = exp.static_type;
          };
          {
            operand = Bt;
            arg1 = jump_else_value;
            arg2 = join_label;
            result;
            line = exp.id.line_num;
            static_type = exp.static_type;
          };
          {
            operand = Bt;
            arg1 = true_location;
            arg2 = body_label;
            result;
            line = exp.id.line_num;
            static_type = exp.static_type;
          };
          {
            operand = Comment;
            arg1 = "while-body";
            arg2 = "";
            result;
            line = exp.id.line_num;
            static_type = exp.static_type;
          };
          {
            operand = Label;
            arg1 = body_label;
            arg2 = "";
            result;
            line = exp.id.line_num;
            static_type = exp.static_type;
          };
        ]
      @ body_tac
      @ [
          {
            operand = Jmp;
            arg1 = cond_label;
            arg2 = "";
            result;
            line = exp.id.line_num;
            static_type = exp.static_type;
          };
          {
            operand = Comment;
            arg1 = "while-join";
            arg2 = "";
            result;
            line = exp.id.line_num;
            static_type = exp.static_type;
          };
          {
            operand = Label;
            arg1 = join_label;
            arg2 = "";
            result;
            line = exp.id.line_num;
            static_type = exp.static_type;
          };
          {
            operand = Default;
            arg1 = "Object";
            arg2 = "";
            result;
            line = exp.id.line_num;
            static_type = exp.static_type;
          };
        ]
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
          exp_to_tac elem elem_result cname mname)
        exp_list
      |> List.flatten
  | New id ->
      [
        {
          operand = New;
          arg1 = id.name;
          arg2 = "";
          result = get_id !var_ctr;
          line = exp.id.line_num;
          static_type = exp.static_type;
        };
      ]
  | Isvoid exp ->
      var_ctr := !var_ctr + 1;
      let arg1 = get_id !var_ctr in
      exp_to_tac exp arg1 cname mname
      @ [
          {
            operand = Isvoid;
            arg1;
            arg2 = "";
            result;
            line = exp.id.line_num;
            static_type = exp.static_type;
          };
        ]
  | Minus (exp, exp2) ->
      var_ctr := !var_ctr + 1;
      let arg1 = get_id !var_ctr in
      var_ctr := !var_ctr + 1;
      let arg2 = get_id !var_ctr in
      let exp1 = exp_to_tac exp arg1 cname mname in
      let exp2 = exp_to_tac exp2 arg2 cname mname in
      exp1 @ exp2
      @ [
          {
            operand = Minus;
            arg1;
            arg2;
            result;
            line = exp.id.line_num;
            static_type = exp.static_type;
          };
        ]
  | Divide (exp, exp2) ->
      var_ctr := !var_ctr + 1;
      let arg1 = get_id !var_ctr in
      var_ctr := !var_ctr + 1;
      let arg2 = get_id !var_ctr in
      let exp1 = exp_to_tac exp arg1 cname mname in
      let exp2 = exp_to_tac exp2 arg2 cname mname in
      exp1 @ exp2
      @ [
          {
            operand = Divide;
            arg1;
            arg2;
            result;
            line = exp.id.line_num;
            static_type = exp.static_type;
          };
        ]
  | Plus (exp, exp2) ->
      var_ctr := !var_ctr + 1;
      let arg1 = get_id !var_ctr in
      var_ctr := !var_ctr + 1;
      let arg2 = get_id !var_ctr in
      let exp1 = exp_to_tac exp arg1 cname mname in
      let exp2 = exp_to_tac exp2 arg2 cname mname in
      exp1 @ exp2
      @ [
          {
            operand = Plus;
            arg1;
            arg2;
            result;
            line = exp.id.line_num;
            static_type = exp.static_type;
          };
        ]
  | Times (exp, exp2) ->
      var_ctr := !var_ctr + 1;
      let arg1 = get_id !var_ctr in
      var_ctr := !var_ctr + 1;
      let arg2 = get_id !var_ctr in
      let exp1 = exp_to_tac exp arg1 cname mname in
      let exp2 = exp_to_tac exp2 arg2 cname mname in
      exp1 @ exp2
      @ [
          {
            operand = Times;
            arg1;
            arg2;
            result;
            line = exp.id.line_num;
            static_type = exp.static_type;
          };
        ]
  | Equal (exp, exp2) ->
      var_ctr := !var_ctr + 1;
      let arg1 = get_id !var_ctr in
      var_ctr := !var_ctr + 1;
      let arg2 = get_id !var_ctr in
      let exp1 = exp_to_tac exp arg1 cname mname in
      let exp2 = exp_to_tac exp2 arg2 cname mname in
      exp1 @ exp2
      @ [
          {
            operand = Equal;
            arg1;
            arg2;
            result;
            line = exp.id.line_num;
            static_type = exp.static_type;
          };
        ]
  | LessEqual (exp, exp2) ->
      var_ctr := !var_ctr + 1;
      let arg1 = get_id !var_ctr in
      var_ctr := !var_ctr + 1;
      let arg2 = get_id !var_ctr in
      let exp1 = exp_to_tac exp arg1 cname mname in
      let exp2 = exp_to_tac exp2 arg2 cname mname in
      exp1 @ exp2
      @ [
          {
            operand = LessEqual;
            arg1;
            arg2;
            result;
            line = exp.id.line_num;
            static_type = exp.static_type;
          };
        ]
  | LessThan (exp, exp2) ->
      var_ctr := !var_ctr + 1;
      let arg1 = get_id !var_ctr in
      var_ctr := !var_ctr + 1;
      let arg2 = get_id !var_ctr in
      let exp1 = exp_to_tac exp arg1 cname mname in
      let exp2 = exp_to_tac exp2 arg2 cname mname in
      exp1 @ exp2
      @ [
          {
            operand = LessThan;
            arg1;
            arg2;
            result;
            line = exp.id.line_num;
            static_type = exp.static_type;
          };
        ]
  | Not exp ->
      var_ctr := !var_ctr + 1;
      exp_to_tac exp (get_id !var_ctr) cname mname
      @ [
          {
            operand = Not;
            arg1 = get_id !var_ctr;
            arg2 = "";
            result;
            line = exp.id.line_num;
            static_type = exp.static_type;
          };
        ]
  | Negate exp ->
      (*let result = get_id !var_ctr in*)
      (*var_ctr := !var_ctr + 1;*)
      exp_to_tac exp (get_id !var_ctr) cname mname
      @ [
          {
            operand = Negate;
            arg1 = get_id !var_ctr;
            arg2 = "";
            result;
            line = exp.id.line_num;
            static_type = exp.static_type;
          };
        ]
  | Int_Constant i ->
      [
        {
          operand = Int_Constant;
          arg1 = string_of_int i;
          arg2 = "";
          result;
          line = exp.id.line_num;
          static_type = exp.static_type;
        };
      ]
  | String_Constant s ->
      [
        {
          operand = String_Constant;
          arg1 = Printf.sprintf "%s" s;
          arg2 = "";
          result;
          line = exp.id.line_num;
          static_type = exp.static_type;
        };
      ]
  | Ident_Expr s -> (
      match Hashtbl.find_opt letTable s.name with
      | Some t ->
          (* Printf.fprintf out_file "Retrieved variable %s as temp %s\n" s.name t; *)
          [
            {
              operand = Ident_Expr t;
              arg1 = "";
              arg2 = "";
              result;
              line = exp.id.line_num;
              static_type = exp.static_type;
            };
          ]
      | None ->
          [
            {
              operand = Ident_Expr s.name;
              arg1 = "";
              arg2 = "";
              result;
              line = exp.id.line_num;
              static_type = exp.static_type;
            };
          ])
  | Boolean_Constant v ->
      [
        {
          operand = Boolean_Constant;
          arg1 = get_bool v;
          arg2 = "";
          result;
          line = exp.id.line_num;
          static_type = exp.static_type;
        };
      ]
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
            | Some value -> exp_to_tac value result cname mname
            | None ->
                [
                  {
                    operand = LetNoInit;
                    arg1 = "default";
                    arg2 = let_type.name;
                    result;
                    line = exp.id.line_num;
                    static_type = exp.static_type;
                  };
                ])
          binding_list
        |> List.flatten
      in
      b @ exp_to_tac exp result cname mname
  | Case (case_expr, case_elements) ->
      let init_label_ctr = !label_ctr in
      var_ctr := !var_ctr + 1;
      let caseResult = get_id !var_ctr in
      let case_expr_value = exp_to_tac case_expr caseResult mname cname in
      let caseExprResult = get_id !var_ctr in
      var_ctr := !var_ctr + 1;
      let case_id = get_id !var_ctr in
      let case_expr_id =
        [
          {
            operand = ClassId;
            arg1 = caseExprResult;
            arg2 = "";
            result = case_id;
            line = exp.id.line_num;
            static_type = exp.static_type;
          };
        ]
      in
      label_ctr := !label_ctr + 1;
      let null_case_label = get_label !label_ctr mname cname in
      let null_case_jump =
        [
          {
            operand = Case null_case_label;
            arg1 = "$0";
            arg2 = case_id;
            result = "";
            line = exp.id.line_num;
            static_type = exp.static_type;
          };
        ]
      in
      let join_label = cname ^ "_" ^ mname ^ "_join" in
      let defined_case_jumps =
        List.map
          (fun elem ->
            var_ctr := !var_ctr + 1;
            let caseElemClassResult = get_id !var_ctr in
            var_ctr := !var_ctr + 1;
            let equalResult = get_id !var_ctr in
            label_ctr := !label_ctr + 1;
            let case_label = get_label !label_ctr mname cname in
            [
              {
                operand = ClassId;
                arg1 = elem.typename.name;
                arg2 = "";
                result = caseElemClassResult;
                line = exp.id.line_num;
                static_type = exp.static_type;
              };
              {
                operand = Case case_label;
                arg1 = caseElemClassResult;
                arg2 = case_id;
                result = equalResult;
                line = exp.id.line_num;
                static_type = exp.static_type;
              };
              {
                operand = Comment;
                arg1 = "case-join";
                arg2 = "";
                result;
                line = exp.id.line_num;
                static_type = exp.static_type;
              };
            ])
          case_elements
        |> List.flatten
      in
      let empty_case_jump =
        label_ctr := !label_ctr + 1;
        let jump_label = get_label !label_ctr mname cname in
        [
          {
            operand = Jmp;
            arg1 = jump_label;
            arg2 = "";
            result = "";
            line = exp.id.line_num;
            static_type = exp.static_type;
          };
        ]
      in
      label_ctr := init_label_ctr;
      label_ctr := !label_ctr + 1;
      let case_label = get_label !label_ctr mname cname in
      let null_case =
        [
          {
            operand = VoidCase;
            arg1 = case_label;
            arg2 = "";
            result;
            line = exp.id.line_num;
            static_type = exp.static_type;
          };
        ]
      in
      let defined_cases =
        List.map
          (fun elem ->
            (* var_ctr := !var_ctr + 1;
            let var_id = get_id !var_ctr in
            Hashtbl.add letTable elem.variable.name var_id; *)
            Hashtbl.add letTable elem.variable.name caseExprResult;
            label_ctr := !label_ctr + 1;
            let case_label = get_label !label_ctr mname cname in
            [
              {
                operand = Label;
                arg1 = case_label;
                arg2 = "";
                result;
                line = exp.id.line_num;
                static_type = exp.static_type;
              };
            ]
            @ exp_to_tac elem.elem_body result mname cname
            @ [
                {
                  operand = Jmp;
                  arg1 = join_label;
                  arg2 = "";
                  result;
                  line = exp.id.line_num;
                  static_type = exp.static_type;
                };
              ])
          case_elements
        |> List.flatten
      in
      label_ctr := !label_ctr + 1;
      let case_label = get_label !label_ctr mname cname in
      let empty_case =
        [
          {
            operand = EmptyCase;
            arg1 = case_label;
            arg2 = "";
            result;
            line = exp.id.line_num;
            static_type = exp.static_type;
          };
        ]
      in

      (* case_expr_value @ case_class_id @ null_case @ case_element_values
      @ case_expressions  *)
      let case_join =
        [
          {
            operand = Label;
            arg1 = join_label;
            arg2 = "";
            result;
            line = exp.id.line_num;
            static_type = exp.static_type;
          };
        ]
      in
      let jumps = null_case_jump @ defined_case_jumps @ empty_case_jump in
      let cases = null_case @ defined_cases @ empty_case @ case_join in
      case_expr_value @ case_expr_id @ jumps @ cases
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

let default_classes : Parser.annotated_ast_elem list =
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
  List.iter add_class default_classes;
  List.iter add_class Parser.annotated_ast

let tacs = ast_to_tac Parser.annotated_ast
