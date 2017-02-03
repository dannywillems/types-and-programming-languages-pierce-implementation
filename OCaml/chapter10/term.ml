exception NotVariableBinding of Ast_helper.info * string
exception TypeError of Ast_helper.info * string

(* {1 Term type. } *)
(* A term is a variable, a lambda (ie an abstraction) or an application. *)
type t =
  | TermTrue of Ast_helper.info
  | TermFalse of Ast_helper.info
  | TermIf of Ast_helper.info * t * t * t
  | TermVar of Ast_helper.info * int * int
  | TermAbs of Ast_helper.info * string * t
  | TermApp of Ast_helper.info * t * t

(* {2 Type type. } *)
type typ =
  | TypeBool
  | TypeFunction of (typ * typ)

let rec string_of_typ = function
  | TypeBool -> "Bool"
  | TypeFunction(t1, t2) ->
    Printf.sprintf
      "%s -> %s"
      (string_of_typ t1)
      (string_of_typ t2)

(* {3 Binding, assumptions and context. } *)
(* The binding type. Here is only a NameBinding because we replace the variable string representation by the DeBruijn index *)
type binding =
  | NameBinding
  | VariableBinding of typ

(* The context is a list of binding. *)
type context = (string * binding) list

let add_binding x binding context =
  (x, binding) :: context

let binding_of_index index context =
  snd @@ List.nth context ((List.length context) - index - 1)

let length_of_context context = List.length context

(* Get the corresponding name of the DeBruijn index from the given context *)
let name_of_index context x =
  fst @@ List.nth context ((List.length context) - x - 1)

let rec index_of_name ?(i = 0) context variable_name =
  match context with
  | [] -> raise Not_found
  | (head, _) :: tail ->
    if head = variable_name
    then i
    else index_of_name ~i:(i + 1) context variable_name

let rec pick_fresh_name context x =
  if List.mem x (List.map fst context)
  then pick_fresh_name context (x ^ "'")
  else (List.rev ( (x, NameBinding) :: (List.rev context) ), x)

let get_type_from_context info context index =
  match binding_of_index index context with
  | VariableBinding typ -> typ
  | _ -> raise (NotVariableBinding (
      info,
      Printf.sprintf "%d is not an index of a variable type binding." index
    ))

let rec type_of context term = match term with
  | TermTrue(_) | TermFalse(_) -> TypeBool
  | TermIf(info, term1, term2, term3) ->
    if (type_of context term1) = TypeBool
    then
      let term2_type = type_of context term2 in
      let term3_type = type_of context term3 in
      if term2_type = term3_type
      then term2_type
      else raise (TypeError(
          info,
          Printf.sprintf
            "%s and %s don't match."
            (string_of_typ term2_type)
            (string_of_typ term3_type)
        ))
    else
      raise (TypeError(info, "Condition must be of type Bool"))
  | TermAbs(info, variable, term) ->
    let index = index_of_name context variable in
    let variable_type = get_type_from_context info context index in
    let term_type = type_of context term in
    TypeFunction(variable_type, term_type)
  | TermApp(info, term1, term2) ->
    let term1_type = type_of context term1 in
    (match term1_type with
    | TypeFunction(type1, type2) ->
      let term2_type = type_of context term2 in
      if type2 = term2_type
      then type1
      else raise (TypeError(
          info,
          Printf.sprintf
            "%s and %s don't match."
            (string_of_typ type1)
            (string_of_typ type2)
        ))
    | _ -> raise (TypeError(
        info,
        "The first term must be an application of type T1 ->T2"
      ))
    )
  | TermVar(info, index, context_length) ->
    get_type_from_context info context index

let rec string_of_term context term = match term with
  | TermAbs (_, x, term1) ->
    let (context', x') = pick_fresh_name context x in
    Printf.sprintf "(λ %s. %s)" x' (string_of_term context' term1)
  | TermApp (_, term1, term2) ->
    Printf.sprintf "(%s %s)" (string_of_term context term1) (string_of_term context term2)
  | TermVar (_, debruijn_index, context_size) ->
    if length_of_context context = context_size
    then (name_of_index context debruijn_index)
    else "[bad index]"
  | TermTrue(_) -> "True"
  | TermFalse(_) -> "False"
  | TermIf(_, term1, term2, term3) ->
    Printf.sprintf
      "if %s then %s else %s"
      (string_of_term context term1)
      (string_of_term context term2)
      (string_of_term context term3)

let print_term context term =
  print_endline @@ string_of_term context term
