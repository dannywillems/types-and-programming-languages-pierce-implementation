open Grammar

exception NameConflict of string
exception TypeError of string

(* The context is a list of binding. *)
type context = {
  variable_binding : (termVariable * typ) list;
  type_binding : typeVariable list
}

let empty_context = {
  variable_binding = [];
  type_binding = []
}

let get_type_of_variable context var =
  let (_, typ) =
    List.find
      (fun (term_variable, _) -> var = term_variable)
      context.variable_binding
  in
  typ

let add_type_of_variable context var typ =
  if List.mem var (List.map fst context.variable_binding)
  then raise (NameConflict var)
  else {
    variable_binding = (var, typ) :: context.variable_binding;
    type_binding = context.type_binding
  }

let rec type_of context term = match term with
  | TermVar(var) ->
    get_type_of_variable context var
  | TermTrue | TermFalse -> TypeBool
  | TermIf(term1, term2, term3) ->
    if (type_of context term1) = TypeBool
    then
      let term2_type = type_of context term2 in
      let term3_type = type_of context term3 in
      if term2_type = term3_type
      then term2_type
      else raise (TypeError(
          Printf.sprintf
            "%s and %s don't match."
            (string_of_typ term2_type)
            (string_of_typ term3_type)
        ))
    else
      raise (TypeError("Condition must be of type Bool"))
  | TermAbs(variable, variable_type, term) ->
    let context' = add_type_of_variable context variable variable_type in
    let term_type = type_of context' term in
    TypeFunction(variable_type, term_type)
  | TermApp(term1, term2) ->
    let term1_type = type_of context term1 in
    (match term1_type with
     | TypeFunction(type1, type2) ->
       let term2_type = type_of context term2 in
       if type2 = term2_type
       then type1
       else raise (TypeError(
           Printf.sprintf
             "%s and %s don't match."
             (string_of_typ type1)
             (string_of_typ type2)
         ))
     | _ -> raise (TypeError(
         "The first term must be an application of type T1 ->T2"
       ))
    )
  | TermTypeAbs(typeVariable, term) ->
    TypeAll(typeVariable, type_of context term)
  | TermTypeApp(term, typ) ->
    let term_typ = type_of context term in
    match term_typ with
    | TypeAll(var, typ2) ->
      term_typ
    | _ -> raise (TypeError("The term must be a forall type."))

and string_of_typ = function
  | TypeBool -> "Bool"
  | TypeFunction(t1, t2) ->
    Printf.sprintf
      "%s -> %s"
      (string_of_typ t1)
      (string_of_typ t2)
  | TypeAll(var, t) ->
    Printf.sprintf
      "∀%s %s"
      var
      (string_of_typ t)

and string_of_term context term = match term with
  | TermAbs (x, variable_type, term1) ->
    Printf.sprintf "(λ(%s : %s). %s)"
      x
      (string_of_typ variable_type)
      (string_of_term context term1)
  | TermApp (term1, term2) ->
    Printf.sprintf "(%s %s)" (string_of_term context term1) (string_of_term context term2)
  | TermVar var -> var
  | TermTrue -> "True"
  | TermFalse -> "False"
  | TermIf(term1, term2, term3) ->
    Printf.sprintf
      "if %s then %s else %s"
      (string_of_term context term1)
      (string_of_term context term2)
      (string_of_term context term3)
  | TermTypeApp(t, typ) ->
    Printf.sprintf
      "%s [%s]"
      (string_of_term context t)
      (string_of_typ typ)
  | TermTypeAbs(var, term) ->
    Printf.sprintf
      "Λ%s %s"
      var
      (string_of_term context term)

let print_term context term =
  print_endline @@ string_of_term context term
