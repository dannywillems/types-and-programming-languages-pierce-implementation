(* A term is a variable, a lambda (ie an abstraction) or an application. *)
type t =
  | TermVar of int * int
  | TermAbs of string * t
  | TermApp of t * t

(* The binding type. Here is only a NameBinding because we replace the variable string representation by the DeBruijn index *)
type binding = NameBinding

(* The context is a list of binding. *)
type context = (string * binding) list

let length_of_context context = List.length context

(* Get the corresponding name of the DeBruijn index from the given context *)
let name_of_index context x =
  fst @@ List.nth context ((List.length context) - x - 1)

let rec pick_fresh_name context x =
  if List.mem x (List.map fst context)
  then pick_fresh_name context (x ^ "'")
  else (List.rev ( (x, NameBinding) :: (List.rev context) ), x)

let rec string_of_term context term = match term with
  | TermAbs (x, term1) ->
    let (context', x') = pick_fresh_name context x in
    Printf.sprintf "(λ %s. %s)" x' (string_of_term context' term1)
  | TermApp (term1, term2) ->
    Printf.sprintf "(%s %s)" (string_of_term context term1) (string_of_term context term2)
  | TermVar (debruijn_index, context_size) ->
    if length_of_context context = context_size
    then (name_of_index context debruijn_index)
    else "[bad index]"

let print_term context term =
  print_endline @@ string_of_term context term
