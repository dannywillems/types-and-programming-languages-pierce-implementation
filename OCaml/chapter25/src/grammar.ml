(* {1 Term type. } *)
(* A term is a variable, a lambda (ie an abstraction) or an application. *)
type t =
  | TermTrue
  | TermFalse
  | TermIf of t * t * t
  | TermVar of termVariable
  | TermAbs of termVariable * typ * t
  | TermApp of t * t
  | TermTypeAbs of typeVariable * t
  | TermTypeApp of t * typ

and termVariable = string

(* {2 Type type. } *)

and typ =
  | TypeBool
  | TypeFunction of (typ * typ)
  | TypeAll of (typeVariable * typ)

and typeVariable = string
