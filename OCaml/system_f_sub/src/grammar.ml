type term =
  (* x *)
  | TermVar of string
  (* λ(x : T) t *)
  | TermAbs of string * typ * term
  (* t t *)
  | TermApp of term * term
  (* λX <: T t *)
  | TermTypeAbs of string * typ * term
  (* t[T] *)
  | TermTypeApp of term * typ

and typ =
  | TypeVar of string
  (* Top *)
  | TypeTop
  (* T -> T *)
  | TypeFunction of typ * typ
  (* ∀X <: T T *)
  | TypeForall of string * typ * typ
