(* A term represents a node in the AST *)
(* The grammar is given by:
   t ::=
      True
      False
      if t1 then t2 else t3
      0
      succ t
      pred t
      isZero t

   Possible values are given by:
   v ::=
      True
      False
      nv

   And nv (numerical value) is bounded to:

   nv ::=
      0
      succ nv

*)

(* Parsers generally give information about the expression like the line. For
   the exercice, we suppose it's simply a string.
*)
type info = string

(* Grammar representation using a sum type. *)
type term =
  | TermTrue of info (* For the True value *)
  | TermFalse of info (* For the False value *)
  | TermIf of info * term * term * term (* For the if ... then ... else ... term *)
  | TermZero of info (* For the 0 value *)
  | TermSucc of info * term (* For the successor term *)
  | TermPred of info * term (* For the predecessor term *)
  | TermIsZero of info * term (* For the isZero term *)

(* Return true if the given term is a numerical values. *)
let rec is_numerical_value term = match term with
  | TermZero _ -> true
  | TermSucc (_, _) -> true
  | _ -> false

let rec is_value term = match term with
  | TermTrue _ -> true
  | TermFalse _ -> true
  | term when is_numerical_value term -> true
  | _ -> false


(* {2 Evaluation. } *)

exception NoRuleApplies

let rec single_step_evaluation term = match term with
  (* t1 -> true => if t1 then t2 else t3 -> t2 *)
  | TermIf (_, TermTrue _, term2, term3) ->  (* If the condition is true, term2 is returned *)
    term2
  (* t1 -> false => if t1 then t2 else t3 -> t3 *)
  | TermIf (_, TermFalse _, term2, term3) ->  (* If the condition is false, term3 is returned *)
    term3
  (* t1 -> t1' => if t1 then t2 else t3 -> if t1' then t2 else t3 *)
  | TermIf (info, term1, term2, term3) -> (* If term1 is not false or true but another term, we compute it first. *)
    let evaluated_term1 = single_step_evaluation term1 in (* W: We first evaluate the condition, not the arguments *)
    TermIf (info, evaluated_term1, term2, term3)
    (* t1 -> t2 => succ t1 => succ(t2) *)
  | TermSucc (info, term1) ->
    let evaluated_term1 = single_step_evaluation term1 in
    TermSucc (info, evaluated_term1)
    (* pred(0) => 0 *)
  | TermPred (info, TermZero(_)) -> TermZero(info)
    (* pred(succ(nv)) -> nv *)
  | TermPred (_, TermSucc(_, nv)) when is_numerical_value nv ->
    nv
    (* t1 -> t2 => pred(t1) -> pred(t2) *)
  | TermPred (info, term) ->
    let evaluated_term = single_step_evaluation term in
    TermPred(info, evaluated_term)
    (* IsZero(0) -> True *)
  | TermIsZero(info, TermZero(_)) -> TermTrue(info)
    (* IsZero(succ(nv)) -> False *)
  | TermIsZero(info, TermSucc(_, _)) -> TermFalse(info)
    (* t1 -> t2 => IsZero(t1) -> IsZero(t2) *)
  | TermIsZero(info, term) ->
    let evaluated_term = single_step_evaluation term in
    TermIsZero(info, evaluated_term)
  | _ -> raise NoRuleApplies
