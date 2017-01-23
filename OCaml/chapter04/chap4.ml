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

(* Small step evaluation.
   The principle of the small step evaluation is to evaluate only once. The
   complete evaluation is done step by step by using small step evaluation.
   The small step evaluation is the direct implementation of the evaluation rules.

   The evaluation rules are:
   * if true then t2 else t3 => t2
   * if false then t2 else t3 => t3
   * t1 -> t1' => if t1 then t2 else t3 -> if t1' then t2 else t3
   * t1 -> t1' => succ(t1) -> succ(t1')
   * pred(0) => 0
   * pred(succ(nv)) -> nv
   * t1 -> t1' => pred(t1) -> pred(t1')
   * iszero(0) -> true
   * iszero(succ(nv)) -> false
   * t1 -> t1' => iszero(t1) -> iszero(t1')

   The term in the pattern matching is the term which must be evaluated in the
   conclusion of each evaluation relation.
*)
let rec small_step_evaluation term = match term with
  (* t1 -> true => if t1 then t2 else t3 -> t2 *)
  | TermIf (_, TermTrue _, term2, term3) ->  (* If the condition is true, term2 is returned *)
    term2
  (* t1 -> false => if t1 then t2 else t3 -> t3 *)
  | TermIf (_, TermFalse _, term2, term3) ->  (* If the condition is false, term3 is returned *)
    term3
  (* t1 -> t1' => if t1 then t2 else t3 -> if t1' then t2 else t3 *)
  | TermIf (info, term1, term2, term3) -> (* If term1 is not false or true but another term, we compute it first. *)
    let evaluated_term1 = small_step_evaluation term1 in (* W: We first evaluate the condition, not the arguments *)
    TermIf (info, evaluated_term1, term2, term3)
    (* t1 -> t2 => succ t1 => succ(t2) *)
  | TermSucc (info, term1) ->
    let evaluated_term1 = small_step_evaluation term1 in
    TermSucc (info, evaluated_term1)
    (* pred(0) => 0 *)
  | TermPred (info, TermZero(_)) -> TermZero(info)
    (* pred(succ(nv)) -> nv *)
  | TermPred (_, TermSucc(_, nv)) when is_numerical_value nv ->
    nv
    (* t1 -> t2 => pred(t1) -> pred(t2) *) | TermPred (info, term) ->
    let evaluated_term = small_step_evaluation term in
    TermPred(info, evaluated_term)
    (* IsZero(0) -> True *)
  | TermIsZero(info, TermZero(_)) -> TermTrue(info)
    (* IsZero(succ(nv)) -> False *)
  | TermIsZero(info, TermSucc(_, _)) -> TermFalse(info)
    (* t1 -> t2 => IsZero(t1) -> IsZero(t2) *)
  | TermIsZero(info, term) ->
    let evaluated_term = small_step_evaluation term in
    TermIsZero(info, evaluated_term)
  | _ -> raise NoRuleApplies

(* Big step evaluation.
   In contrast to single step evaluation, terms are evaluated directly to
   values, not to a term.
   the term t is evaluated to the value is noted t ⇓ v.

   The evaluation rules for big step evaluation are:
   * v ⇓ v
   * t1 ⇓ true, t2 ⇓ v2 => if t1 then t2 else t3 ⇓ v2
   * t1 ⇓ false, t3 ⇓ v3 => if t1 then t2 else t3 ⇓ v3
   * t1 ⇓ nv1 => succ(t1) ⇓ succ(nv1)
   * t1 ⇓ 0 => pred(t1) ⇓ 0
   * t1 ⇓ succ(nv1) => pred(t1) ⇓ nv1
   * t1 ⇓ 0 => iszero(t1) ⇓ true
   * t1 ⇓ succ(nv1) => iszero(t1) ⇓ false

   The term in the pattern matching is the term which must be evaluated in the
   conclusion of each evaluation relation.
*)

let rec big_step_evaluation term = match term with
  (* v ⇓ v *)
  | v when is_value v -> v
  (* t1 ⇓ true, t2 ⇓ v2 => if t1 then t2 else t3 ⇓ v2. We need to evaluate t2 and return the corresponding value. *)
  | TermIf(info, TermTrue(_), term2, term3) ->
    big_step_evaluation term2
  (* t1 ⇓ false, t3 ⇓ v3 => if t1 then t2 else t3 ⇓ v3. We need to evaluate t3 and return the corresponding value. *)
  | TermIf(info, TermFalse(_), term2, term3) ->
    big_step_evaluation term3
  (* t1 ⇓ nv1 => succ(t1) ⇓ succ(nv1). *)
  | TermSucc(info, term) ->
    let evaluated_term = big_step_evaluation term in
    if is_value evaluated_term
    then TermSucc(info, evaluated_term)
    else raise NoRuleApplies
  (* t1 ⇓ 0 => pred(t1) ⇓ 0 *)
  | TermPred(info, TermZero(_)) ->
    TermZero(info)
  (* t1 ⇓ succ(nv1) => pred(t1) ⇓ nv1 *)
  | TermPred(info, TermSucc(_, nv)) when is_numerical_value nv ->
    nv
  (* t1 ⇓ 0 => iszero(t1) ⇓ true *)
  | TermIsZero(info, TermZero(_)) ->
    TermTrue(info)
  (* t1 ⇓ succ(nv1) => iszero(t1) ⇓ false *)
  | TermIsZero(info, TermSucc(_, nv)) when is_numerical_value nv ->
    TermFalse(info)
  | _ -> raise NoRuleApplies

