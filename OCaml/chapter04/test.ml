open Chap4

let rec equal term1 term2 = match (term1, term2) with
  | (TermTrue(_), TermTrue(_)) -> true
  | (TermFalse(_), TermFalse(_)) -> true
  | (TermIf(_, t1, t2, t3), TermIf(_, t1', t2', t3')) ->
    equal t1 t1' && equal t2 t2' && equal t3 t3'
  | (TermZero(_), TermZero(_)) -> true
  | (TermSucc(_, t1), TermSucc(_, t1')) ->
    equal t1 t1'
  | (TermPred(_, t1), TermPred(_, t1')) ->
    equal t1 t1'
  | (TermIsZero(_, t1), TermIsZero(_, t1')) ->
    equal t1 t1'
  | _ -> false

let test ?(text_if_ok="Ok") ?(text_if_error="Error") name term evaluated_term =
  Printf.printf "Test : %s.\n" name;
  if equal term evaluated_term
  then print_endline @@ "\t" ^ text_if_ok
  else print_endline @@ "\t" ^ text_if_error

let () =
  test "Single Step - Simple if with true"
    (single_step_evaluation (TermIf("", TermTrue(""), TermZero(""), TermTrue(""))))
    (TermZero(""));
  test "Single Step - Simple if with false"
    (single_step_evaluation (TermIf("", TermFalse(""), TermZero(""), TermTrue(""))))
    (TermTrue(""));
  test
    "Single Step - If where condition evaluation is needed"
    (single_step_evaluation (TermIf("", TermIsZero("", TermZero("")), TermZero(""), TermTrue(""))))
    (TermIf("", TermTrue(""), TermZero(""), TermTrue("")))


