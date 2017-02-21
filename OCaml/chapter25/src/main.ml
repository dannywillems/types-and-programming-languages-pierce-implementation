let t =
  Grammar.TermTypeAbs("X", Grammar.TermTypeAbs("Y", Grammar.TermTrue))

let () =
  print_endline (
    Print.string_of_typ (Print.type_of Print.empty_context t)
  )
