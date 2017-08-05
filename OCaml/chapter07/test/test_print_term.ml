open Term

let identity var context_size =
  TermAbs(var, TermVar(0, context_size + 1))

let () =
  print_term
    [("x", NameBinding)]
    (TermVar(0, 1));
  print_term
    []
    (TermAbs("x",
             TermVar(0, 1)
            )
    );
  print_term
    [("x", NameBinding)]
    (TermAbs("x",
             TermVar(0, 2)
            )
    );
  print_term
    [("x", NameBinding) ; ("y", NameBinding)]
    (TermApp(TermVar(1, 2),
             TermVar(0, 2)
            )
    );
  print_term
    []
    (TermApp(identity "x" 0,
             identity "y" 0
            )
    );
  print_term
    []
    (TermApp(identity "x" 0,
             identity "x" 0
            )
    );
  print_term
    [("x", NameBinding) ; ("y", NameBinding)]
    (TermApp(identity "x" 2,
             identity "y" 2
            )
    );
  print_term
    [("x", NameBinding)]
    (TermApp(identity "x" 1,
             identity "x" 1
            )
    );
  print_term
    []
    (TermAbs("x",
             TermAbs("y",
                     TermApp(TermVar(1, 2),
                             TermVar(0, 2)
                            )
                    )
            )
    )
