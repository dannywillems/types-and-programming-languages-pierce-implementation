%{
  let context_size = ref 0
  let new_variable () =
    incr context_size;
    Term.TermVar(!context_size - 1, !context_size)
%}

%token <Parsing_information.t, string> ID

%token <Parsing_information.t> ABSTRACTION
%token <Parsing_information.t> OPEN_BRACKET
%token <Parsing_information.t> CLOSE_BRACKET
%token <Parsing_information.t> DOUBLE_SEMICOLON
%token <Parsing_information.t> SEMICOLON

%token EOF

(* The start non terminal symbol is « prog » *)
%start <(Term.context * Term.t)> toplevel
%%

toplevel:
  | EOF { None }
  | expression DOUBLE_SEMICOLON { Some $1 }

expression:
  | ID { let (info, id) = $1 in Term.Var(info, 0, 0) }
  | ABSTRACTION ID expression {
      let (_, id) = $2 in
      Term.TermAbs($1, id, $3)
    }
  | OPEN_BRACKET expression expression CLOSE_BRACKET {
      Term.TermApp($1, $2, $3)
    }
