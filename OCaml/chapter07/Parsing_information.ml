type t = {
  line : int;
  start_position : int;
  filename : string;
}

let t_of_lexbuf lexbuf = {
  line = lexbuf.Lexing.pos_lnum;
  start_position = lexbuf.Lexing.lex_start_pos;
  filename = lexbuf.Lexing.pos_fname;
}

let line_of_t = t.line
let start_position_of_t = t.start_position
let filename_of_t = t.filename
