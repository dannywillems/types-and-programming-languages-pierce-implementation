{

open Lexing

exception SyntaxError of string

let next_line lexbuf =
  let pos = lexbuf.lex_curr_p in
  lexbuf.lex_curr_p <-
    {
      pos with pos_bol = lexbuf.Lexing.lex_curr_pos;
               pos_lnum = lexbuf.Lexing.pos_lnum + 1
    }

let tokens_list = [
  "fun", (fun info -> Parser.ABSTRACTION info);
  "lambda", (fun info -> Parser.ABSTRACTION info);
  ";;", (fun info -> Parser.DOUBLE_SEMICOLON info);
  ";", (fun info -> Parser.SEMICOLON info);
  "(", (fun info -> Parser.OPEN_BRACKET info);
  ")", (fun info -> Parser.CLOSE_BRACKET info);
]

let tokens_table = Hashtbl.create (List.length tokens_list)

let _ =
  List.iter (fun token, fn -> Hashtbl.add tokens_table token fn) tokens_list

let token_of_string string info =
  Hashtbl.find tokens_table string @@ info

(* Check if a variable is bounded *)
let is_bounded variable bounded_variables =
  List.mem variable bounded_variables

(* Check if a variable is free *)
let is_free variable free_variables =
  List.mem variable free_variables

(* Get the index of a value in a list *)
let rec get_index ?(i = 0) v l = match l with
  | [] -> failwith "The value doesn't exist in the list"
  | head :: tail -> if head = v then i else get_index ~i:(i + 1) v tail

(* Add a value to the end of a list (with complexity O(n)) *)
let add_at_the_end value l =
  List.rev (value :: (List.rev l))
}

let white = [' ' '\t']+
let newline = "\r" | "\n" | "\r\n"

(* Grammar for variables. *)
let variable_name = ['a' - 'z']+ '_' (['a' - 'z'] | ['A' - 'Z'] | '\'')*

rule read =
  parse
  | white {
      read lexbuf
    }
  | newline {
      next_line lexbuf;
      read lexbuf
    }
  | "(" | ")" | ";;" | ";" | "fun" | "lambda" {
      token_of_string (Lexing.lexeme lexbuf) Parsing_information.t_of_lexbuf lexbuf
    }
  | variable_name {
    Parser.ID(
      token_of_string
        (Parsing_information.t_of_lexbuf lexbuf)
        Lexing.lexeme lexbuf
    )
  }
  | _ { raise (SyntaxError ("Unexpected char: " ^ Lexing.lexeme lexbuf)) }
