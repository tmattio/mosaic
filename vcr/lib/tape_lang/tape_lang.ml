module Ast = Ast

let parse_lexbuf lexbuf =
  try Ok (Parser.tape Lexer.token lexbuf) with
  | Lexer.SyntaxError msg ->
      let pos = lexbuf.lex_curr_p in
      let err_msg =
        Printf.sprintf "Syntax error at line %d, column %d: %s" pos.pos_lnum
          (pos.pos_cnum - pos.pos_bol + 1)
          msg
      in
      Error err_msg
  | Parser.Error ->
      let pos = lexbuf.lex_curr_p in
      let err_msg =
        Printf.sprintf "Parser error at line %d, column %d near token '%s'"
          pos.pos_lnum
          (pos.pos_cnum - pos.pos_bol + 1)
          (Lexing.lexeme lexbuf)
      in
      Error err_msg

let from_string s =
  let lexbuf = Lexing.from_string s in
  parse_lexbuf lexbuf

let from_channel ic =
  let lexbuf = Lexing.from_channel ic in
  parse_lexbuf lexbuf
