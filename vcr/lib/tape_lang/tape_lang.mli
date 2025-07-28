module Ast = Ast

val parse_lexbuf : Lexing.lexbuf -> (Ast.tape, string) result
val from_string : string -> (Ast.tape, string) result
val from_channel : in_channel -> (Ast.tape, string) result
