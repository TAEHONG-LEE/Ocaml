
(* The type of tokens. *)

type token = 
  | RIGHT_PARENTHESIS
  | PLUS
  | NUMBER of (int)
  | MINUS
  | LEFT_PARENTHESIS
  | EOF

(* This exception is raised by the monolithic API functions. *)

exception Error

(* The monolithic API. *)

val parse: (Lexing.lexbuf -> token) -> Lexing.lexbuf -> (Ast.expr)
