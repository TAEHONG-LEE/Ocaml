open Ast
open Value

let rec interp (e:Ast.expr) : Value.t = 
  match e with
  | Num n -> NumV n
  | Add (e1, e2) ->(
    match interp e1, interp e2 with
    | NumV v1, NumV v2 -> NumV (v1 + v2))
  | Sub (e1, e2) ->(
    match interp e1, interp e2 with
    | NumV v1, NumV v2 -> NumV(v1 - v2))


let _ = Format.printf "%a" Ast.pp (ParserMain.parse "1+2-3")
let _ = Format.printf "%a" Ast.pp_ast (ParserMain.parse "1+2-3")
let _ = Format.printf "%a" Value.pp (interp(ParserMain.parse "1+2-3"))
