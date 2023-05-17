open Ast
open Store
let interp a b =
  match a with
  | Num n -> NumV n
  | Id x -> Store.find x b
  | Add (e1, e2) ->
    begin
      match interp e1 b

let ast = ParserMain.parse "let y = 4 in let x = 3 in x + y - 3"
let Store.NumV intp = interp ast Store.empty
let _ = Format.printf "result = %d\n" intp