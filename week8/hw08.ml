let rec interp (e: Ast.expr) (s:Store.t) =
  match e with
  | Ast.Num n -> Store.NumV n
  | Ast.Add (e1, e2) ->(
    match interp e1 s, interp e2 s with
    | Store.NumV v1, Store.NumV v2 -> Store.NumV(v1 + v2)
  )
  | Ast.Sub (e1, e2) ->(
    match interp e1 s, interp e2 s with
    | Store.NumV v1, Store.NumV v2 -> Store.NumV(v1 - v2)
  )
  | LetIn (x, e1, e2) -> (
    let val1 = interp e1 s in
    let s1 = Store.add x val1 s in
    let expr2 = interp e2 s1 in
    expr2
  )
  | Ast.Id x -> Store.find x s

(* let ast = ParserMain.parse "let y = 4 in let x = 3 in x + y - 3"
let Store.NumV intp = interp ast Store.empty
let _ = Format.printf "result = %d\n" intp *)