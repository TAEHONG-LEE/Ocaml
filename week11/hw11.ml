let rec interp (expr : Ast.expr) (store : Store.t) = 
  match expr with
  | Ast.Num v -> Store.NumV v
  | Ast.Add (expr1, expr2) ->(
    match interp expr1 store, interp expr2 store with
    | Store.NumV v1, Store.NumV v2 -> Store.NumV(v1 + v2)
    | _-> failwith (Format.asprintf "[Error] Not a number: %a + %a" Ast.pp expr1 Ast.pp expr2)
  )
  | Ast.Sub (expr1, expr2) ->(
    match interp expr1 store, interp expr2 store with
    | Store.NumV v1, Store.NumV v2 -> Store.NumV(v1 - v2)
    | _-> failwith (Format.asprintf "[Error] Not a number: %a - %a" Ast.pp expr1 Ast.pp expr2)
  )
  | LetIn (name, expr1, expr2) -> (
    let val1 = interp expr1 store in
    let store1 = Store.add name val1 store in
    interp expr2 store1
  )
  | Ast.Id id -> (
    Store.find id store
  )
  | Ast.Lambda (param, expr1) -> (
    Store.ClosureV (param, expr1, store)
  )
  | Ast.App (func, expr1) -> (
    (* 함수 호출 시 사용해야하는 arg값을 expr를 해석하여 저장한다*)
    let arg = interp expr1 store in
    (* func을 해석하여 패턴매칭을 실행해야 한다.
       재귀를 이용하여 또 들어가고 또 들어가고를 생각한다
       만약 한풀 벗긴 값이 Store.ClosureV라면 param, expr1, store을 가지고 온다.*)
    let (param, expr1, closure_store) = match interp func store with
    | Store.ClosureV (param, expr1, store) -> (param, expr1, store)
    | _ -> failwith (Format.asprintf "[Error] Not a function: %a" Ast.pp func) in
    let temp_store = Store.add param arg closure_store in
    interp expr1 temp_store
  )
  (* | Ast.LessThan (expr1, expr2) ->
    match interp expr1 store, interp expr2 store with
    | Store.NumV v1, Store.NumV v2 ->
      if v1 < v2 then Store.ClosureV ("x", Ast.Lambda("y", Ast.Id "x"),store) 
      else Store.ClosureV("x", Ast.Lambda("y", Ast.Id "y"),store) 
    | _ -> failwith (Format.asprintf "[Error] Not a number: %a < %a" Ast.pp expr1 Ast.pp expr2) *)
  | Ast.LessThan (expr1, expr2) ->
    match interp expr1 store, interp expr2 store with
    | Store.NumV v1, Store.NumV v2 ->
      if v1 < v2 then interp (Ast.Lambda("x", Ast.Lambda("y", Ast.Id "x"))) store
      else interp (Ast.Lambda("x", Ast.Lambda("y", Ast.Id "y"))) store
    | _ -> failwith (Format.asprintf "[Error] Not a number: %a < %a" Ast.pp expr1 Ast.pp expr2)


let expected = Store.NumV 4
let actual = 
        interp (ParserMain.parse "(fun x y -> x + y) 1 3") Store.empty
let _ = assert (expected = actual)

let expected = Store.NumV 4
let actual = 
        interp (ParserMain.parse "let f = (fun x y -> x + y) in f 1 3") Store.empty
let _ = assert (expected = actual)
