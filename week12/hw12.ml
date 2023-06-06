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
  (* | Ast.Id id -> (
    Store.find id store
  ) *)
  | Ast.Id id -> (
    let value = Store.find id store in
    match value with
    | Store.NumV x -> Store.NumV x
    | Store.FreezedV (expr1, t) -> interp expr1 t
    | Store.ClosureV (s, expr1, t) -> Store.ClosureV (s, expr1, t)
  )
  | Ast.App (func, expr1) -> (
    (* 함수 호출 시 사용해야하는 arg값을 expr를 해석하여 저장한다*)
    let freezed = Store.FreezedV (expr1, store) in
    let (param, expr1, closure_store) = match interp func store with
    | Store.ClosureV (param, expr1, store) -> (param, expr1, store)
    | _ -> failwith (Format.asprintf "[Error] Not a function: %a" Ast.pp func) in
    let temp_store = Store.add param freezed closure_store in
    interp expr1 temp_store
  )

  (* | Ast.App (func, expr) -> begin
    let freezed = Store.FreezedV (expr, mstore) in
    let (param, expr, closure_mstore) = match interp func mstore with
    | Store.ClosureV (param, expr, mstore) -> (param, expr, mstore)
    | _ -> failwith (Format.asprintf "[Error] Not a function: %a" Ast.pp func) in
    (* Temporary mstore only for function evaluation *)
    let temporary_mstore = Store.add param freezed closure_mstore in
    interp expr temporary_mstore
end *)

  (* | Ast.LessThan (expr1, expr2) ->
    match interp expr1 store, interp expr2 store with
    | Store.NumV v1, Store.NumV v2 ->
      if v1 < v2 then Store.ClosureV ("x", Ast.Lambda("y", Ast.Id "x"),store) 
      else Store.ClosureV("x", Ast.Lambda("y", Ast.Id "y"),store) 
    | _ -> failwith (Format.asprintf "[Error] Not a number: %a < %a" Ast.pp expr1 Ast.pp expr2) *)
  | Ast.LessThan (expr1, expr2) -> (
    match interp expr1 store, interp expr2 store with
    | Store.NumV v1, Store.NumV v2 ->
      if v1 < v2 then interp (Ast.Lambda("x", Ast.Lambda("y", Ast.Id "x"))) store
      else interp (Ast.Lambda("x", Ast.Lambda("y", Ast.Id "y"))) store
    | _ -> failwith (Format.asprintf "[Error] Not a number: %a < %a" Ast.pp expr1 Ast.pp expr2)
    )
  | Ast.Lambda (param, expr1) -> (
      Store.ClosureV (param, expr1, store)
  )
  | Ast.RLetIn (x, e1, e2) ->
    begin
      match interp e1 store with
      | ClosureV (x', expr, s') ->
        let rec s'' = (x, Store.ClosureV (x', expr, s'')) :: s' in
      interp e2 s''
      | _ -> failwith (Format.asprintf "ClosureV required: %a" Ast.pp e1)
    end
      

(* let expected = Store.NumV 4
let actual = 
        interp (ParserMain.parse "(fun x y -> x + y) 1 3") Store.empty
let _ = assert (expected = actual)

let expected = Store.NumV 4
let actual = 
        interp (ParserMain.parse "let f = (fun x y -> x + y) in f 1 3") Store.empty
let _ = assert (expected = actual) *)

let prog = "let rec sum = (fun x -> if (x < 1) then 0 else x + (sum (x - 1))) in sum 10"
let _ = assert (interp (ParserMain.parse prog) Store.empty = Store.NumV 55)