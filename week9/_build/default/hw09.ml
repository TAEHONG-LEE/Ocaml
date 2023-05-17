let interp_def (fdef : Ast.fundef) (fstore : Fstore.t) : Fstore.t =
  match fdef with
  | Ast.FunDef (fname, stored_lst, stored_expr) -> Fstore.add fname stored_lst stored_expr fstore

let rec interp_expr (expr : Ast.expr) (fstore : Fstore.t) (str : Store.t) =
  match expr with
  | Ast.Num n -> Store.NumV n
  | Ast.Add (e1, e2) -> begin
    match interp_expr e1 fstore str, interp_expr e2 fstore str with
    | Store.NumV v1, Store.NumV v2 -> Store.NumV(v1 + v2)
  end
  | Ast.Sub (e1, e2) -> begin
    match interp_expr e1 fstore str, interp_expr e2 fstore str with
    | Store.NumV v1, Store.NumV v2 -> Store.NumV(v1 - v2)
  end
  | LetIn (x, e1, e2) -> begin
    let val1 = interp_expr e1 fstore str in
    let s1 = Store.add x val1 str in
    let expr2 = interp_expr e2 fstore s1 in
    expr2
  end
  | Ast.Id x -> Store.find x str
  | Ast.Call (fname, args) ->
    let (params, body) = Fstore.find fname fstore in
    if List.length params = List.length args then
      let arg_vals = List.map (fun arg -> interp_expr arg fstore str) args in
      let str' = List.fold_left2 (fun acc param arg_val -> Store.add param arg_val acc) str params arg_vals in
      interp_expr body fstore str'
    else
      failwith "Unmatched number of arguments"


(* type prog = Prog of fundef list * expr *)
let interp_prog (p: Ast.prog) : Store.value = 
  let store = [] in
  match p with
  | Prog (fundef_lst, e) -> (
    let fStore = List.fold_left (fun fs fdef -> interp_def fdef fs) [] fundef_lst in
    interp_expr e fStore store
  )


(* let%test _ = interp_expr (Add (Call ("f", [Num 3; Num 2]), Id "x")) [("f", (["x"; "y"], Add (Id "x", Id "y")))] [("x", NumV 10)]  =  Store.NumV 15
let%test _ = interp_def (Ast.FunDef ("f", ["x"; "y"], Ast.Add (Ast.Id "x", Ast.Id "y"))) Fstore.empty = [("f", (["x"; "y"], Ast.Add (Ast.Id "x", Ast.Id "y")))] *)
(* let%test _ = interp_prog (ParserMain.parse "def f1 x y = x + y endef def f2 x y = x - y endef f1(3, 4) + f2(4, 7)") = Store.NumV 4 *)