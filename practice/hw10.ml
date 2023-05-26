let rec interp expr mstore = 
        (* No fstore needed, as functions are values *)
        match expr with
        | Ast.Num value -> Store.NumV value
        | Ast.Id id -> Store.find id mstore
        | Ast.Add (expr1, expr2) -> begin
                let evaluated1 = interp expr1 mstore in
                let evaluated2 = interp expr2 mstore in
                match (evaluated1, evaluated2) with
                | (Store.NumV x, Store.NumV y) -> Store.NumV (x + y)
                | _-> failwith (Format.asprintf "[Error] Not a number: %a + %a" Ast.pp expr1 Ast.pp expr2)
        end
        | Ast.Sub (expr1, expr2) -> begin
                let evaluated1 = interp expr1 mstore in
                let evaluated2 = interp expr2 mstore in
                match (evaluated1, evaluated2) with
                | (Store.NumV x, Store.NumV y) -> Store.NumV (x - y)
                | _-> failwith (Format.asprintf "[Error] Not a number: %a - %a" Ast.pp expr1 Ast.pp expr2)
        end
        | Ast.LetIn (var_name, expr1, expr2) -> begin
                let evaluated1 = interp expr1 mstore in
                let mstore =  Store.add var_name evaluated1 mstore in
                interp expr2 mstore
        end
        | Ast.Lambda (param, expr) -> begin
                Store.ClosureV (param, expr, mstore)
        end
        | Ast.App (func, expr) -> begin
                let argument = interp expr mstore in
                let (param, expr, closure_mstore) = match interp func mstore with
                | Store.ClosureV (param, expr, mstore) -> (param, expr, mstore)
                | _ -> failwith (Format.asprintf "[Error] Not a function: %a" Ast.pp func) in
                (* Temporary mstore only for function evaluation *)
                let temporary_mstore = Store.add param argument closure_mstore in
                interp expr temporary_mstore
        end
(*
let expected = Store.NumV 4
let actual = 
        interp (ParserMain.parse "(fun x y -> x + y) 1 3") Store.empty
let _ = assert (expected = actual)

let expected = Store.NumV 4
let actual = 
        interp (ParserMain.parse "let f = (fun x y -> x + y) in f 1 3") Store.empty
let _ = assert (expected = actual) *)
