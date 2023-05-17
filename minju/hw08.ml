open Ast
open Store

let rec interp e s =
    match e with
    | Num n -> NumV n
    | Id x -> Store.find x s
    | Add (e1, e2) ->
        begin
            match interp e1 s, interp e2 s with
            | NumV n1, NumV n2 -> NumV (n1 + n2)
        end
    | Sub (e1, e2) ->

        
        begin 
            match interp e1 s, interp e2 s with
            | NumV n1, NumV n2 -> NumV (n1 - n2)
        end
    | LetIn (x, e1, e2) ->
        let v1 = interp e1 s in
        interp e2 (add x v1 s)

let _ = 
    let ex3 = ParserMain.parse "y + y" in
    let NumV ret = interp ex3 Store.empty in
    Format.printf "%d\n" ret
