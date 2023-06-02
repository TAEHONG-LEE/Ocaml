type t = (string * (string list * Ast.expr)) list

let empty : t = []

(* 함수 저장소에서 함수 이름으로 함수를 찾는 함수 *)
let rec find (name: string) (store: t) : (string list * Ast.expr) =
  match store with
  | [] -> failwith ("[Error] Unbound function: " ^ name)
  | (fname, (args, body)) :: rest -> if fname = name then (args, body) else find name rest

  let add (name : string) (lst : string list) (expr : Ast.expr) (store : t) : t =
    let rec add' name store =
      match store with
      | [] -> []
      | (fname, _) :: rest ->
          if fname = name then rest
          else (fname, find fname store) :: add' name rest
    in
    match store with
    | [] -> [(name, (lst, expr))]
    | _ ->
        let store' = add' name store in
        (name, (lst, expr)) :: store'