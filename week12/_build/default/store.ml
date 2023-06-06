type t = (string * value) list
and value = NumV of int 
| ClosureV of string * Ast.expr * t
| FreezedV of Ast.expr * t

let empty : t = []

let rec find (str : string) (lst : t) =
  match lst with
  | [] -> failwith ("[Error] Free identifier: "^str)
  | (s, v) :: t -> if s = str then v else find str t

let rec add (str : string) (v : value) (lst : t) =
  match lst with
  | [] -> (str, v) :: lst
  | (s, e) :: t -> if s = str then (str, v) :: t else (s, e) :: add str v t

