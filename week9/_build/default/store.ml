type value = NumV of int
type t = (string * value) list
let empty : t = []

let rec find (str : string) (lst : t) =
  match lst with
  | [] -> failwith ("[Error] Free identifier: "^str)
  | (s, v) :: t -> if s = str then v else find str t

let rec add (str : string) (v : value) (lst : t) =
  match lst with
  | [] -> (str, v) :: lst
  | (s, e) :: t -> if s = str then (str, v) :: t else (s, e) :: add str v t

