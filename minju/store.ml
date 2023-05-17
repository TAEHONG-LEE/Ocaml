type value = NumV of int
type t = (string * value) list
let empty : t = []

let rec find (str: string) (mem_list : t) : value = 
  match mem_list with
  | (x, NumV y)::t -> if x = str then NumV y else find str t
  | [] ->  failwith ("[Error] Free identifier: str")

let add (str: string) (value : value) (list : t) : t =
  match str,value  with
  | str, value -> [(str, value)]@(List.filter (fun (a,_) -> (a <> str)) list)