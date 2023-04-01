module F = Format

let rec len lst = 
  match lst with
  | [] -> 0
  | _ :: t -> 1 + (len t)

let rec rev lst =
  match lst with
  | [] -> []
  | h :: t -> (rev t) @ [h]

let rec map func lst =
  match lst with
  | [] -> []
  | h :: t -> func t :: (map func t)

let rec fold_right func i lst=
  match lst with
  | [] -> i 
  | h :: t -> fold_right func func(h, i) t
  

let _ = F.printf "%d\n" fold_right (fun (x,i) -> x + i) 0 [1;2;3] 
`