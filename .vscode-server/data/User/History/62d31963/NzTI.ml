module F = Format

let rec len lst = 
  match lst with
  | [] -> 0
  | _ :: t -> 1 + (len t)

let rec rev lst =
  match lst with
  | [] -> []
  | h :: t -> h :: (rev t)