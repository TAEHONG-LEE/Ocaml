module F = Format

let rec len lst = 
  match lst with
  | [] -> 0
  | _ :: t ->
    1 + (len t)
  
let _ = F.printf "%d\n" (len []) 
let _ = F.printf "%d\n" (len [1;2;3]) 
let _ = F.printf "%d\n" (len [1;2]) 