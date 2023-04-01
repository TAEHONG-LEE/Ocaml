module F = Format

let rec len lst =
  match lst with
  | [] -> 0
  | _ :: t-> 1+ (len t)
in
let _ = F.printf "%d" (len [])