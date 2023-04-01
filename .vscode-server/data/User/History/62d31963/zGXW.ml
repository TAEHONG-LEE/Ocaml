let rec len lst =
  match lst with
  | [] -> 0
  | _ :: t-> 1+ (len t)
in
let _ = Format.printf "%d" (len []) in
Format.printf "%d" (len [1;2])