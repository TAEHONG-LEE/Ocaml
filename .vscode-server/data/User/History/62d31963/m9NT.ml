module F = Format

  let rec len lst = 
    match lst with
    | [] -> 0
    | _ :: t ->
      1 + (len t)
  in
  let _ = F.printf "%d\n" (len []) in
  let _ = F.printf "%d\n" (len [1;2;3]) in
  F.printf "%d\n" (len [1;2]) 