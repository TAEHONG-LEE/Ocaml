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
  | h :: t -> func h :: (map func t)

let rec fold_right func i lst=
  match lst with
  | [] -> i
  | h :: t -> func(h, fold_right func i t )

  (* | h :: t -> fold_left func (func(h, i)) t*)


let _ = 
  let _ = F.printf "%d\n" len[]
  let _ = F.printf "%d\n" len[1;2;3]
  let _ = F.printf "%d\n\n" len[1;2;3;4;5;]

  let _ = List.iter (fun x -> F.printf "%d " x) rev [] in F.printf "\n"
  let _ = List.iter (fun x -> F.printf "%d " x) rev [1;2;3] in F.printf "\n"
  let _ = List.iter (fun x -> F.printf "%d " x) rev [1;2;3;4;5;6] in F.printf "\n\n"

  let _ = List.iter (fun x -> F.printf "%d " x) map (fun x -> x + 1) [] in F.printf "\n"
  let _ = List.iter (fun x -> F.printf "%d " x) map (fun x -> x + 1) [1;2;3] in F.printf "\n"
  let _ = List.iter (fun x -> F.printf "%d " x) map (fun x -> (string_of_int x) ^ "_string") [] in F.printf "\n"
  let _ = List.iter (fun x -> F.printf "%d " x) map (fun x -> (string_of_int x) ^ "_string") [1;2;3] in F.printf "\n\n"

  let _ = F.printf "%d\n" (fold_right (fun (x,i) -> x + i) 1 [1;2;3;4])
  let _ = F.printf "%d\n\n" (fold_right (fun (x,i) -> x * i) 1 [1;2;3;4])

