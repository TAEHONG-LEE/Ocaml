module F = Format
(* 리스트의 길이를 구하는 재귀 함수 작성 *)
let _ =
  let rec len lst = 
    match lst with
    | [] -> 0
    | _ :: t ->
    1 + (len t)
  in 
  F.printf "%d\n" (len [1;2;3;4;5])