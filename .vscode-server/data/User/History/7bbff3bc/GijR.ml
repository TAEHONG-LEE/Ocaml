(* 함수 사용 방법 *)


fun x -> x + 1
let x = (fun x -> x + 1) 3 in 
Format.printf "Result : %d\n" x

