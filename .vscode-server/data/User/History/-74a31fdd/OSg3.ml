type state =
  |Q0
  |Q1
  |Q2
  |Q3
  |Q4
  |Q5

let transfer (state : state) (input : char) =
  match state, input with
  | (_, '0' .. '9') -> Q1  
  | (Q1, '+') ->  Q2
  | (Q1, '-') -> Q3
  | (Q1, '*') -> Q4
  | (Q1, '/') -> Q5
  | _ -> failwith "rejected"

(* 
let lex str =
  let lex' lst state =
    ???
in lex' (List.of_seq (String.to_seq str)) Q0 *)

let lex (str : string) =
  let rec rec_lex state input= 
    match input with
    | [] -> if state = Q1 then true else failwith "rejected"
    | h :: t -> rec_lex (transfer state h) t
  in rec_lex Q0 (List.of_seq (String.to_seq str))


  
  

