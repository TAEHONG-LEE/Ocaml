type state =
  |Q0
  |Q1
  |Q2
  |Q3
  |Q4
  |Q5

let transfer (state : state) (ch : char) =
  match state, ch with
  | (_, '0' .. '9') -> Q1  
  | (Q1, '+') ->  Q2
  | (Q1, '-') -> Q3
  | (Q1, '*') -> Q4
  | (Q1, '/') -> Q5
  | _ -> failwith "rejected"

let lex str =
  begin
    match List.of_seq(String.to_seq str) with
    | h::t ->
      let rec rec_transfer state ch =  
        begin
          match state, ch with
          | -> rec_transfer (transfer state) 
        end  
  end 


let lex str =
  let lex' lst state =
    ???
in lex' (List.of_seq (String.to_seq str)) Q0