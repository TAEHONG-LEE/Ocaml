type state =
  |Q0
  |Q1
  |Q2
  |Q3
  |Q4
  |Q5

let transfer (state : state) (ch : char) =
  match state, ch with
  | _ -> Q1


let lex state =
  begin
    match List.of_seq(String.to_seq state) with
    | [] -> failwith "rejected"
    | '0' .. '9' -> Q1
    | 
    let rec 
      

            
  end
