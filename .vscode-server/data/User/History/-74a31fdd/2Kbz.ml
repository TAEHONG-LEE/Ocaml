type state =
  |Q0
  |Q1
  |Q2
  |Q3
  |Q4
  |Q5

type op = ADD|SUB|MUL|DIV

type value = Int of int|FLoat of float|Err

type expr = E of op*expr*expr|V of value


let transfer (state : state) (input : char) =
  match state, input with
  | Q1, '+' ->  Q2
  | Q1, '-' -> Q3
  | Q1, '*' -> Q4
  | Q1, '/' -> Q5
  | _, '0' .. '9' -> Q1  
  | _ -> failwith "rejected"

let tokenize (str : string) : string list =
  let rec tokenize' char_lst state str str_lst =
    match char_lst, state with
    | h :: t, Q0 -> tokenize' t (transfer state h) (String.make 1 h) str_lst (*Q0에서 시작하는 경우*)
    | h :: t, Q1 -> if (transfer state h) = Q1 then tokenize' t (transfer state h) (str^(String.make 1 h)) str_lst (*Q1에서 digit이 입력되는 경우*) 
                    else tokenize' t (transfer state h) (String.make 1 h) (str_lst @ [str]) (*Q1에서 operator이 들어오는 경우 *)
    | h :: t, _ -> tokenize' t (transfer state h) (String.make 1 h) (str_lst @ [str])
    | [], Q1 -> str_lst @ [str] 
    | _ -> failwith "rejected"
  in tokenize' (List.of_seq (String.to_seq str)) Q0 "" []

let rec lex_to_expr str : expr =
  let tokens = tokenize str in
  match tokens with
  | [h] -> V(Int (int_of_string h))
  | h1::op::t ->
    let e1 = V(Int (int_of_string h1)) in
    let e2 = lex_to_expr (String.concat "" t) in
    (match op with
    | "+" -> E (ADD, e1, e2)
    | "-" -> E (SUB, e1, e2)
    | "*" -> E (MUL, e1, e2)
    | "/" -> E (DIV, e1, e2)
    | _ -> failwith "rejected")
  | _ -> failwith "rejected"

      
let lex (str : string) =
  let rec rec_lex state input= 
    match input with
    | [] -> if state = Q1 then true else failwith "rejected"
    | h :: t -> rec_lex (transfer state h) t
  in rec_lex Q0 (List.of_seq (String.to_seq str))

(* let lex (str : string) =
  let rec rec_lex state input= 
    match state, input with
    | Q1, [] -> true
    | _, [] -> failwith "rejected"
    | _, h :: t -> rec_lex (transfer state h) t
  in rec_lex Q0 (List.of_seq (String.to_seq str)) *)



(* let tokenize (str : string) : string list =
  let rec tokenize' char_lst state str str_lst =
    match char_lst, state with
    | [], Q1 -> str_lst
    | h :: t, Q0 -> tokenize' t (transfer Q0 h) (String.make 1 h) str_lst
    | h :: t, Q1 -> tokenize' t (transfer Q1 h) (String.make 1 h ^ str) str_lst
    | h :: t, _ -> tokenize' t (transfer 현재의 상태 h) 
  in tokenize' (List.of_seq (String.to_seq str)) Q0 "" [] *)


(* let tokenize (str : string) : string list =
  let rec tokenize' char_lst state str str_lst =
    match char_lst, state with
    | [], _ -> List.rev str
    | h :: t, Q0 -> tokenize' t (transfer Q0 h) str str_lst
    | h :: t, Q1 -> tokenize' t (transfer Q1 h) (String.make 1 h :: str) str_lst
    | h :: t, _ when h = '+' || h = '-' || h = '*' || h = '/' ->
        let token = List.rev str |> String.concat "" in
        tokenize' t (transfer Q0 h) (token :: String.make 1 h :: str) str_lst
    | h :: t, _ when h = ' ' || h = '\t' || h = '\n' ->
        tokenize' t Q0 str str_lst
    | h :: t, _ -> failwith "rejected"
  in
  tokenize' (List.of_seq (String.to_seq str)) Q0 [] [] *)