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

type token = TL of string | TO of string


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

let rec to_token (str_lst : string list) : token list =
  match str_lst with
  | [] -> []
  | h :: t ->
      let token =
        match h with
        | "+" -> TO "+"
        | "-" -> TO "-"
        | "*" -> TO "*"
        | "/" -> TO "/"
        | _ -> TL h
      in
      token :: to_token t

let to_postfix (token_lst : token list) : token list =
  let rec to_postfix' token_lst stack =
    match token_lst with
    | [] -> List.rev stack
    | h :: t ->
      match h, stack with
      | TL _, [] -> to_postfix' t (h :: stack)
      | TO _, _ -> to_postfix' t (h :: stack)
      | TL _, h' :: t' ->
        match h' with
        | TO _ ->  to_postfix' t (h' :: (h :: t'))
      | _ -> failwith "rejected"
    in to_postfix' token_lst []

let parse (token_lst : token list) : expr =
  let rec parse' token_lst stack =
    match token_lst with
    | [] -> stack
    | h :: t ->
      match h, stack with
      | TL x, _ -> parse' t (V (Int (int_of_string x)) :: stack)
      | TO x, V (Int v1) :: V (Int v2) :: tail ->
        let op =
          match x with
          | "+" -> ADD
          | "-" -> SUB
          | "*" -> MUL
          | "/" -> DIV
          | _ -> failwith "rejected"
        in
        parse' t ((E (op, V (Int v2), V (Int v1))) :: tail)
      | TO x, V (Int v) :: E (operation, e1, e2) :: tail ->
        let op =
          match x with
          | "+" -> ADD
          | "-" -> SUB
          | "*" -> MUL
          | "/" -> DIV
          | _ -> failwith "rejected"
        in
        parse' t ((E (op, E (operation, e1, e2), V (Int v))) :: tail)
      | _, _ -> failwith "rejected" 
  in
  match parse' token_lst [] with
  | [expr] -> expr
  | _ -> failwith "rejected"



(* let rec parse (token_lst : token list) : expr =
  match token_lst with
  | [] ->  
  |   *)

(* let parse tokens =
  let rec parse' tokens stack =
    match tokens with
    | [] ->
      begin match stack with
      | [V v] -> v
      | _ -> failwith "rejected"
      end
    | TL x :: t -> parse' t (V (Int x) :: stack)
    | TO op :: t ->
      begin match stack with
      | right :: left :: stack' -> parse' t (E (op, left, right) :: stack')
      | _ -> failwith "rejected"
      end
  in
  match parse' tokens [] with
  | Int x -> V (Int x)
  | _ -> failwith "rejected" *)