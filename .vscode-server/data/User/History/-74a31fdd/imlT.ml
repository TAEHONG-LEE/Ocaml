type state =
  |Q0
  |Q1
  |Q2
  |Q3
  |Q4
  |Q5

type op = ADD|SUB|BUL|DIV
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


let lex_to_expr str : expr =
  let token_list = tokenize str in  (* 입력 문자열을 토큰 리스트로 변환 *)
  let rec parse_expr tokens =
    match tokens with
    | [] -> failwith "rejected"  (* 토큰이 더 이상 없는 경우 에러 처리 *)
    | [token] ->  (* 마지막 토큰인 경우, 피연산자인 값으로 변환 *)
      begin
        match token with
        | "Int" n -> V (Int (int_of_string n))  (* 정수인 경우, Int 값으로 변환 *)
        | "Float" f -> V (FLoat (float_of_string f))  (* 실수인 경우, Float 값으로 변환 *)
        | _ -> failwith "rejected"  (* 그 외의 토큰은 에러 처리 *)
      end
    | _ ->
      let rec find_operator tokens idx =
        match tokens with
        | [] -> failwith "rejected"  (* 토큰이 더 이상 없는 경우 에러 처리 *)
        | op :: tl ->
          begin
            match op with
            | "ADD" | "SUB" | "MUL" | "DIV" -> idx  (* 연산자 토큰을 찾은 경우, 해당 인덱스 반환 *)
            | _ -> find_operator tl (idx + 1)  (* 다음 토큰 탐색 *)
          end
      in
      let op_idx = find_operator tokens 0 in  (* 연산자 토큰의 인덱스 찾기 *)
      let op =  (* 인덱스를 통해 해당 연산자 토큰을 가져오기 *)
        match List.nth tokens op_idx with
        | "ADD" -> ADD
        | "SUB" -> SUB
        | "MUL" -> MUL
        | "DIV" -> DIV
        | _ -> failwith "rejected"  (* 연산자 토큰이 아닌 경우 에러 처리 *)
      in
      let left_expr = parse_expr (List.sub token_list 0 op_idx) in  (* 연산자 토큰 기준 왼쪽 토큰들로 재귀적으로 표현식 파싱 *)
      let right_expr = parse_expr (List.sub token_list (op_idx + 1) (List.length token_list - op_idx - 1)) in  (* 연산자 토큰 기준 오른쪽 토큰들로 재귀적으로 표현식 파싱 *)
      E (op, left_expr, right_expr)  (* 파싱한 연산자와 두 개의 재귀적인 표현식으로 새로운 `E` 노드 생성 *)
  in
  parse_expr token_list  (* 토큰 리스트로부터 표현식을 파싱하여 최종 `expr` 반환 *)
  

  



(* let lex (str : string) =
  let rec rec_lex state input= 
    match input with
    | [] -> if state = Q1 then true else failwith "rejected"
    | h :: t -> rec_lex (transfer state h) t
  in rec_lex Q0 (List.of_seq (String.to_seq str)) *)

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