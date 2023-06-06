
module MenhirBasics = struct
  
  exception Error
  
  let _eRR =
    fun _s ->
      raise Error
  
  type token = 
    | TRUE
    | RIGHT_PARENTHESIS
    | PLUS
    | NUMBER of (
# 4 "parser/parser.mly"
       (int)
# 18 "parser/parser.ml"
  )
    | NEG
    | MINUS
    | LESSTHAN
    | LEFT_PARENTHESIS
    | KW_THEN
    | KW_LET
    | KW_IN
    | KW_IF
    | KW_FUN
    | KW_ELSE
    | ID of (
# 5 "parser/parser.mly"
       (string)
# 33 "parser/parser.ml"
  )
    | FALSE
    | EQ
    | EOF
    | ARROW
  
end

include MenhirBasics

# 1 "parser/parser.mly"
  
  open Ast

# 48 "parser/parser.ml"

type ('s, 'r) _menhir_state = 
  | MenhirState00 : ('s, _menhir_box_parse) _menhir_state
    (** State 00.
        Stack shape : .
        Start symbol: parse. *)

  | MenhirState03 : (('s, _menhir_box_parse) _menhir_cell1_NEG, _menhir_box_parse) _menhir_state
    (** State 03.
        Stack shape : NEG.
        Start symbol: parse. *)

  | MenhirState04 : ((('s, _menhir_box_parse) _menhir_cell1_NEG, _menhir_box_parse) _menhir_cell1_LEFT_PARENTHESIS, _menhir_box_parse) _menhir_state
    (** State 04.
        Stack shape : NEG LEFT_PARENTHESIS.
        Start symbol: parse. *)

  | MenhirState05 : (('s, _menhir_box_parse) _menhir_cell1_LEFT_PARENTHESIS, _menhir_box_parse) _menhir_state
    (** State 05.
        Stack shape : LEFT_PARENTHESIS.
        Start symbol: parse. *)

  | MenhirState07 : (('s, _menhir_box_parse) _menhir_cell1_KW_LET _menhir_cell0_ID, _menhir_box_parse) _menhir_state
    (** State 07.
        Stack shape : KW_LET ID.
        Start symbol: parse. *)

  | MenhirState08 : (('s, _menhir_box_parse) _menhir_cell1_ID, _menhir_box_parse) _menhir_state
    (** State 08.
        Stack shape : ID.
        Start symbol: parse. *)

  | MenhirState10 : ((('s, _menhir_box_parse) _menhir_cell1_KW_LET _menhir_cell0_ID, _menhir_box_parse) _menhir_cell1_EQ, _menhir_box_parse) _menhir_state
    (** State 10.
        Stack shape : KW_LET ID EQ.
        Start symbol: parse. *)

  | MenhirState11 : (('s, _menhir_box_parse) _menhir_cell1_KW_IF, _menhir_box_parse) _menhir_state
    (** State 11.
        Stack shape : KW_IF.
        Start symbol: parse. *)

  | MenhirState15 : ((('s, _menhir_box_parse) _menhir_cell1_KW_IF, _menhir_box_parse) _menhir_cell1_expr, _menhir_box_parse) _menhir_state
    (** State 15.
        Stack shape : KW_IF expr.
        Start symbol: parse. *)

  | MenhirState16 : ((('s, _menhir_box_parse) _menhir_cell1_expr, _menhir_box_parse) _menhir_cell1_PLUS, _menhir_box_parse) _menhir_state
    (** State 16.
        Stack shape : expr PLUS.
        Start symbol: parse. *)

  | MenhirState17 : (((('s, _menhir_box_parse) _menhir_cell1_expr, _menhir_box_parse) _menhir_cell1_PLUS, _menhir_box_parse) _menhir_cell1_expr, _menhir_box_parse) _menhir_state
    (** State 17.
        Stack shape : expr PLUS expr.
        Start symbol: parse. *)

  | MenhirState18 : ((('s, _menhir_box_parse) _menhir_cell1_expr, _menhir_box_parse) _menhir_cell1_expr, _menhir_box_parse) _menhir_state
    (** State 18.
        Stack shape : expr expr.
        Start symbol: parse. *)

  | MenhirState19 : ((('s, _menhir_box_parse) _menhir_cell1_expr, _menhir_box_parse) _menhir_cell1_MINUS, _menhir_box_parse) _menhir_state
    (** State 19.
        Stack shape : expr MINUS.
        Start symbol: parse. *)

  | MenhirState20 : (((('s, _menhir_box_parse) _menhir_cell1_expr, _menhir_box_parse) _menhir_cell1_MINUS, _menhir_box_parse) _menhir_cell1_expr, _menhir_box_parse) _menhir_state
    (** State 20.
        Stack shape : expr MINUS expr.
        Start symbol: parse. *)

  | MenhirState21 : ((('s, _menhir_box_parse) _menhir_cell1_expr, _menhir_box_parse) _menhir_cell1_LESSTHAN, _menhir_box_parse) _menhir_state
    (** State 21.
        Stack shape : expr LESSTHAN.
        Start symbol: parse. *)

  | MenhirState22 : (((('s, _menhir_box_parse) _menhir_cell1_expr, _menhir_box_parse) _menhir_cell1_LESSTHAN, _menhir_box_parse) _menhir_cell1_expr, _menhir_box_parse) _menhir_state
    (** State 22.
        Stack shape : expr LESSTHAN expr.
        Start symbol: parse. *)

  | MenhirState23 : (((('s, _menhir_box_parse) _menhir_cell1_KW_IF, _menhir_box_parse) _menhir_cell1_expr, _menhir_box_parse) _menhir_cell1_KW_THEN, _menhir_box_parse) _menhir_state
    (** State 23.
        Stack shape : KW_IF expr KW_THEN.
        Start symbol: parse. *)

  | MenhirState24 : ((((('s, _menhir_box_parse) _menhir_cell1_KW_IF, _menhir_box_parse) _menhir_cell1_expr, _menhir_box_parse) _menhir_cell1_KW_THEN, _menhir_box_parse) _menhir_cell1_expr, _menhir_box_parse) _menhir_state
    (** State 24.
        Stack shape : KW_IF expr KW_THEN expr.
        Start symbol: parse. *)

  | MenhirState25 : (((((('s, _menhir_box_parse) _menhir_cell1_KW_IF, _menhir_box_parse) _menhir_cell1_expr, _menhir_box_parse) _menhir_cell1_KW_THEN, _menhir_box_parse) _menhir_cell1_expr, _menhir_box_parse) _menhir_cell1_KW_ELSE, _menhir_box_parse) _menhir_state
    (** State 25.
        Stack shape : KW_IF expr KW_THEN expr KW_ELSE.
        Start symbol: parse. *)

  | MenhirState26 : ((((((('s, _menhir_box_parse) _menhir_cell1_KW_IF, _menhir_box_parse) _menhir_cell1_expr, _menhir_box_parse) _menhir_cell1_KW_THEN, _menhir_box_parse) _menhir_cell1_expr, _menhir_box_parse) _menhir_cell1_KW_ELSE, _menhir_box_parse) _menhir_cell1_expr, _menhir_box_parse) _menhir_state
    (** State 26.
        Stack shape : KW_IF expr KW_THEN expr KW_ELSE expr.
        Start symbol: parse. *)

  | MenhirState27 : (((('s, _menhir_box_parse) _menhir_cell1_KW_LET _menhir_cell0_ID, _menhir_box_parse) _menhir_cell1_EQ, _menhir_box_parse) _menhir_cell1_expr, _menhir_box_parse) _menhir_state
    (** State 27.
        Stack shape : KW_LET ID EQ expr.
        Start symbol: parse. *)

  | MenhirState28 : ((((('s, _menhir_box_parse) _menhir_cell1_KW_LET _menhir_cell0_ID, _menhir_box_parse) _menhir_cell1_EQ, _menhir_box_parse) _menhir_cell1_expr, _menhir_box_parse) _menhir_cell1_KW_IN, _menhir_box_parse) _menhir_state
    (** State 28.
        Stack shape : KW_LET ID EQ expr KW_IN.
        Start symbol: parse. *)

  | MenhirState29 : (((((('s, _menhir_box_parse) _menhir_cell1_KW_LET _menhir_cell0_ID, _menhir_box_parse) _menhir_cell1_EQ, _menhir_box_parse) _menhir_cell1_expr, _menhir_box_parse) _menhir_cell1_KW_IN, _menhir_box_parse) _menhir_cell1_expr, _menhir_box_parse) _menhir_state
    (** State 29.
        Stack shape : KW_LET ID EQ expr KW_IN expr.
        Start symbol: parse. *)

  | MenhirState31 : ((('s, _menhir_box_parse) _menhir_cell1_KW_LET _menhir_cell0_ID, _menhir_box_parse) _menhir_cell1_var_list, _menhir_box_parse) _menhir_state
    (** State 31.
        Stack shape : KW_LET ID var_list.
        Start symbol: parse. *)

  | MenhirState32 : (((('s, _menhir_box_parse) _menhir_cell1_KW_LET _menhir_cell0_ID, _menhir_box_parse) _menhir_cell1_var_list, _menhir_box_parse) _menhir_cell1_expr, _menhir_box_parse) _menhir_state
    (** State 32.
        Stack shape : KW_LET ID var_list expr.
        Start symbol: parse. *)

  | MenhirState33 : ((((('s, _menhir_box_parse) _menhir_cell1_KW_LET _menhir_cell0_ID, _menhir_box_parse) _menhir_cell1_var_list, _menhir_box_parse) _menhir_cell1_expr, _menhir_box_parse) _menhir_cell1_KW_IN, _menhir_box_parse) _menhir_state
    (** State 33.
        Stack shape : KW_LET ID var_list expr KW_IN.
        Start symbol: parse. *)

  | MenhirState34 : (((((('s, _menhir_box_parse) _menhir_cell1_KW_LET _menhir_cell0_ID, _menhir_box_parse) _menhir_cell1_var_list, _menhir_box_parse) _menhir_cell1_expr, _menhir_box_parse) _menhir_cell1_KW_IN, _menhir_box_parse) _menhir_cell1_expr, _menhir_box_parse) _menhir_state
    (** State 34.
        Stack shape : KW_LET ID var_list expr KW_IN expr.
        Start symbol: parse. *)

  | MenhirState35 : ((('s, _menhir_box_parse) _menhir_cell1_LEFT_PARENTHESIS, _menhir_box_parse) _menhir_cell1_KW_FUN, _menhir_box_parse) _menhir_state
    (** State 35.
        Stack shape : LEFT_PARENTHESIS KW_FUN.
        Start symbol: parse. *)

  | MenhirState37 : (((('s, _menhir_box_parse) _menhir_cell1_LEFT_PARENTHESIS, _menhir_box_parse) _menhir_cell1_KW_FUN, _menhir_box_parse) _menhir_cell1_var_list, _menhir_box_parse) _menhir_state
    (** State 37.
        Stack shape : LEFT_PARENTHESIS KW_FUN var_list.
        Start symbol: parse. *)

  | MenhirState38 : ((((('s, _menhir_box_parse) _menhir_cell1_LEFT_PARENTHESIS, _menhir_box_parse) _menhir_cell1_KW_FUN, _menhir_box_parse) _menhir_cell1_var_list, _menhir_box_parse) _menhir_cell1_expr, _menhir_box_parse) _menhir_state
    (** State 38.
        Stack shape : LEFT_PARENTHESIS KW_FUN var_list expr.
        Start symbol: parse. *)

  | MenhirState40 : ((('s, _menhir_box_parse) _menhir_cell1_LEFT_PARENTHESIS, _menhir_box_parse) _menhir_cell1_expr, _menhir_box_parse) _menhir_state
    (** State 40.
        Stack shape : LEFT_PARENTHESIS expr.
        Start symbol: parse. *)

  | MenhirState44 : (('s, _menhir_box_parse) _menhir_cell1_expr, _menhir_box_parse) _menhir_state
    (** State 44.
        Stack shape : expr.
        Start symbol: parse. *)


and ('s, 'r) _menhir_cell1_expr = 
  | MenhirCell1_expr of 's * ('s, 'r) _menhir_state * (Ast.expr)

and ('s, 'r) _menhir_cell1_var_list = 
  | MenhirCell1_var_list of 's * ('s, 'r) _menhir_state * (string list)

and ('s, 'r) _menhir_cell1_EQ = 
  | MenhirCell1_EQ of 's * ('s, 'r) _menhir_state

and ('s, 'r) _menhir_cell1_ID = 
  | MenhirCell1_ID of 's * ('s, 'r) _menhir_state * (
# 5 "parser/parser.mly"
       (string)
# 225 "parser/parser.ml"
)

and 's _menhir_cell0_ID = 
  | MenhirCell0_ID of 's * (
# 5 "parser/parser.mly"
       (string)
# 232 "parser/parser.ml"
)

and ('s, 'r) _menhir_cell1_KW_ELSE = 
  | MenhirCell1_KW_ELSE of 's * ('s, 'r) _menhir_state

and ('s, 'r) _menhir_cell1_KW_FUN = 
  | MenhirCell1_KW_FUN of 's * ('s, 'r) _menhir_state

and ('s, 'r) _menhir_cell1_KW_IF = 
  | MenhirCell1_KW_IF of 's * ('s, 'r) _menhir_state

and ('s, 'r) _menhir_cell1_KW_IN = 
  | MenhirCell1_KW_IN of 's * ('s, 'r) _menhir_state

and ('s, 'r) _menhir_cell1_KW_LET = 
  | MenhirCell1_KW_LET of 's * ('s, 'r) _menhir_state

and ('s, 'r) _menhir_cell1_KW_THEN = 
  | MenhirCell1_KW_THEN of 's * ('s, 'r) _menhir_state

and ('s, 'r) _menhir_cell1_LEFT_PARENTHESIS = 
  | MenhirCell1_LEFT_PARENTHESIS of 's * ('s, 'r) _menhir_state

and ('s, 'r) _menhir_cell1_LESSTHAN = 
  | MenhirCell1_LESSTHAN of 's * ('s, 'r) _menhir_state

and ('s, 'r) _menhir_cell1_MINUS = 
  | MenhirCell1_MINUS of 's * ('s, 'r) _menhir_state

and ('s, 'r) _menhir_cell1_NEG = 
  | MenhirCell1_NEG of 's * ('s, 'r) _menhir_state

and ('s, 'r) _menhir_cell1_PLUS = 
  | MenhirCell1_PLUS of 's * ('s, 'r) _menhir_state

and _menhir_box_parse = 
  | MenhirBox_parse of (Ast.expr) [@@unboxed]

let _menhir_action_01 =
  fun e1 e2 x ->
    (
# 26 "parser/parser.mly"
                                                   ( LetIn (x, e1, e2) )
# 276 "parser/parser.ml"
     : (Ast.expr))

let _menhir_action_02 =
  fun e1 e2 ->
    (
# 27 "parser/parser.mly"
                               ( App (e1, e2) )
# 284 "parser/parser.ml"
     : (Ast.expr))

let _menhir_action_03 =
  fun e1 e2 x xlst ->
    (
# 28 "parser/parser.mly"
                                                                 ( 
      LetIn (x, List.fold_right (fun x e -> Lambda (x, e)) xlst e1, e2)
    )
# 294 "parser/parser.ml"
     : (Ast.expr))

let _menhir_action_04 =
  fun e xlst ->
    (
# 31 "parser/parser.mly"
                                                                         ( 
      List.fold_right (fun x e -> Lambda (x, e)) xlst e
    )
# 304 "parser/parser.ml"
     : (Ast.expr))

let _menhir_action_05 =
  fun e1 e2 e3 ->
    (
# 34 "parser/parser.mly"
                                                            ( App (App (e1, e2), e3) )
# 312 "parser/parser.ml"
     : (Ast.expr))

let _menhir_action_06 =
  fun x ->
    (
# 35 "parser/parser.mly"
         ( Id x )
# 320 "parser/parser.ml"
     : (Ast.expr))

let _menhir_action_07 =
  fun () ->
    (
# 36 "parser/parser.mly"
         ( true_encoding )
# 328 "parser/parser.ml"
     : (Ast.expr))

let _menhir_action_08 =
  fun () ->
    (
# 37 "parser/parser.mly"
          ( false_encoding )
# 336 "parser/parser.ml"
     : (Ast.expr))

let _menhir_action_09 =
  fun e1 e2 ->
    (
# 38 "parser/parser.mly"
                             ( LessThan (e1, e2) )
# 344 "parser/parser.ml"
     : (Ast.expr))

let _menhir_action_10 =
  fun e1 e2 ->
    (
# 39 "parser/parser.mly"
                         ( Add (e1, e2) )
# 352 "parser/parser.ml"
     : (Ast.expr))

let _menhir_action_11 =
  fun e1 e2 ->
    (
# 40 "parser/parser.mly"
                          ( Sub (e1, e2) )
# 360 "parser/parser.ml"
     : (Ast.expr))

let _menhir_action_12 =
  fun n ->
    (
# 41 "parser/parser.mly"
             ( Num n )
# 368 "parser/parser.ml"
     : (Ast.expr))

let _menhir_action_13 =
  fun e ->
    (
# 42 "parser/parser.mly"
                     ( Sub (Num 0, e))
# 376 "parser/parser.ml"
     : (Ast.expr))

let _menhir_action_14 =
  fun e ->
    (
# 43 "parser/parser.mly"
                 ( e )
# 384 "parser/parser.ml"
     : (Ast.expr))

let _menhir_action_15 =
  fun e ->
    (
# 50 "parser/parser.mly"
                                              ( e )
# 392 "parser/parser.ml"
     : (Ast.expr))

let _menhir_action_16 =
  fun e ->
    (
# 23 "parser/parser.mly"
               ( e )
# 400 "parser/parser.ml"
     : (Ast.expr))

let _menhir_action_17 =
  fun x ->
    (
# 46 "parser/parser.mly"
         ( [x] )
# 408 "parser/parser.ml"
     : (string list))

let _menhir_action_18 =
  fun x xlst ->
    (
# 47 "parser/parser.mly"
                       ( x :: xlst )
# 416 "parser/parser.ml"
     : (string list))

let _menhir_print_token : token -> string =
  fun _tok ->
    match _tok with
    | ARROW ->
        "ARROW"
    | EOF ->
        "EOF"
    | EQ ->
        "EQ"
    | FALSE ->
        "FALSE"
    | ID _ ->
        "ID"
    | KW_ELSE ->
        "KW_ELSE"
    | KW_FUN ->
        "KW_FUN"
    | KW_IF ->
        "KW_IF"
    | KW_IN ->
        "KW_IN"
    | KW_LET ->
        "KW_LET"
    | KW_THEN ->
        "KW_THEN"
    | LEFT_PARENTHESIS ->
        "LEFT_PARENTHESIS"
    | LESSTHAN ->
        "LESSTHAN"
    | MINUS ->
        "MINUS"
    | NEG ->
        "NEG"
    | NUMBER _ ->
        "NUMBER"
    | PLUS ->
        "PLUS"
    | RIGHT_PARENTHESIS ->
        "RIGHT_PARENTHESIS"
    | TRUE ->
        "TRUE"

let _menhir_fail : unit -> 'a =
  fun () ->
    Printf.eprintf "Internal failure -- please contact the parser generator's developers.\n%!";
    assert false

include struct
  
  [@@@ocaml.warning "-4-37-39"]
  
  let rec _menhir_run_44 : type  ttv_stack. ttv_stack -> _ -> _ -> _ -> (ttv_stack, _menhir_box_parse) _menhir_state -> _ -> _menhir_box_parse =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      match (_tok : MenhirBasics.token) with
      | TRUE ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          let _tok = _menhir_lexer _menhir_lexbuf in
          let _v = _menhir_action_07 () in
          _menhir_run_18 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | PLUS ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_16 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState44
      | NUMBER _v_1 ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          let _tok = _menhir_lexer _menhir_lexbuf in
          let n = _v_1 in
          let _v = _menhir_action_12 n in
          _menhir_run_18 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | NEG ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_03 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState44
      | MINUS ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_19 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState44
      | LESSTHAN ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_21 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState44
      | LEFT_PARENTHESIS ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_05 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState44
      | KW_LET ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_06 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState44
      | KW_IF ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_11 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState44
      | ID _v_3 ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          let _tok = _menhir_lexer _menhir_lexbuf in
          let x = _v_3 in
          let _v = _menhir_action_06 x in
          _menhir_run_18 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | FALSE ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          let _tok = _menhir_lexer _menhir_lexbuf in
          let _v = _menhir_action_08 () in
          _menhir_run_18 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | EOF ->
          let e = _v in
          let _v = _menhir_action_16 e in
          MenhirBox_parse _v
      | _ ->
          _eRR ()
  
  and _menhir_run_18 : type  ttv_stack. (ttv_stack, _menhir_box_parse) _menhir_cell1_expr -> _ -> _ -> _ -> _ -> _menhir_box_parse =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok ->
      let MenhirCell1_expr (_menhir_stack, _menhir_s, e1) = _menhir_stack in
      let e2 = _v in
      let _v = _menhir_action_02 e1 e2 in
      _menhir_goto_expr _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
  
  and _menhir_goto_expr : type  ttv_stack. ttv_stack -> _ -> _ -> _ -> (ttv_stack, _menhir_box_parse) _menhir_state -> _ -> _menhir_box_parse =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      match _menhir_s with
      | MenhirState00 ->
          _menhir_run_44 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState04 ->
          _menhir_run_40 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState05 ->
          _menhir_run_40 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState37 ->
          _menhir_run_38 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState33 ->
          _menhir_run_34 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState31 ->
          _menhir_run_32 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState28 ->
          _menhir_run_29 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState10 ->
          _menhir_run_27 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState25 ->
          _menhir_run_26 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState23 ->
          _menhir_run_24 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState21 ->
          _menhir_run_22 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState19 ->
          _menhir_run_20 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState44 ->
          _menhir_run_18 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | MenhirState40 ->
          _menhir_run_18 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | MenhirState38 ->
          _menhir_run_18 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | MenhirState32 ->
          _menhir_run_18 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | MenhirState34 ->
          _menhir_run_18 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | MenhirState27 ->
          _menhir_run_18 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | MenhirState29 ->
          _menhir_run_18 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | MenhirState15 ->
          _menhir_run_18 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | MenhirState24 ->
          _menhir_run_18 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | MenhirState26 ->
          _menhir_run_18 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | MenhirState22 ->
          _menhir_run_18 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | MenhirState20 ->
          _menhir_run_18 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | MenhirState18 ->
          _menhir_run_18 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | MenhirState17 ->
          _menhir_run_18 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | MenhirState16 ->
          _menhir_run_17 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState11 ->
          _menhir_run_15 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | _ ->
          _menhir_fail ()
  
  and _menhir_run_40 : type  ttv_stack. ((ttv_stack, _menhir_box_parse) _menhir_cell1_LEFT_PARENTHESIS as 'stack) -> _ -> _ -> _ -> ('stack, _menhir_box_parse) _menhir_state -> _ -> _menhir_box_parse =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      match (_tok : MenhirBasics.token) with
      | TRUE ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          let _tok = _menhir_lexer _menhir_lexbuf in
          let _v = _menhir_action_07 () in
          _menhir_run_18 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | RIGHT_PARENTHESIS ->
          let _tok = _menhir_lexer _menhir_lexbuf in
          let MenhirCell1_LEFT_PARENTHESIS (_menhir_stack, _menhir_s) = _menhir_stack in
          let e = _v in
          let _v = _menhir_action_15 e in
          _menhir_goto_paren_expr _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | PLUS ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_16 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState40
      | NUMBER _v_1 ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          let _tok = _menhir_lexer _menhir_lexbuf in
          let n = _v_1 in
          let _v = _menhir_action_12 n in
          _menhir_run_18 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | NEG ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_03 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState40
      | MINUS ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_19 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState40
      | LESSTHAN ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_21 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState40
      | LEFT_PARENTHESIS ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_05 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState40
      | KW_LET ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_06 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState40
      | KW_IF ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_11 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState40
      | ID _v_3 ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          let _tok = _menhir_lexer _menhir_lexbuf in
          let x = _v_3 in
          let _v = _menhir_action_06 x in
          _menhir_run_18 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | FALSE ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          let _tok = _menhir_lexer _menhir_lexbuf in
          let _v = _menhir_action_08 () in
          _menhir_run_18 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | _ ->
          _eRR ()
  
  and _menhir_goto_paren_expr : type  ttv_stack. ttv_stack -> _ -> _ -> _ -> (ttv_stack, _menhir_box_parse) _menhir_state -> _ -> _menhir_box_parse =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      match _menhir_s with
      | MenhirState03 ->
          _menhir_run_42 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | MenhirState44 ->
          _menhir_run_14_spec_44 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | MenhirState00 ->
          _menhir_run_14_spec_00 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | MenhirState04 ->
          _menhir_run_14_spec_04 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | MenhirState40 ->
          _menhir_run_14_spec_40 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | MenhirState05 ->
          _menhir_run_14_spec_05 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | MenhirState38 ->
          _menhir_run_14_spec_38 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | MenhirState37 ->
          _menhir_run_14_spec_37 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | MenhirState32 ->
          _menhir_run_14_spec_32 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | MenhirState34 ->
          _menhir_run_14_spec_34 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | MenhirState33 ->
          _menhir_run_14_spec_33 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | MenhirState31 ->
          _menhir_run_14_spec_31 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | MenhirState27 ->
          _menhir_run_14_spec_27 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | MenhirState29 ->
          _menhir_run_14_spec_29 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | MenhirState28 ->
          _menhir_run_14_spec_28 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | MenhirState10 ->
          _menhir_run_14_spec_10 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | MenhirState15 ->
          _menhir_run_14_spec_15 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | MenhirState24 ->
          _menhir_run_14_spec_24 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | MenhirState26 ->
          _menhir_run_14_spec_26 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | MenhirState25 ->
          _menhir_run_14_spec_25 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | MenhirState23 ->
          _menhir_run_14_spec_23 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | MenhirState22 ->
          _menhir_run_14_spec_22 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | MenhirState21 ->
          _menhir_run_14_spec_21 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | MenhirState20 ->
          _menhir_run_14_spec_20 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | MenhirState19 ->
          _menhir_run_14_spec_19 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | MenhirState18 ->
          _menhir_run_14_spec_18 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | MenhirState17 ->
          _menhir_run_14_spec_17 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | MenhirState16 ->
          _menhir_run_14_spec_16 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | MenhirState11 ->
          _menhir_run_14_spec_11 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | _ ->
          _menhir_fail ()
  
  and _menhir_run_42 : type  ttv_stack. (ttv_stack, _menhir_box_parse) _menhir_cell1_NEG -> _ -> _ -> _ -> _ -> _menhir_box_parse =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok ->
      let MenhirCell1_NEG (_menhir_stack, _menhir_s) = _menhir_stack in
      let e = _v in
      let _v = _menhir_action_13 e in
      _menhir_goto_expr _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
  
  and _menhir_run_14_spec_44 : type  ttv_stack. (ttv_stack, _menhir_box_parse) _menhir_cell1_expr -> _ -> _ -> _ -> _ -> _menhir_box_parse =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok ->
      let e = _v in
      let _v = _menhir_action_14 e in
      _menhir_run_18 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
  
  and _menhir_run_14_spec_00 : type  ttv_stack. ttv_stack -> _ -> _ -> _ -> _ -> _menhir_box_parse =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok ->
      let e = _v in
      let _v = _menhir_action_14 e in
      _menhir_run_44 _menhir_stack _menhir_lexbuf _menhir_lexer _v MenhirState00 _tok
  
  and _menhir_run_14_spec_04 : type  ttv_stack. ((ttv_stack, _menhir_box_parse) _menhir_cell1_NEG, _menhir_box_parse) _menhir_cell1_LEFT_PARENTHESIS -> _ -> _ -> _ -> _ -> _menhir_box_parse =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok ->
      let e = _v in
      let _v = _menhir_action_14 e in
      _menhir_run_40 _menhir_stack _menhir_lexbuf _menhir_lexer _v MenhirState04 _tok
  
  and _menhir_run_14_spec_40 : type  ttv_stack. ((ttv_stack, _menhir_box_parse) _menhir_cell1_LEFT_PARENTHESIS, _menhir_box_parse) _menhir_cell1_expr -> _ -> _ -> _ -> _ -> _menhir_box_parse =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok ->
      let e = _v in
      let _v = _menhir_action_14 e in
      _menhir_run_18 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
  
  and _menhir_run_14_spec_05 : type  ttv_stack. (ttv_stack, _menhir_box_parse) _menhir_cell1_LEFT_PARENTHESIS -> _ -> _ -> _ -> _ -> _menhir_box_parse =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok ->
      let e = _v in
      let _v = _menhir_action_14 e in
      _menhir_run_40 _menhir_stack _menhir_lexbuf _menhir_lexer _v MenhirState05 _tok
  
  and _menhir_run_14_spec_38 : type  ttv_stack. ((((ttv_stack, _menhir_box_parse) _menhir_cell1_LEFT_PARENTHESIS, _menhir_box_parse) _menhir_cell1_KW_FUN, _menhir_box_parse) _menhir_cell1_var_list, _menhir_box_parse) _menhir_cell1_expr -> _ -> _ -> _ -> _ -> _menhir_box_parse =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok ->
      let e = _v in
      let _v = _menhir_action_14 e in
      _menhir_run_18 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
  
  and _menhir_run_14_spec_37 : type  ttv_stack. (((ttv_stack, _menhir_box_parse) _menhir_cell1_LEFT_PARENTHESIS, _menhir_box_parse) _menhir_cell1_KW_FUN, _menhir_box_parse) _menhir_cell1_var_list -> _ -> _ -> _ -> _ -> _menhir_box_parse =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok ->
      let e = _v in
      let _v = _menhir_action_14 e in
      _menhir_run_38 _menhir_stack _menhir_lexbuf _menhir_lexer _v MenhirState37 _tok
  
  and _menhir_run_38 : type  ttv_stack. ((((ttv_stack, _menhir_box_parse) _menhir_cell1_LEFT_PARENTHESIS, _menhir_box_parse) _menhir_cell1_KW_FUN, _menhir_box_parse) _menhir_cell1_var_list as 'stack) -> _ -> _ -> _ -> ('stack, _menhir_box_parse) _menhir_state -> _ -> _menhir_box_parse =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      match (_tok : MenhirBasics.token) with
      | TRUE ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          let _tok = _menhir_lexer _menhir_lexbuf in
          let _v = _menhir_action_07 () in
          _menhir_run_18 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | RIGHT_PARENTHESIS ->
          let _tok = _menhir_lexer _menhir_lexbuf in
          let MenhirCell1_var_list (_menhir_stack, _, xlst) = _menhir_stack in
          let MenhirCell1_KW_FUN (_menhir_stack, _) = _menhir_stack in
          let MenhirCell1_LEFT_PARENTHESIS (_menhir_stack, _menhir_s) = _menhir_stack in
          let e = _v in
          let _v = _menhir_action_04 e xlst in
          _menhir_goto_expr _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | PLUS ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_16 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState38
      | NUMBER _v_1 ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          let _tok = _menhir_lexer _menhir_lexbuf in
          let n = _v_1 in
          let _v = _menhir_action_12 n in
          _menhir_run_18 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | NEG ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_03 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState38
      | MINUS ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_19 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState38
      | LESSTHAN ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_21 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState38
      | LEFT_PARENTHESIS ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_05 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState38
      | KW_LET ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_06 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState38
      | KW_IF ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_11 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState38
      | ID _v_3 ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          let _tok = _menhir_lexer _menhir_lexbuf in
          let x = _v_3 in
          let _v = _menhir_action_06 x in
          _menhir_run_18 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | FALSE ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          let _tok = _menhir_lexer _menhir_lexbuf in
          let _v = _menhir_action_08 () in
          _menhir_run_18 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | _ ->
          _eRR ()
  
  and _menhir_run_16 : type  ttv_stack. ((ttv_stack, _menhir_box_parse) _menhir_cell1_expr as 'stack) -> _ -> _ -> ('stack, _menhir_box_parse) _menhir_state -> _menhir_box_parse =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s ->
      let _menhir_stack = MenhirCell1_PLUS (_menhir_stack, _menhir_s) in
      let _tok = _menhir_lexer _menhir_lexbuf in
      match (_tok : MenhirBasics.token) with
      | TRUE ->
          let _tok = _menhir_lexer _menhir_lexbuf in
          let _v = _menhir_action_07 () in
          _menhir_run_17 _menhir_stack _menhir_lexbuf _menhir_lexer _v MenhirState16 _tok
      | NUMBER _v ->
          let _tok = _menhir_lexer _menhir_lexbuf in
          let n = _v in
          let _v = _menhir_action_12 n in
          _menhir_run_17 _menhir_stack _menhir_lexbuf _menhir_lexer _v MenhirState16 _tok
      | NEG ->
          _menhir_run_03 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState16
      | LEFT_PARENTHESIS ->
          _menhir_run_05 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState16
      | KW_LET ->
          _menhir_run_06 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState16
      | KW_IF ->
          _menhir_run_11 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState16
      | ID _v ->
          let _tok = _menhir_lexer _menhir_lexbuf in
          let x = _v in
          let _v = _menhir_action_06 x in
          _menhir_run_17 _menhir_stack _menhir_lexbuf _menhir_lexer _v MenhirState16 _tok
      | FALSE ->
          let _tok = _menhir_lexer _menhir_lexbuf in
          let _v = _menhir_action_08 () in
          _menhir_run_17 _menhir_stack _menhir_lexbuf _menhir_lexer _v MenhirState16 _tok
      | _ ->
          _eRR ()
  
  and _menhir_run_17 : type  ttv_stack. (((ttv_stack, _menhir_box_parse) _menhir_cell1_expr, _menhir_box_parse) _menhir_cell1_PLUS as 'stack) -> _ -> _ -> _ -> ('stack, _menhir_box_parse) _menhir_state -> _ -> _menhir_box_parse =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      match (_tok : MenhirBasics.token) with
      | TRUE ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          let _tok = _menhir_lexer _menhir_lexbuf in
          let _v = _menhir_action_07 () in
          _menhir_run_18 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | NUMBER _v_1 ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          let _tok = _menhir_lexer _menhir_lexbuf in
          let n = _v_1 in
          let _v = _menhir_action_12 n in
          _menhir_run_18 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | NEG ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_03 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState17
      | KW_IF ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_11 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState17
      | ID _v_3 ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          let _tok = _menhir_lexer _menhir_lexbuf in
          let x = _v_3 in
          let _v = _menhir_action_06 x in
          _menhir_run_18 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | FALSE ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          let _tok = _menhir_lexer _menhir_lexbuf in
          let _v = _menhir_action_08 () in
          _menhir_run_18 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | EOF | KW_ELSE | KW_IN | KW_LET | KW_THEN | LEFT_PARENTHESIS | LESSTHAN | MINUS | PLUS | RIGHT_PARENTHESIS ->
          let MenhirCell1_PLUS (_menhir_stack, _) = _menhir_stack in
          let MenhirCell1_expr (_menhir_stack, _menhir_s, e1) = _menhir_stack in
          let e2 = _v in
          let _v = _menhir_action_10 e1 e2 in
          _menhir_goto_expr _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | _ ->
          _eRR ()
  
  and _menhir_run_03 : type  ttv_stack. ttv_stack -> _ -> _ -> (ttv_stack, _menhir_box_parse) _menhir_state -> _menhir_box_parse =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s ->
      let _menhir_stack = MenhirCell1_NEG (_menhir_stack, _menhir_s) in
      let _tok = _menhir_lexer _menhir_lexbuf in
      match (_tok : MenhirBasics.token) with
      | LEFT_PARENTHESIS ->
          let _menhir_stack = MenhirCell1_LEFT_PARENTHESIS (_menhir_stack, MenhirState03) in
          let _tok = _menhir_lexer _menhir_lexbuf in
          (match (_tok : MenhirBasics.token) with
          | TRUE ->
              let _tok = _menhir_lexer _menhir_lexbuf in
              let _v = _menhir_action_07 () in
              _menhir_run_40 _menhir_stack _menhir_lexbuf _menhir_lexer _v MenhirState04 _tok
          | NUMBER _v ->
              let _tok = _menhir_lexer _menhir_lexbuf in
              let n = _v in
              let _v = _menhir_action_12 n in
              _menhir_run_40 _menhir_stack _menhir_lexbuf _menhir_lexer _v MenhirState04 _tok
          | NEG ->
              _menhir_run_03 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState04
          | LEFT_PARENTHESIS ->
              _menhir_run_05 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState04
          | KW_LET ->
              _menhir_run_06 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState04
          | KW_IF ->
              _menhir_run_11 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState04
          | ID _v ->
              let _tok = _menhir_lexer _menhir_lexbuf in
              let x = _v in
              let _v = _menhir_action_06 x in
              _menhir_run_40 _menhir_stack _menhir_lexbuf _menhir_lexer _v MenhirState04 _tok
          | FALSE ->
              let _tok = _menhir_lexer _menhir_lexbuf in
              let _v = _menhir_action_08 () in
              _menhir_run_40 _menhir_stack _menhir_lexbuf _menhir_lexer _v MenhirState04 _tok
          | _ ->
              _eRR ())
      | _ ->
          _eRR ()
  
  and _menhir_run_05 : type  ttv_stack. ttv_stack -> _ -> _ -> (ttv_stack, _menhir_box_parse) _menhir_state -> _menhir_box_parse =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s ->
      let _menhir_stack = MenhirCell1_LEFT_PARENTHESIS (_menhir_stack, _menhir_s) in
      let _tok = _menhir_lexer _menhir_lexbuf in
      match (_tok : MenhirBasics.token) with
      | TRUE ->
          let _tok = _menhir_lexer _menhir_lexbuf in
          let _v = _menhir_action_07 () in
          _menhir_run_40 _menhir_stack _menhir_lexbuf _menhir_lexer _v MenhirState05 _tok
      | NUMBER _v ->
          let _tok = _menhir_lexer _menhir_lexbuf in
          let n = _v in
          let _v = _menhir_action_12 n in
          _menhir_run_40 _menhir_stack _menhir_lexbuf _menhir_lexer _v MenhirState05 _tok
      | NEG ->
          _menhir_run_03 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState05
      | LEFT_PARENTHESIS ->
          _menhir_run_05 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState05
      | KW_LET ->
          _menhir_run_06 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState05
      | KW_IF ->
          _menhir_run_11 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState05
      | KW_FUN ->
          let _menhir_stack = MenhirCell1_KW_FUN (_menhir_stack, MenhirState05) in
          let _tok = _menhir_lexer _menhir_lexbuf in
          (match (_tok : MenhirBasics.token) with
          | ID _v ->
              _menhir_run_08 _menhir_stack _menhir_lexbuf _menhir_lexer _v MenhirState35
          | _ ->
              _eRR ())
      | ID _v ->
          let _tok = _menhir_lexer _menhir_lexbuf in
          let x = _v in
          let _v = _menhir_action_06 x in
          _menhir_run_40 _menhir_stack _menhir_lexbuf _menhir_lexer _v MenhirState05 _tok
      | FALSE ->
          let _tok = _menhir_lexer _menhir_lexbuf in
          let _v = _menhir_action_08 () in
          _menhir_run_40 _menhir_stack _menhir_lexbuf _menhir_lexer _v MenhirState05 _tok
      | _ ->
          _eRR ()
  
  and _menhir_run_06 : type  ttv_stack. ttv_stack -> _ -> _ -> (ttv_stack, _menhir_box_parse) _menhir_state -> _menhir_box_parse =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s ->
      let _menhir_stack = MenhirCell1_KW_LET (_menhir_stack, _menhir_s) in
      let _tok = _menhir_lexer _menhir_lexbuf in
      match (_tok : MenhirBasics.token) with
      | ID _v ->
          let _menhir_stack = MenhirCell0_ID (_menhir_stack, _v) in
          let _tok = _menhir_lexer _menhir_lexbuf in
          (match (_tok : MenhirBasics.token) with
          | ID _v_0 ->
              _menhir_run_08 _menhir_stack _menhir_lexbuf _menhir_lexer _v_0 MenhirState07
          | EQ ->
              let _menhir_stack = MenhirCell1_EQ (_menhir_stack, MenhirState07) in
              let _tok = _menhir_lexer _menhir_lexbuf in
              (match (_tok : MenhirBasics.token) with
              | TRUE ->
                  let _tok = _menhir_lexer _menhir_lexbuf in
                  let _v = _menhir_action_07 () in
                  _menhir_run_27 _menhir_stack _menhir_lexbuf _menhir_lexer _v MenhirState10 _tok
              | NUMBER _v_2 ->
                  let _tok = _menhir_lexer _menhir_lexbuf in
                  let n = _v_2 in
                  let _v = _menhir_action_12 n in
                  _menhir_run_27 _menhir_stack _menhir_lexbuf _menhir_lexer _v MenhirState10 _tok
              | NEG ->
                  _menhir_run_03 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState10
              | LEFT_PARENTHESIS ->
                  _menhir_run_05 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState10
              | KW_LET ->
                  _menhir_run_06 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState10
              | KW_IF ->
                  _menhir_run_11 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState10
              | ID _v_4 ->
                  let _tok = _menhir_lexer _menhir_lexbuf in
                  let x = _v_4 in
                  let _v = _menhir_action_06 x in
                  _menhir_run_27 _menhir_stack _menhir_lexbuf _menhir_lexer _v MenhirState10 _tok
              | FALSE ->
                  let _tok = _menhir_lexer _menhir_lexbuf in
                  let _v = _menhir_action_08 () in
                  _menhir_run_27 _menhir_stack _menhir_lexbuf _menhir_lexer _v MenhirState10 _tok
              | _ ->
                  _eRR ())
          | _ ->
              _eRR ())
      | _ ->
          _eRR ()
  
  and _menhir_run_08 : type  ttv_stack. ttv_stack -> _ -> _ -> _ -> (ttv_stack, _menhir_box_parse) _menhir_state -> _menhir_box_parse =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s ->
      let _tok = _menhir_lexer _menhir_lexbuf in
      match (_tok : MenhirBasics.token) with
      | ID _v_0 ->
          let _menhir_stack = MenhirCell1_ID (_menhir_stack, _menhir_s, _v) in
          _menhir_run_08 _menhir_stack _menhir_lexbuf _menhir_lexer _v_0 MenhirState08
      | ARROW | EQ ->
          let x = _v in
          let _v = _menhir_action_17 x in
          _menhir_goto_var_list _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | _ ->
          _eRR ()
  
  and _menhir_goto_var_list : type  ttv_stack. ttv_stack -> _ -> _ -> _ -> (ttv_stack, _menhir_box_parse) _menhir_state -> _ -> _menhir_box_parse =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      match _menhir_s with
      | MenhirState35 ->
          _menhir_run_36 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState07 ->
          _menhir_run_30 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState08 ->
          _menhir_run_09 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | _ ->
          _menhir_fail ()
  
  and _menhir_run_36 : type  ttv_stack. (((ttv_stack, _menhir_box_parse) _menhir_cell1_LEFT_PARENTHESIS, _menhir_box_parse) _menhir_cell1_KW_FUN as 'stack) -> _ -> _ -> _ -> ('stack, _menhir_box_parse) _menhir_state -> _ -> _menhir_box_parse =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      let _menhir_stack = MenhirCell1_var_list (_menhir_stack, _menhir_s, _v) in
      match (_tok : MenhirBasics.token) with
      | ARROW ->
          let _tok = _menhir_lexer _menhir_lexbuf in
          (match (_tok : MenhirBasics.token) with
          | TRUE ->
              let _tok = _menhir_lexer _menhir_lexbuf in
              let _v = _menhir_action_07 () in
              _menhir_run_38 _menhir_stack _menhir_lexbuf _menhir_lexer _v MenhirState37 _tok
          | NUMBER _v_1 ->
              let _tok = _menhir_lexer _menhir_lexbuf in
              let n = _v_1 in
              let _v = _menhir_action_12 n in
              _menhir_run_38 _menhir_stack _menhir_lexbuf _menhir_lexer _v MenhirState37 _tok
          | NEG ->
              _menhir_run_03 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState37
          | LEFT_PARENTHESIS ->
              _menhir_run_05 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState37
          | KW_LET ->
              _menhir_run_06 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState37
          | KW_IF ->
              _menhir_run_11 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState37
          | ID _v_3 ->
              let _tok = _menhir_lexer _menhir_lexbuf in
              let x = _v_3 in
              let _v = _menhir_action_06 x in
              _menhir_run_38 _menhir_stack _menhir_lexbuf _menhir_lexer _v MenhirState37 _tok
          | FALSE ->
              let _tok = _menhir_lexer _menhir_lexbuf in
              let _v = _menhir_action_08 () in
              _menhir_run_38 _menhir_stack _menhir_lexbuf _menhir_lexer _v MenhirState37 _tok
          | _ ->
              _eRR ())
      | _ ->
          _eRR ()
  
  and _menhir_run_11 : type  ttv_stack. ttv_stack -> _ -> _ -> (ttv_stack, _menhir_box_parse) _menhir_state -> _menhir_box_parse =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s ->
      let _menhir_stack = MenhirCell1_KW_IF (_menhir_stack, _menhir_s) in
      let _tok = _menhir_lexer _menhir_lexbuf in
      match (_tok : MenhirBasics.token) with
      | TRUE ->
          let _tok = _menhir_lexer _menhir_lexbuf in
          let _v = _menhir_action_07 () in
          _menhir_run_15 _menhir_stack _menhir_lexbuf _menhir_lexer _v MenhirState11 _tok
      | NUMBER _v ->
          let _tok = _menhir_lexer _menhir_lexbuf in
          let n = _v in
          let _v = _menhir_action_12 n in
          _menhir_run_15 _menhir_stack _menhir_lexbuf _menhir_lexer _v MenhirState11 _tok
      | NEG ->
          _menhir_run_03 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState11
      | LEFT_PARENTHESIS ->
          _menhir_run_05 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState11
      | KW_LET ->
          _menhir_run_06 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState11
      | KW_IF ->
          _menhir_run_11 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState11
      | ID _v ->
          let _tok = _menhir_lexer _menhir_lexbuf in
          let x = _v in
          let _v = _menhir_action_06 x in
          _menhir_run_15 _menhir_stack _menhir_lexbuf _menhir_lexer _v MenhirState11 _tok
      | FALSE ->
          let _tok = _menhir_lexer _menhir_lexbuf in
          let _v = _menhir_action_08 () in
          _menhir_run_15 _menhir_stack _menhir_lexbuf _menhir_lexer _v MenhirState11 _tok
      | _ ->
          _eRR ()
  
  and _menhir_run_15 : type  ttv_stack. ((ttv_stack, _menhir_box_parse) _menhir_cell1_KW_IF as 'stack) -> _ -> _ -> _ -> ('stack, _menhir_box_parse) _menhir_state -> _ -> _menhir_box_parse =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
      match (_tok : MenhirBasics.token) with
      | TRUE ->
          let _tok = _menhir_lexer _menhir_lexbuf in
          let _v = _menhir_action_07 () in
          _menhir_run_18 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | PLUS ->
          _menhir_run_16 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState15
      | NUMBER _v_1 ->
          let _tok = _menhir_lexer _menhir_lexbuf in
          let n = _v_1 in
          let _v = _menhir_action_12 n in
          _menhir_run_18 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | NEG ->
          _menhir_run_03 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState15
      | MINUS ->
          _menhir_run_19 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState15
      | LESSTHAN ->
          _menhir_run_21 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState15
      | LEFT_PARENTHESIS ->
          _menhir_run_05 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState15
      | KW_THEN ->
          let _menhir_stack = MenhirCell1_KW_THEN (_menhir_stack, MenhirState15) in
          let _tok = _menhir_lexer _menhir_lexbuf in
          (match (_tok : MenhirBasics.token) with
          | TRUE ->
              let _tok = _menhir_lexer _menhir_lexbuf in
              let _v = _menhir_action_07 () in
              _menhir_run_24 _menhir_stack _menhir_lexbuf _menhir_lexer _v MenhirState23 _tok
          | NUMBER _v_4 ->
              let _tok = _menhir_lexer _menhir_lexbuf in
              let n = _v_4 in
              let _v = _menhir_action_12 n in
              _menhir_run_24 _menhir_stack _menhir_lexbuf _menhir_lexer _v MenhirState23 _tok
          | NEG ->
              _menhir_run_03 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState23
          | LEFT_PARENTHESIS ->
              _menhir_run_05 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState23
          | KW_LET ->
              _menhir_run_06 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState23
          | KW_IF ->
              _menhir_run_11 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState23
          | ID _v_6 ->
              let _tok = _menhir_lexer _menhir_lexbuf in
              let x = _v_6 in
              let _v = _menhir_action_06 x in
              _menhir_run_24 _menhir_stack _menhir_lexbuf _menhir_lexer _v MenhirState23 _tok
          | FALSE ->
              let _tok = _menhir_lexer _menhir_lexbuf in
              let _v = _menhir_action_08 () in
              _menhir_run_24 _menhir_stack _menhir_lexbuf _menhir_lexer _v MenhirState23 _tok
          | _ ->
              _eRR ())
      | KW_LET ->
          _menhir_run_06 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState15
      | KW_IF ->
          _menhir_run_11 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState15
      | ID _v_9 ->
          let _tok = _menhir_lexer _menhir_lexbuf in
          let x = _v_9 in
          let _v = _menhir_action_06 x in
          _menhir_run_18 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | FALSE ->
          let _tok = _menhir_lexer _menhir_lexbuf in
          let _v = _menhir_action_08 () in
          _menhir_run_18 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | _ ->
          _eRR ()
  
  and _menhir_run_19 : type  ttv_stack. ((ttv_stack, _menhir_box_parse) _menhir_cell1_expr as 'stack) -> _ -> _ -> ('stack, _menhir_box_parse) _menhir_state -> _menhir_box_parse =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s ->
      let _menhir_stack = MenhirCell1_MINUS (_menhir_stack, _menhir_s) in
      let _tok = _menhir_lexer _menhir_lexbuf in
      match (_tok : MenhirBasics.token) with
      | TRUE ->
          let _tok = _menhir_lexer _menhir_lexbuf in
          let _v = _menhir_action_07 () in
          _menhir_run_20 _menhir_stack _menhir_lexbuf _menhir_lexer _v MenhirState19 _tok
      | NUMBER _v ->
          let _tok = _menhir_lexer _menhir_lexbuf in
          let n = _v in
          let _v = _menhir_action_12 n in
          _menhir_run_20 _menhir_stack _menhir_lexbuf _menhir_lexer _v MenhirState19 _tok
      | NEG ->
          _menhir_run_03 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState19
      | LEFT_PARENTHESIS ->
          _menhir_run_05 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState19
      | KW_LET ->
          _menhir_run_06 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState19
      | KW_IF ->
          _menhir_run_11 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState19
      | ID _v ->
          let _tok = _menhir_lexer _menhir_lexbuf in
          let x = _v in
          let _v = _menhir_action_06 x in
          _menhir_run_20 _menhir_stack _menhir_lexbuf _menhir_lexer _v MenhirState19 _tok
      | FALSE ->
          let _tok = _menhir_lexer _menhir_lexbuf in
          let _v = _menhir_action_08 () in
          _menhir_run_20 _menhir_stack _menhir_lexbuf _menhir_lexer _v MenhirState19 _tok
      | _ ->
          _eRR ()
  
  and _menhir_run_20 : type  ttv_stack. (((ttv_stack, _menhir_box_parse) _menhir_cell1_expr, _menhir_box_parse) _menhir_cell1_MINUS as 'stack) -> _ -> _ -> _ -> ('stack, _menhir_box_parse) _menhir_state -> _ -> _menhir_box_parse =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      match (_tok : MenhirBasics.token) with
      | TRUE ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          let _tok = _menhir_lexer _menhir_lexbuf in
          let _v = _menhir_action_07 () in
          _menhir_run_18 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | NUMBER _v_1 ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          let _tok = _menhir_lexer _menhir_lexbuf in
          let n = _v_1 in
          let _v = _menhir_action_12 n in
          _menhir_run_18 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | NEG ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_03 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState20
      | KW_IF ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_11 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState20
      | ID _v_3 ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          let _tok = _menhir_lexer _menhir_lexbuf in
          let x = _v_3 in
          let _v = _menhir_action_06 x in
          _menhir_run_18 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | FALSE ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          let _tok = _menhir_lexer _menhir_lexbuf in
          let _v = _menhir_action_08 () in
          _menhir_run_18 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | EOF | KW_ELSE | KW_IN | KW_LET | KW_THEN | LEFT_PARENTHESIS | LESSTHAN | MINUS | PLUS | RIGHT_PARENTHESIS ->
          let MenhirCell1_MINUS (_menhir_stack, _) = _menhir_stack in
          let MenhirCell1_expr (_menhir_stack, _menhir_s, e1) = _menhir_stack in
          let e2 = _v in
          let _v = _menhir_action_11 e1 e2 in
          _menhir_goto_expr _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | _ ->
          _eRR ()
  
  and _menhir_run_21 : type  ttv_stack. ((ttv_stack, _menhir_box_parse) _menhir_cell1_expr as 'stack) -> _ -> _ -> ('stack, _menhir_box_parse) _menhir_state -> _menhir_box_parse =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s ->
      let _menhir_stack = MenhirCell1_LESSTHAN (_menhir_stack, _menhir_s) in
      let _tok = _menhir_lexer _menhir_lexbuf in
      match (_tok : MenhirBasics.token) with
      | TRUE ->
          let _tok = _menhir_lexer _menhir_lexbuf in
          let _v = _menhir_action_07 () in
          _menhir_run_22 _menhir_stack _menhir_lexbuf _menhir_lexer _v MenhirState21 _tok
      | NUMBER _v ->
          let _tok = _menhir_lexer _menhir_lexbuf in
          let n = _v in
          let _v = _menhir_action_12 n in
          _menhir_run_22 _menhir_stack _menhir_lexbuf _menhir_lexer _v MenhirState21 _tok
      | NEG ->
          _menhir_run_03 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState21
      | LEFT_PARENTHESIS ->
          _menhir_run_05 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState21
      | KW_LET ->
          _menhir_run_06 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState21
      | KW_IF ->
          _menhir_run_11 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState21
      | ID _v ->
          let _tok = _menhir_lexer _menhir_lexbuf in
          let x = _v in
          let _v = _menhir_action_06 x in
          _menhir_run_22 _menhir_stack _menhir_lexbuf _menhir_lexer _v MenhirState21 _tok
      | FALSE ->
          let _tok = _menhir_lexer _menhir_lexbuf in
          let _v = _menhir_action_08 () in
          _menhir_run_22 _menhir_stack _menhir_lexbuf _menhir_lexer _v MenhirState21 _tok
      | _ ->
          _eRR ()
  
  and _menhir_run_22 : type  ttv_stack. (((ttv_stack, _menhir_box_parse) _menhir_cell1_expr, _menhir_box_parse) _menhir_cell1_LESSTHAN as 'stack) -> _ -> _ -> _ -> ('stack, _menhir_box_parse) _menhir_state -> _ -> _menhir_box_parse =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      match (_tok : MenhirBasics.token) with
      | TRUE ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          let _tok = _menhir_lexer _menhir_lexbuf in
          let _v = _menhir_action_07 () in
          _menhir_run_18 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | PLUS ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_16 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState22
      | NUMBER _v_1 ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          let _tok = _menhir_lexer _menhir_lexbuf in
          let n = _v_1 in
          let _v = _menhir_action_12 n in
          _menhir_run_18 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | NEG ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_03 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState22
      | MINUS ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_19 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState22
      | KW_IF ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_11 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState22
      | ID _v_3 ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          let _tok = _menhir_lexer _menhir_lexbuf in
          let x = _v_3 in
          let _v = _menhir_action_06 x in
          _menhir_run_18 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | FALSE ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          let _tok = _menhir_lexer _menhir_lexbuf in
          let _v = _menhir_action_08 () in
          _menhir_run_18 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | EOF | KW_ELSE | KW_IN | KW_LET | KW_THEN | LEFT_PARENTHESIS | LESSTHAN | RIGHT_PARENTHESIS ->
          let MenhirCell1_LESSTHAN (_menhir_stack, _) = _menhir_stack in
          let MenhirCell1_expr (_menhir_stack, _menhir_s, e1) = _menhir_stack in
          let e2 = _v in
          let _v = _menhir_action_09 e1 e2 in
          _menhir_goto_expr _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | _ ->
          _eRR ()
  
  and _menhir_run_24 : type  ttv_stack. ((((ttv_stack, _menhir_box_parse) _menhir_cell1_KW_IF, _menhir_box_parse) _menhir_cell1_expr, _menhir_box_parse) _menhir_cell1_KW_THEN as 'stack) -> _ -> _ -> _ -> ('stack, _menhir_box_parse) _menhir_state -> _ -> _menhir_box_parse =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
      match (_tok : MenhirBasics.token) with
      | TRUE ->
          let _tok = _menhir_lexer _menhir_lexbuf in
          let _v = _menhir_action_07 () in
          _menhir_run_18 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | PLUS ->
          _menhir_run_16 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState24
      | NUMBER _v_1 ->
          let _tok = _menhir_lexer _menhir_lexbuf in
          let n = _v_1 in
          let _v = _menhir_action_12 n in
          _menhir_run_18 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | NEG ->
          _menhir_run_03 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState24
      | MINUS ->
          _menhir_run_19 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState24
      | LESSTHAN ->
          _menhir_run_21 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState24
      | LEFT_PARENTHESIS ->
          _menhir_run_05 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState24
      | KW_LET ->
          _menhir_run_06 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState24
      | KW_IF ->
          _menhir_run_11 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState24
      | KW_ELSE ->
          let _menhir_stack = MenhirCell1_KW_ELSE (_menhir_stack, MenhirState24) in
          let _tok = _menhir_lexer _menhir_lexbuf in
          (match (_tok : MenhirBasics.token) with
          | TRUE ->
              let _tok = _menhir_lexer _menhir_lexbuf in
              let _v = _menhir_action_07 () in
              _menhir_run_26 _menhir_stack _menhir_lexbuf _menhir_lexer _v MenhirState25 _tok
          | NUMBER _v_4 ->
              let _tok = _menhir_lexer _menhir_lexbuf in
              let n = _v_4 in
              let _v = _menhir_action_12 n in
              _menhir_run_26 _menhir_stack _menhir_lexbuf _menhir_lexer _v MenhirState25 _tok
          | NEG ->
              _menhir_run_03 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState25
          | LEFT_PARENTHESIS ->
              _menhir_run_05 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState25
          | KW_LET ->
              _menhir_run_06 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState25
          | KW_IF ->
              _menhir_run_11 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState25
          | ID _v_6 ->
              let _tok = _menhir_lexer _menhir_lexbuf in
              let x = _v_6 in
              let _v = _menhir_action_06 x in
              _menhir_run_26 _menhir_stack _menhir_lexbuf _menhir_lexer _v MenhirState25 _tok
          | FALSE ->
              let _tok = _menhir_lexer _menhir_lexbuf in
              let _v = _menhir_action_08 () in
              _menhir_run_26 _menhir_stack _menhir_lexbuf _menhir_lexer _v MenhirState25 _tok
          | _ ->
              _eRR ())
      | ID _v_9 ->
          let _tok = _menhir_lexer _menhir_lexbuf in
          let x = _v_9 in
          let _v = _menhir_action_06 x in
          _menhir_run_18 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | FALSE ->
          let _tok = _menhir_lexer _menhir_lexbuf in
          let _v = _menhir_action_08 () in
          _menhir_run_18 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | _ ->
          _eRR ()
  
  and _menhir_run_26 : type  ttv_stack. ((((((ttv_stack, _menhir_box_parse) _menhir_cell1_KW_IF, _menhir_box_parse) _menhir_cell1_expr, _menhir_box_parse) _menhir_cell1_KW_THEN, _menhir_box_parse) _menhir_cell1_expr, _menhir_box_parse) _menhir_cell1_KW_ELSE as 'stack) -> _ -> _ -> _ -> ('stack, _menhir_box_parse) _menhir_state -> _ -> _menhir_box_parse =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      match (_tok : MenhirBasics.token) with
      | TRUE ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          let _tok = _menhir_lexer _menhir_lexbuf in
          let _v = _menhir_action_07 () in
          _menhir_run_18 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | PLUS ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_16 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState26
      | NUMBER _v_1 ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          let _tok = _menhir_lexer _menhir_lexbuf in
          let n = _v_1 in
          let _v = _menhir_action_12 n in
          _menhir_run_18 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | NEG ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_03 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState26
      | MINUS ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_19 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState26
      | LESSTHAN ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_21 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState26
      | LEFT_PARENTHESIS ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_05 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState26
      | KW_LET ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_06 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState26
      | KW_IF ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_11 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState26
      | ID _v_3 ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          let _tok = _menhir_lexer _menhir_lexbuf in
          let x = _v_3 in
          let _v = _menhir_action_06 x in
          _menhir_run_18 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | FALSE ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          let _tok = _menhir_lexer _menhir_lexbuf in
          let _v = _menhir_action_08 () in
          _menhir_run_18 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | EOF | KW_ELSE | KW_IN | KW_THEN | RIGHT_PARENTHESIS ->
          let MenhirCell1_KW_ELSE (_menhir_stack, _) = _menhir_stack in
          let MenhirCell1_expr (_menhir_stack, _, e2) = _menhir_stack in
          let MenhirCell1_KW_THEN (_menhir_stack, _) = _menhir_stack in
          let MenhirCell1_expr (_menhir_stack, _, e1) = _menhir_stack in
          let MenhirCell1_KW_IF (_menhir_stack, _menhir_s) = _menhir_stack in
          let e3 = _v in
          let _v = _menhir_action_05 e1 e2 e3 in
          _menhir_goto_expr _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | _ ->
          _eRR ()
  
  and _menhir_run_30 : type  ttv_stack. ((ttv_stack, _menhir_box_parse) _menhir_cell1_KW_LET _menhir_cell0_ID as 'stack) -> _ -> _ -> _ -> ('stack, _menhir_box_parse) _menhir_state -> _ -> _menhir_box_parse =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      let _menhir_stack = MenhirCell1_var_list (_menhir_stack, _menhir_s, _v) in
      match (_tok : MenhirBasics.token) with
      | EQ ->
          let _tok = _menhir_lexer _menhir_lexbuf in
          (match (_tok : MenhirBasics.token) with
          | TRUE ->
              let _tok = _menhir_lexer _menhir_lexbuf in
              let _v = _menhir_action_07 () in
              _menhir_run_32 _menhir_stack _menhir_lexbuf _menhir_lexer _v MenhirState31 _tok
          | NUMBER _v_1 ->
              let _tok = _menhir_lexer _menhir_lexbuf in
              let n = _v_1 in
              let _v = _menhir_action_12 n in
              _menhir_run_32 _menhir_stack _menhir_lexbuf _menhir_lexer _v MenhirState31 _tok
          | NEG ->
              _menhir_run_03 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState31
          | LEFT_PARENTHESIS ->
              _menhir_run_05 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState31
          | KW_LET ->
              _menhir_run_06 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState31
          | KW_IF ->
              _menhir_run_11 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState31
          | ID _v_3 ->
              let _tok = _menhir_lexer _menhir_lexbuf in
              let x = _v_3 in
              let _v = _menhir_action_06 x in
              _menhir_run_32 _menhir_stack _menhir_lexbuf _menhir_lexer _v MenhirState31 _tok
          | FALSE ->
              let _tok = _menhir_lexer _menhir_lexbuf in
              let _v = _menhir_action_08 () in
              _menhir_run_32 _menhir_stack _menhir_lexbuf _menhir_lexer _v MenhirState31 _tok
          | _ ->
              _eRR ())
      | _ ->
          _eRR ()
  
  and _menhir_run_32 : type  ttv_stack. (((ttv_stack, _menhir_box_parse) _menhir_cell1_KW_LET _menhir_cell0_ID, _menhir_box_parse) _menhir_cell1_var_list as 'stack) -> _ -> _ -> _ -> ('stack, _menhir_box_parse) _menhir_state -> _ -> _menhir_box_parse =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
      match (_tok : MenhirBasics.token) with
      | TRUE ->
          let _tok = _menhir_lexer _menhir_lexbuf in
          let _v = _menhir_action_07 () in
          _menhir_run_18 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | PLUS ->
          _menhir_run_16 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState32
      | NUMBER _v_1 ->
          let _tok = _menhir_lexer _menhir_lexbuf in
          let n = _v_1 in
          let _v = _menhir_action_12 n in
          _menhir_run_18 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | NEG ->
          _menhir_run_03 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState32
      | MINUS ->
          _menhir_run_19 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState32
      | LESSTHAN ->
          _menhir_run_21 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState32
      | LEFT_PARENTHESIS ->
          _menhir_run_05 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState32
      | KW_LET ->
          _menhir_run_06 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState32
      | KW_IN ->
          let _menhir_stack = MenhirCell1_KW_IN (_menhir_stack, MenhirState32) in
          let _tok = _menhir_lexer _menhir_lexbuf in
          (match (_tok : MenhirBasics.token) with
          | TRUE ->
              let _tok = _menhir_lexer _menhir_lexbuf in
              let _v = _menhir_action_07 () in
              _menhir_run_34 _menhir_stack _menhir_lexbuf _menhir_lexer _v MenhirState33 _tok
          | NUMBER _v_4 ->
              let _tok = _menhir_lexer _menhir_lexbuf in
              let n = _v_4 in
              let _v = _menhir_action_12 n in
              _menhir_run_34 _menhir_stack _menhir_lexbuf _menhir_lexer _v MenhirState33 _tok
          | NEG ->
              _menhir_run_03 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState33
          | LEFT_PARENTHESIS ->
              _menhir_run_05 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState33
          | KW_LET ->
              _menhir_run_06 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState33
          | KW_IF ->
              _menhir_run_11 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState33
          | ID _v_6 ->
              let _tok = _menhir_lexer _menhir_lexbuf in
              let x = _v_6 in
              let _v = _menhir_action_06 x in
              _menhir_run_34 _menhir_stack _menhir_lexbuf _menhir_lexer _v MenhirState33 _tok
          | FALSE ->
              let _tok = _menhir_lexer _menhir_lexbuf in
              let _v = _menhir_action_08 () in
              _menhir_run_34 _menhir_stack _menhir_lexbuf _menhir_lexer _v MenhirState33 _tok
          | _ ->
              _eRR ())
      | KW_IF ->
          _menhir_run_11 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState32
      | ID _v_9 ->
          let _tok = _menhir_lexer _menhir_lexbuf in
          let x = _v_9 in
          let _v = _menhir_action_06 x in
          _menhir_run_18 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | FALSE ->
          let _tok = _menhir_lexer _menhir_lexbuf in
          let _v = _menhir_action_08 () in
          _menhir_run_18 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | _ ->
          _eRR ()
  
  and _menhir_run_34 : type  ttv_stack. (((((ttv_stack, _menhir_box_parse) _menhir_cell1_KW_LET _menhir_cell0_ID, _menhir_box_parse) _menhir_cell1_var_list, _menhir_box_parse) _menhir_cell1_expr, _menhir_box_parse) _menhir_cell1_KW_IN as 'stack) -> _ -> _ -> _ -> ('stack, _menhir_box_parse) _menhir_state -> _ -> _menhir_box_parse =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      match (_tok : MenhirBasics.token) with
      | TRUE ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          let _tok = _menhir_lexer _menhir_lexbuf in
          let _v = _menhir_action_07 () in
          _menhir_run_18 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | PLUS ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_16 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState34
      | NUMBER _v_1 ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          let _tok = _menhir_lexer _menhir_lexbuf in
          let n = _v_1 in
          let _v = _menhir_action_12 n in
          _menhir_run_18 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | NEG ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_03 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState34
      | MINUS ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_19 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState34
      | LESSTHAN ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_21 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState34
      | LEFT_PARENTHESIS ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_05 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState34
      | KW_LET ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_06 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState34
      | KW_IF ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_11 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState34
      | ID _v_3 ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          let _tok = _menhir_lexer _menhir_lexbuf in
          let x = _v_3 in
          let _v = _menhir_action_06 x in
          _menhir_run_18 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | FALSE ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          let _tok = _menhir_lexer _menhir_lexbuf in
          let _v = _menhir_action_08 () in
          _menhir_run_18 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | EOF | KW_ELSE | KW_IN | KW_THEN | RIGHT_PARENTHESIS ->
          let MenhirCell1_KW_IN (_menhir_stack, _) = _menhir_stack in
          let MenhirCell1_expr (_menhir_stack, _, e1) = _menhir_stack in
          let MenhirCell1_var_list (_menhir_stack, _, xlst) = _menhir_stack in
          let MenhirCell0_ID (_menhir_stack, x) = _menhir_stack in
          let MenhirCell1_KW_LET (_menhir_stack, _menhir_s) = _menhir_stack in
          let e2 = _v in
          let _v = _menhir_action_03 e1 e2 x xlst in
          _menhir_goto_expr _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | _ ->
          _eRR ()
  
  and _menhir_run_09 : type  ttv_stack. (ttv_stack, _menhir_box_parse) _menhir_cell1_ID -> _ -> _ -> _ -> _ -> _menhir_box_parse =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok ->
      let MenhirCell1_ID (_menhir_stack, _menhir_s, x) = _menhir_stack in
      let xlst = _v in
      let _v = _menhir_action_18 x xlst in
      _menhir_goto_var_list _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
  
  and _menhir_run_27 : type  ttv_stack. (((ttv_stack, _menhir_box_parse) _menhir_cell1_KW_LET _menhir_cell0_ID, _menhir_box_parse) _menhir_cell1_EQ as 'stack) -> _ -> _ -> _ -> ('stack, _menhir_box_parse) _menhir_state -> _ -> _menhir_box_parse =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
      match (_tok : MenhirBasics.token) with
      | TRUE ->
          let _tok = _menhir_lexer _menhir_lexbuf in
          let _v = _menhir_action_07 () in
          _menhir_run_18 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | PLUS ->
          _menhir_run_16 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState27
      | NUMBER _v_1 ->
          let _tok = _menhir_lexer _menhir_lexbuf in
          let n = _v_1 in
          let _v = _menhir_action_12 n in
          _menhir_run_18 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | NEG ->
          _menhir_run_03 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState27
      | MINUS ->
          _menhir_run_19 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState27
      | LESSTHAN ->
          _menhir_run_21 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState27
      | LEFT_PARENTHESIS ->
          _menhir_run_05 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState27
      | KW_LET ->
          _menhir_run_06 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState27
      | KW_IN ->
          let _menhir_stack = MenhirCell1_KW_IN (_menhir_stack, MenhirState27) in
          let _tok = _menhir_lexer _menhir_lexbuf in
          (match (_tok : MenhirBasics.token) with
          | TRUE ->
              let _tok = _menhir_lexer _menhir_lexbuf in
              let _v = _menhir_action_07 () in
              _menhir_run_29 _menhir_stack _menhir_lexbuf _menhir_lexer _v MenhirState28 _tok
          | NUMBER _v_4 ->
              let _tok = _menhir_lexer _menhir_lexbuf in
              let n = _v_4 in
              let _v = _menhir_action_12 n in
              _menhir_run_29 _menhir_stack _menhir_lexbuf _menhir_lexer _v MenhirState28 _tok
          | NEG ->
              _menhir_run_03 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState28
          | LEFT_PARENTHESIS ->
              _menhir_run_05 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState28
          | KW_LET ->
              _menhir_run_06 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState28
          | KW_IF ->
              _menhir_run_11 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState28
          | ID _v_6 ->
              let _tok = _menhir_lexer _menhir_lexbuf in
              let x = _v_6 in
              let _v = _menhir_action_06 x in
              _menhir_run_29 _menhir_stack _menhir_lexbuf _menhir_lexer _v MenhirState28 _tok
          | FALSE ->
              let _tok = _menhir_lexer _menhir_lexbuf in
              let _v = _menhir_action_08 () in
              _menhir_run_29 _menhir_stack _menhir_lexbuf _menhir_lexer _v MenhirState28 _tok
          | _ ->
              _eRR ())
      | KW_IF ->
          _menhir_run_11 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState27
      | ID _v_9 ->
          let _tok = _menhir_lexer _menhir_lexbuf in
          let x = _v_9 in
          let _v = _menhir_action_06 x in
          _menhir_run_18 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | FALSE ->
          let _tok = _menhir_lexer _menhir_lexbuf in
          let _v = _menhir_action_08 () in
          _menhir_run_18 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | _ ->
          _eRR ()
  
  and _menhir_run_29 : type  ttv_stack. (((((ttv_stack, _menhir_box_parse) _menhir_cell1_KW_LET _menhir_cell0_ID, _menhir_box_parse) _menhir_cell1_EQ, _menhir_box_parse) _menhir_cell1_expr, _menhir_box_parse) _menhir_cell1_KW_IN as 'stack) -> _ -> _ -> _ -> ('stack, _menhir_box_parse) _menhir_state -> _ -> _menhir_box_parse =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      match (_tok : MenhirBasics.token) with
      | TRUE ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          let _tok = _menhir_lexer _menhir_lexbuf in
          let _v = _menhir_action_07 () in
          _menhir_run_18 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | PLUS ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_16 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState29
      | NUMBER _v_1 ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          let _tok = _menhir_lexer _menhir_lexbuf in
          let n = _v_1 in
          let _v = _menhir_action_12 n in
          _menhir_run_18 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | NEG ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_03 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState29
      | MINUS ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_19 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState29
      | LESSTHAN ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_21 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState29
      | LEFT_PARENTHESIS ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_05 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState29
      | KW_LET ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_06 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState29
      | KW_IF ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_11 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState29
      | ID _v_3 ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          let _tok = _menhir_lexer _menhir_lexbuf in
          let x = _v_3 in
          let _v = _menhir_action_06 x in
          _menhir_run_18 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | FALSE ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          let _tok = _menhir_lexer _menhir_lexbuf in
          let _v = _menhir_action_08 () in
          _menhir_run_18 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | EOF | KW_ELSE | KW_IN | KW_THEN | RIGHT_PARENTHESIS ->
          let MenhirCell1_KW_IN (_menhir_stack, _) = _menhir_stack in
          let MenhirCell1_expr (_menhir_stack, _, e1) = _menhir_stack in
          let MenhirCell1_EQ (_menhir_stack, _) = _menhir_stack in
          let MenhirCell0_ID (_menhir_stack, x) = _menhir_stack in
          let MenhirCell1_KW_LET (_menhir_stack, _menhir_s) = _menhir_stack in
          let e2 = _v in
          let _v = _menhir_action_01 e1 e2 x in
          _menhir_goto_expr _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | _ ->
          _eRR ()
  
  and _menhir_run_14_spec_32 : type  ttv_stack. (((ttv_stack, _menhir_box_parse) _menhir_cell1_KW_LET _menhir_cell0_ID, _menhir_box_parse) _menhir_cell1_var_list, _menhir_box_parse) _menhir_cell1_expr -> _ -> _ -> _ -> _ -> _menhir_box_parse =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok ->
      let e = _v in
      let _v = _menhir_action_14 e in
      _menhir_run_18 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
  
  and _menhir_run_14_spec_34 : type  ttv_stack. (((((ttv_stack, _menhir_box_parse) _menhir_cell1_KW_LET _menhir_cell0_ID, _menhir_box_parse) _menhir_cell1_var_list, _menhir_box_parse) _menhir_cell1_expr, _menhir_box_parse) _menhir_cell1_KW_IN, _menhir_box_parse) _menhir_cell1_expr -> _ -> _ -> _ -> _ -> _menhir_box_parse =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok ->
      let e = _v in
      let _v = _menhir_action_14 e in
      _menhir_run_18 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
  
  and _menhir_run_14_spec_33 : type  ttv_stack. ((((ttv_stack, _menhir_box_parse) _menhir_cell1_KW_LET _menhir_cell0_ID, _menhir_box_parse) _menhir_cell1_var_list, _menhir_box_parse) _menhir_cell1_expr, _menhir_box_parse) _menhir_cell1_KW_IN -> _ -> _ -> _ -> _ -> _menhir_box_parse =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok ->
      let e = _v in
      let _v = _menhir_action_14 e in
      _menhir_run_34 _menhir_stack _menhir_lexbuf _menhir_lexer _v MenhirState33 _tok
  
  and _menhir_run_14_spec_31 : type  ttv_stack. ((ttv_stack, _menhir_box_parse) _menhir_cell1_KW_LET _menhir_cell0_ID, _menhir_box_parse) _menhir_cell1_var_list -> _ -> _ -> _ -> _ -> _menhir_box_parse =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok ->
      let e = _v in
      let _v = _menhir_action_14 e in
      _menhir_run_32 _menhir_stack _menhir_lexbuf _menhir_lexer _v MenhirState31 _tok
  
  and _menhir_run_14_spec_27 : type  ttv_stack. (((ttv_stack, _menhir_box_parse) _menhir_cell1_KW_LET _menhir_cell0_ID, _menhir_box_parse) _menhir_cell1_EQ, _menhir_box_parse) _menhir_cell1_expr -> _ -> _ -> _ -> _ -> _menhir_box_parse =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok ->
      let e = _v in
      let _v = _menhir_action_14 e in
      _menhir_run_18 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
  
  and _menhir_run_14_spec_29 : type  ttv_stack. (((((ttv_stack, _menhir_box_parse) _menhir_cell1_KW_LET _menhir_cell0_ID, _menhir_box_parse) _menhir_cell1_EQ, _menhir_box_parse) _menhir_cell1_expr, _menhir_box_parse) _menhir_cell1_KW_IN, _menhir_box_parse) _menhir_cell1_expr -> _ -> _ -> _ -> _ -> _menhir_box_parse =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok ->
      let e = _v in
      let _v = _menhir_action_14 e in
      _menhir_run_18 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
  
  and _menhir_run_14_spec_28 : type  ttv_stack. ((((ttv_stack, _menhir_box_parse) _menhir_cell1_KW_LET _menhir_cell0_ID, _menhir_box_parse) _menhir_cell1_EQ, _menhir_box_parse) _menhir_cell1_expr, _menhir_box_parse) _menhir_cell1_KW_IN -> _ -> _ -> _ -> _ -> _menhir_box_parse =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok ->
      let e = _v in
      let _v = _menhir_action_14 e in
      _menhir_run_29 _menhir_stack _menhir_lexbuf _menhir_lexer _v MenhirState28 _tok
  
  and _menhir_run_14_spec_10 : type  ttv_stack. ((ttv_stack, _menhir_box_parse) _menhir_cell1_KW_LET _menhir_cell0_ID, _menhir_box_parse) _menhir_cell1_EQ -> _ -> _ -> _ -> _ -> _menhir_box_parse =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok ->
      let e = _v in
      let _v = _menhir_action_14 e in
      _menhir_run_27 _menhir_stack _menhir_lexbuf _menhir_lexer _v MenhirState10 _tok
  
  and _menhir_run_14_spec_15 : type  ttv_stack. ((ttv_stack, _menhir_box_parse) _menhir_cell1_KW_IF, _menhir_box_parse) _menhir_cell1_expr -> _ -> _ -> _ -> _ -> _menhir_box_parse =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok ->
      let e = _v in
      let _v = _menhir_action_14 e in
      _menhir_run_18 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
  
  and _menhir_run_14_spec_24 : type  ttv_stack. ((((ttv_stack, _menhir_box_parse) _menhir_cell1_KW_IF, _menhir_box_parse) _menhir_cell1_expr, _menhir_box_parse) _menhir_cell1_KW_THEN, _menhir_box_parse) _menhir_cell1_expr -> _ -> _ -> _ -> _ -> _menhir_box_parse =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok ->
      let e = _v in
      let _v = _menhir_action_14 e in
      _menhir_run_18 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
  
  and _menhir_run_14_spec_26 : type  ttv_stack. ((((((ttv_stack, _menhir_box_parse) _menhir_cell1_KW_IF, _menhir_box_parse) _menhir_cell1_expr, _menhir_box_parse) _menhir_cell1_KW_THEN, _menhir_box_parse) _menhir_cell1_expr, _menhir_box_parse) _menhir_cell1_KW_ELSE, _menhir_box_parse) _menhir_cell1_expr -> _ -> _ -> _ -> _ -> _menhir_box_parse =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok ->
      let e = _v in
      let _v = _menhir_action_14 e in
      _menhir_run_18 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
  
  and _menhir_run_14_spec_25 : type  ttv_stack. (((((ttv_stack, _menhir_box_parse) _menhir_cell1_KW_IF, _menhir_box_parse) _menhir_cell1_expr, _menhir_box_parse) _menhir_cell1_KW_THEN, _menhir_box_parse) _menhir_cell1_expr, _menhir_box_parse) _menhir_cell1_KW_ELSE -> _ -> _ -> _ -> _ -> _menhir_box_parse =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok ->
      let e = _v in
      let _v = _menhir_action_14 e in
      _menhir_run_26 _menhir_stack _menhir_lexbuf _menhir_lexer _v MenhirState25 _tok
  
  and _menhir_run_14_spec_23 : type  ttv_stack. (((ttv_stack, _menhir_box_parse) _menhir_cell1_KW_IF, _menhir_box_parse) _menhir_cell1_expr, _menhir_box_parse) _menhir_cell1_KW_THEN -> _ -> _ -> _ -> _ -> _menhir_box_parse =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok ->
      let e = _v in
      let _v = _menhir_action_14 e in
      _menhir_run_24 _menhir_stack _menhir_lexbuf _menhir_lexer _v MenhirState23 _tok
  
  and _menhir_run_14_spec_22 : type  ttv_stack. (((ttv_stack, _menhir_box_parse) _menhir_cell1_expr, _menhir_box_parse) _menhir_cell1_LESSTHAN, _menhir_box_parse) _menhir_cell1_expr -> _ -> _ -> _ -> _ -> _menhir_box_parse =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok ->
      let e = _v in
      let _v = _menhir_action_14 e in
      _menhir_run_18 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
  
  and _menhir_run_14_spec_21 : type  ttv_stack. ((ttv_stack, _menhir_box_parse) _menhir_cell1_expr, _menhir_box_parse) _menhir_cell1_LESSTHAN -> _ -> _ -> _ -> _ -> _menhir_box_parse =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok ->
      let e = _v in
      let _v = _menhir_action_14 e in
      _menhir_run_22 _menhir_stack _menhir_lexbuf _menhir_lexer _v MenhirState21 _tok
  
  and _menhir_run_14_spec_20 : type  ttv_stack. (((ttv_stack, _menhir_box_parse) _menhir_cell1_expr, _menhir_box_parse) _menhir_cell1_MINUS, _menhir_box_parse) _menhir_cell1_expr -> _ -> _ -> _ -> _ -> _menhir_box_parse =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok ->
      let e = _v in
      let _v = _menhir_action_14 e in
      _menhir_run_18 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
  
  and _menhir_run_14_spec_19 : type  ttv_stack. ((ttv_stack, _menhir_box_parse) _menhir_cell1_expr, _menhir_box_parse) _menhir_cell1_MINUS -> _ -> _ -> _ -> _ -> _menhir_box_parse =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok ->
      let e = _v in
      let _v = _menhir_action_14 e in
      _menhir_run_20 _menhir_stack _menhir_lexbuf _menhir_lexer _v MenhirState19 _tok
  
  and _menhir_run_14_spec_18 : type  ttv_stack. ((ttv_stack, _menhir_box_parse) _menhir_cell1_expr, _menhir_box_parse) _menhir_cell1_expr -> _ -> _ -> _ -> _ -> _menhir_box_parse =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok ->
      let e = _v in
      let _v = _menhir_action_14 e in
      _menhir_run_18 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
  
  and _menhir_run_14_spec_17 : type  ttv_stack. (((ttv_stack, _menhir_box_parse) _menhir_cell1_expr, _menhir_box_parse) _menhir_cell1_PLUS, _menhir_box_parse) _menhir_cell1_expr -> _ -> _ -> _ -> _ -> _menhir_box_parse =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok ->
      let e = _v in
      let _v = _menhir_action_14 e in
      _menhir_run_18 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
  
  and _menhir_run_14_spec_16 : type  ttv_stack. ((ttv_stack, _menhir_box_parse) _menhir_cell1_expr, _menhir_box_parse) _menhir_cell1_PLUS -> _ -> _ -> _ -> _ -> _menhir_box_parse =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok ->
      let e = _v in
      let _v = _menhir_action_14 e in
      _menhir_run_17 _menhir_stack _menhir_lexbuf _menhir_lexer _v MenhirState16 _tok
  
  and _menhir_run_14_spec_11 : type  ttv_stack. (ttv_stack, _menhir_box_parse) _menhir_cell1_KW_IF -> _ -> _ -> _ -> _ -> _menhir_box_parse =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok ->
      let e = _v in
      let _v = _menhir_action_14 e in
      _menhir_run_15 _menhir_stack _menhir_lexbuf _menhir_lexer _v MenhirState11 _tok
  
  let rec _menhir_run_00 : type  ttv_stack. ttv_stack -> _ -> _ -> _menhir_box_parse =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer ->
      let _tok = _menhir_lexer _menhir_lexbuf in
      match (_tok : MenhirBasics.token) with
      | TRUE ->
          let _tok = _menhir_lexer _menhir_lexbuf in
          let _v = _menhir_action_07 () in
          _menhir_run_44 _menhir_stack _menhir_lexbuf _menhir_lexer _v MenhirState00 _tok
      | NUMBER _v ->
          let _tok = _menhir_lexer _menhir_lexbuf in
          let n = _v in
          let _v = _menhir_action_12 n in
          _menhir_run_44 _menhir_stack _menhir_lexbuf _menhir_lexer _v MenhirState00 _tok
      | NEG ->
          _menhir_run_03 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState00
      | LEFT_PARENTHESIS ->
          _menhir_run_05 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState00
      | KW_LET ->
          _menhir_run_06 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState00
      | KW_IF ->
          _menhir_run_11 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState00
      | ID _v ->
          let _tok = _menhir_lexer _menhir_lexbuf in
          let x = _v in
          let _v = _menhir_action_06 x in
          _menhir_run_44 _menhir_stack _menhir_lexbuf _menhir_lexer _v MenhirState00 _tok
      | FALSE ->
          let _tok = _menhir_lexer _menhir_lexbuf in
          let _v = _menhir_action_08 () in
          _menhir_run_44 _menhir_stack _menhir_lexbuf _menhir_lexer _v MenhirState00 _tok
      | _ ->
          _eRR ()
  
end

let parse =
  fun _menhir_lexer _menhir_lexbuf ->
    let _menhir_stack = () in
    let MenhirBox_parse v = _menhir_run_00 _menhir_stack _menhir_lexbuf _menhir_lexer in
    v
