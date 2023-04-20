
module MenhirBasics = struct
  
  exception Error
  
  let _eRR =
    fun _s ->
      raise Error
  
  type token = 
    | RIGHT_PARENTHESIS
    | PLUS
    | NUMBER of (
# 4 "parser/parser.mly"
       (int)
# 17 "parser/parser.ml"
  )
    | MINUS
    | LEFT_PARENTHESIS
    | EOF
  
end

include MenhirBasics

# 1 "parser/parser.mly"
  
  open Ast

# 31 "parser/parser.ml"

type ('s, 'r) _menhir_state = 
  | MenhirState00 : ('s, _menhir_box_parse) _menhir_state
    (** State 00.
        Stack shape : .
        Start symbol: parse. *)

  | MenhirState04 : (('s, _menhir_box_parse) _menhir_cell1_LEFT_PARENTHESIS, _menhir_box_parse) _menhir_state
    (** State 04.
        Stack shape : LEFT_PARENTHESIS.
        Start symbol: parse. *)

  | MenhirState08 : (('s, _menhir_box_parse) _menhir_cell1_expr, _menhir_box_parse) _menhir_state
    (** State 08.
        Stack shape : expr.
        Start symbol: parse. *)

  | MenhirState10 : (('s, _menhir_box_parse) _menhir_cell1_expr, _menhir_box_parse) _menhir_state
    (** State 10.
        Stack shape : expr.
        Start symbol: parse. *)


and ('s, 'r) _menhir_cell1_expr = 
  | MenhirCell1_expr of 's * ('s, 'r) _menhir_state * (Ast.expr)

and ('s, 'r) _menhir_cell1_LEFT_PARENTHESIS = 
  | MenhirCell1_LEFT_PARENTHESIS of 's * ('s, 'r) _menhir_state

and _menhir_box_parse = 
  | MenhirBox_parse of (Ast.expr) [@@unboxed]

let _menhir_action_1 =
  fun e1 e2 ->
    (
# 17 "parser/parser.mly"
                         ( Add (e1, e2) )
# 69 "parser/parser.ml"
     : (Ast.expr))

let _menhir_action_2 =
  fun e1 e2 ->
    (
# 18 "parser/parser.mly"
                                     ( Sub (e1, e2) )
# 77 "parser/parser.ml"
     : (Ast.expr))

let _menhir_action_3 =
  fun n ->
    (
# 19 "parser/parser.mly"
             ( Num n )
# 85 "parser/parser.ml"
     : (Ast.expr))

let _menhir_action_4 =
  fun n ->
    (
# 20 "parser/parser.mly"
                   ( Num (-1 * n) )
# 93 "parser/parser.ml"
     : (Ast.expr))

let _menhir_action_5 =
  fun e ->
    (
# 21 "parser/parser.mly"
                 ( e )
# 101 "parser/parser.ml"
     : (Ast.expr))

let _menhir_action_6 =
  fun e ->
    (
# 24 "parser/parser.mly"
                                              ( e )
# 109 "parser/parser.ml"
     : (Ast.expr))

let _menhir_action_7 =
  fun e ->
    (
# 14 "parser/parser.mly"
               ( e )
# 117 "parser/parser.ml"
     : (Ast.expr))

let _menhir_print_token : token -> string =
  fun _tok ->
    match _tok with
    | EOF ->
        "EOF"
    | LEFT_PARENTHESIS ->
        "LEFT_PARENTHESIS"
    | MINUS ->
        "MINUS"
    | NUMBER _ ->
        "NUMBER"
    | PLUS ->
        "PLUS"
    | RIGHT_PARENTHESIS ->
        "RIGHT_PARENTHESIS"

let _menhir_fail : unit -> 'a =
  fun () ->
    Printf.eprintf "Internal failure -- please contact the parser generator's developers.\n%!";
    assert false

include struct
  
  [@@@ocaml.warning "-4-37-39"]
  
  let rec _menhir_run_13 : type  ttv_stack. ttv_stack -> _ -> _ -> _ -> (ttv_stack, _menhir_box_parse) _menhir_state -> _ -> _menhir_box_parse =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      match (_tok : MenhirBasics.token) with
      | PLUS ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_08 _menhir_stack _menhir_lexbuf _menhir_lexer
      | MINUS ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_10 _menhir_stack _menhir_lexbuf _menhir_lexer
      | EOF ->
          let e = _v in
          let _v = _menhir_action_7 e in
          MenhirBox_parse _v
      | _ ->
          _eRR ()
  
  and _menhir_run_08 : type  ttv_stack. (ttv_stack, _menhir_box_parse) _menhir_cell1_expr -> _ -> _ -> _menhir_box_parse =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer ->
      let _tok = _menhir_lexer _menhir_lexbuf in
      match (_tok : MenhirBasics.token) with
      | NUMBER _v ->
          let _tok = _menhir_lexer _menhir_lexbuf in
          let n = _v in
          let _v = _menhir_action_3 n in
          _menhir_run_09 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | MINUS ->
          _menhir_run_02 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState08
      | LEFT_PARENTHESIS ->
          _menhir_run_04 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState08
      | _ ->
          _eRR ()
  
  and _menhir_run_09 : type  ttv_stack. (ttv_stack, _menhir_box_parse) _menhir_cell1_expr -> _ -> _ -> _ -> _ -> _menhir_box_parse =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok ->
      let MenhirCell1_expr (_menhir_stack, _menhir_s, e1) = _menhir_stack in
      let e2 = _v in
      let _v = _menhir_action_1 e1 e2 in
      _menhir_goto_expr _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
  
  and _menhir_goto_expr : type  ttv_stack. ttv_stack -> _ -> _ -> _ -> (ttv_stack, _menhir_box_parse) _menhir_state -> _ -> _menhir_box_parse =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      match _menhir_s with
      | MenhirState00 ->
          _menhir_run_13 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState10 ->
          _menhir_run_11 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | MenhirState08 ->
          _menhir_run_09 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | MenhirState04 ->
          _menhir_run_06 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
  
  and _menhir_run_11 : type  ttv_stack. (ttv_stack, _menhir_box_parse) _menhir_cell1_expr -> _ -> _ -> _ -> _ -> _menhir_box_parse =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok ->
      let MenhirCell1_expr (_menhir_stack, _menhir_s, e1) = _menhir_stack in
      let e2 = _v in
      let _v = _menhir_action_2 e1 e2 in
      _menhir_goto_expr _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
  
  and _menhir_run_06 : type  ttv_stack. ((ttv_stack, _menhir_box_parse) _menhir_cell1_LEFT_PARENTHESIS as 'stack) -> _ -> _ -> _ -> ('stack, _menhir_box_parse) _menhir_state -> _ -> _menhir_box_parse =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      match (_tok : MenhirBasics.token) with
      | RIGHT_PARENTHESIS ->
          let _tok = _menhir_lexer _menhir_lexbuf in
          let MenhirCell1_LEFT_PARENTHESIS (_menhir_stack, _menhir_s) = _menhir_stack in
          let e = _v in
          let _v = _menhir_action_6 e in
          let e = _v in
          let _v = _menhir_action_5 e in
          _menhir_goto_expr _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | PLUS ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_08 _menhir_stack _menhir_lexbuf _menhir_lexer
      | MINUS ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_10 _menhir_stack _menhir_lexbuf _menhir_lexer
      | _ ->
          _eRR ()
  
  and _menhir_run_10 : type  ttv_stack. (ttv_stack, _menhir_box_parse) _menhir_cell1_expr -> _ -> _ -> _menhir_box_parse =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer ->
      let _tok = _menhir_lexer _menhir_lexbuf in
      match (_tok : MenhirBasics.token) with
      | NUMBER _v ->
          let _tok = _menhir_lexer _menhir_lexbuf in
          let n = _v in
          let _v = _menhir_action_3 n in
          _menhir_run_11 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | MINUS ->
          _menhir_run_02 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState10
      | LEFT_PARENTHESIS ->
          _menhir_run_04 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState10
      | _ ->
          _eRR ()
  
  and _menhir_run_02 : type  ttv_stack. ttv_stack -> _ -> _ -> (ttv_stack, _menhir_box_parse) _menhir_state -> _menhir_box_parse =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s ->
      let _tok = _menhir_lexer _menhir_lexbuf in
      match (_tok : MenhirBasics.token) with
      | NUMBER _v ->
          let _tok = _menhir_lexer _menhir_lexbuf in
          let n = _v in
          let _v = _menhir_action_4 n in
          _menhir_goto_expr _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | _ ->
          _eRR ()
  
  and _menhir_run_04 : type  ttv_stack. ttv_stack -> _ -> _ -> (ttv_stack, _menhir_box_parse) _menhir_state -> _menhir_box_parse =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s ->
      let _menhir_stack = MenhirCell1_LEFT_PARENTHESIS (_menhir_stack, _menhir_s) in
      let _tok = _menhir_lexer _menhir_lexbuf in
      match (_tok : MenhirBasics.token) with
      | NUMBER _v ->
          let _tok = _menhir_lexer _menhir_lexbuf in
          let n = _v in
          let _v = _menhir_action_3 n in
          _menhir_run_06 _menhir_stack _menhir_lexbuf _menhir_lexer _v MenhirState04 _tok
      | MINUS ->
          _menhir_run_02 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState04
      | LEFT_PARENTHESIS ->
          _menhir_run_04 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState04
      | _ ->
          _eRR ()
  
  let rec _menhir_run_00 : type  ttv_stack. ttv_stack -> _ -> _ -> _menhir_box_parse =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer ->
      let _tok = _menhir_lexer _menhir_lexbuf in
      match (_tok : MenhirBasics.token) with
      | NUMBER _v ->
          let _tok = _menhir_lexer _menhir_lexbuf in
          let n = _v in
          let _v = _menhir_action_3 n in
          _menhir_run_13 _menhir_stack _menhir_lexbuf _menhir_lexer _v MenhirState00 _tok
      | MINUS ->
          _menhir_run_02 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState00
      | LEFT_PARENTHESIS ->
          _menhir_run_04 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState00
      | _ ->
          _eRR ()
  
end

let parse =
  fun _menhir_lexer _menhir_lexbuf ->
    let _menhir_stack = () in
    let MenhirBox_parse v = _menhir_run_00 _menhir_stack _menhir_lexbuf _menhir_lexer in
    v
