type op =
  | ADD
  | SUB
  | MUL
  | DIV

type value =
  | Err
  | Int of int
  | Float of float

type expr =
  | E of op * expr * expr
  | V of value

let rec eval expr =
  begin
    match expr with
      | V v -> v
      | E(op, expr1, expr2) ->
        let v1 = eval expr1 in 
        let v2 = eval expr2 in
        begin
          match v1, v2 with
          | Int i1, Int i2 -> 
          begin
            match op with
            | ADD -> Int (i1 + i2)
            | SUB -> Int (i1 - i2)
            | MUL -> Int (i1 * i2)
            | DIV -> if i2 = 0 then Err else Int (i1 / i2)
          end
          | Float f1, Float f2 ->
          begin
            match op with
            | ADD -> Float (f1 +. f2)
            | SUB -> Float (f1 -. f2)
            | MUL -> Float (f1 *. f2)
            | DIV -> if f2 = 0. then Err else Float (f1 /. f2)
          end
          | Int i, Float f
          | Float f, Int i ->
          begin
            match op with
            | ADD -> Float ((float_of_int i) +. f)
            | SUB -> Float ((float_of_int i) -. f)
            | MUL -> Float ((float_of_int i) *. f)
            | DIV -> if f = 0. then Err else Float ((float_of_int i) /. f)
          end
          |  _ -> Err
        end
      end