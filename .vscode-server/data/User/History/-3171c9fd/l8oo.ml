let rec factorial a =
  if a < 0 then -1
  else if a <= 1 then 1
  else a * (factorial(a-1))

let rec fib b =
  if b < 0 then -1
  else if b = 0 then 0
  else if b = 1 then 1
  else if b = 2 then 2
  else fib(b-1) + fib(b-2)

let rec acc c =
  if c = 0 then 0
  else if c > 0 then acc(c-1) + c
  else acc(c+1) + c

let rec pow d e = 
  if e<0 then 0
  else if e=0 then 1
  else (pow d e-1)*d


let _ = Format.printf "%d" factorial (3)
let _ = Format.printf "%d" fib (3)
let _ = Format.printf "%d" acc (3)
let _ = Format.printf "%d" pow (2 2)