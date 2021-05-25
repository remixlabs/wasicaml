(* This is supported: *)

let rec f2 b c d e =
  b * c * d * e

and f1 a b c =
  f2 (a+b) (a+c) (a-b) (a-c)

let () =
  let x = f1 3 4 5 in
  Testprint.int "x" x
