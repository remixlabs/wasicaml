(* This is supported: *)

let rec f2 a b c =
  b * c * c

and f1 a b c =
  f2 (a+b) c

let () =
  let x = f1 3 4 5 6 in
  Testprint.int "x" x
