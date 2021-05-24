let rec f1 a b c =
  a * f2 b c

and f2 b c =
  b + c

let () =
  let x = f1 3 4 5 in
  Testprint.int "x" x
