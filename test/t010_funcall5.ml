let f a b c =
  let rec f1 b c =
    a * f2 b c

  and f2 b c =
    b + (a*c) in

  f1 b c + 1

let () =
  let x = f 3 4 5 in
  Testprint.int "x" x
