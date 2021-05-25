(* This is supported: *)

let rec f2 a0 a1 a2 a3 a4 a5 a6 a7 a8 a9 a10 =
  a0 * (a1 + a2 + a3 + a4 + a5 + a6 + a7 + a8 + a9) * a10

and f1 a b c =
  f2 (a+b) (a+c) (a+c+5) (a+c+7) (a+c+11) (a+c+18) (a+c+22) (a+c+27) (a+c+31) (a+c+45) (a-c)

let () =
  let x = f1 3 4 5 in
  Testprint.int "x" x
