type t =
  | A
  | B of int
  | C
  | D of int

let f x =
  match x with
    | A ->
        Testprint.string "x" "A"
    | B x ->
        Testprint.string "x" "B";
        Testprint.int "arg" x
    | C ->
        Testprint.string "x" "C"
    | D x ->
        Testprint.string "x" "D";
        Testprint.int "arg" x

let g() =
  f A;
  f (B 10);
  f C;
  f (D 11)

let () =
  g()
