let is_digit c =
  match c with
    | '0'..'9' -> true
    | _ -> false

let f() =
  Testprint.string "x2" (is_digit '/' |> string_of_bool);
  Testprint.string "x3" (is_digit '0' |> string_of_bool);
  Testprint.string "x5" (is_digit '1' |> string_of_bool);
  Testprint.string "x4" (is_digit '8' |> string_of_bool);
  Testprint.string "x4" (is_digit '9' |> string_of_bool);
  Testprint.string "x1" (is_digit ':' |> string_of_bool);
  ()

let () = f()
