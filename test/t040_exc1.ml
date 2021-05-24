let f() =
  try
    "hello"
  with Not_found -> ""

let () =
  let x = f() in
  Testprint.string "x" x
