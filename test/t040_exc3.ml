let g() =
  failwith "hello"

let f() =
  try
    g()
  with Failure msg -> msg

let () =
  let x = f() in
  Testprint.string "x" x
