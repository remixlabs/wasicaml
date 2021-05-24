let g() =
  raise Not_found

let f() =
  try
    g()
  with Not_found -> "hello"

let () =
  let x = f() in
  Testprint.string "x" x
