let add_something x =
  x + 15

let () =
  Testprint.int "x" (add_something 3)
