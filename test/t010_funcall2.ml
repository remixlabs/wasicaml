let add_something x y z =
  (x * y) + 15 - z

let () =
  Testprint.int "x" (add_something 3 6 8)
