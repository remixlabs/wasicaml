let compute_something x y z =
  (x * y) + 15 - z

let () =
  let f = compute_something 4 5 in
  Testprint.int "x" (f 2)
