let compute_something x y =
  let p = (x * y) + 15 in
  fun z -> p - z

let () =
  let f = compute_something 4 5 in
  Testprint.int "x" (f 2)
