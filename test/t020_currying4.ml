let compute_something x y =
  let p = (x * y) + 15 in
  fun z u -> p - (z * u)

let () =
  let f1 = compute_something 4 in
  let f2 = f1 5 3 in
  let x = f2 2 in
  Testprint.int "x" x
