let compute_something x y =
  let p = (x * y) + 15 in
  fun z -> Testprint.int "p" p; Testprint.int "z" z; p - z

let () =
  let x = compute_something 4 5 2 in
  Testprint.int "x" x
