let compute_something p q r s =
  (p * q) - (r * s)

let () =
  let x = compute_something 2 3 4 5 in
  Testprint.int "x" x
