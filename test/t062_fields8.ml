let next = ref 0

let g() =
  incr next

let f() =
  let k = !next in
  g();
  Testprint.int "k" k;
  Testprint.int "next" !next

let () =
  f()
