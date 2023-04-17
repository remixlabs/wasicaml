let next = ref 0

let f() =
  let k = !next in
  incr next;
  Testprint.int "k" k;
  Testprint.int "next" !next

let () =
  f()
