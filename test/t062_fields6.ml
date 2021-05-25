let f() =
  let x = ref 10 in
  incr x;
  Testprint.int "x" !x

let () =
  f()
