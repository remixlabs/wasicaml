let f() =
  let a = Testprint.vector 10 11 12 13 14 15 in
  Testprint.int "a0" a.(0);
  Testprint.int "a1" a.(1);
  Testprint.int "a2" a.(2);
  Testprint.int "a3" a.(3);
  Testprint.int "a4" a.(4);
  Testprint.int "a5" a.(5);
  ()

let () = f()
