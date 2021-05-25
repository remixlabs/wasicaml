type r =
  { a : int;
    b : float;
    c : int
  }

let f() =
  let r1 = { a = 10; b = 11.0; c = 12 } in
  Testprint.int "a" r1.a;
  Testprint.float "b" r1.b;
  Testprint.int "c" r1.c;
  let r2 = { r1 with b = 13.0 } in
  Testprint.int "a" r2.a;
  Testprint.float "b" r2.b;
  Testprint.int "c" r2.c

let () =
  f()
