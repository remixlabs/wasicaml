type r =
  { a : float;
    b : float;
    c : float
  }

let f() =
  let r1 = { a = 10.0; b = 11.0; c = 12.0 } in
  Testprint.float "a" r1.a;
  Testprint.float "b" r1.b;
  Testprint.float "c" r1.c;
  let r2 = { r1 with b = 13.0 } in
  Testprint.float "a" r2.a;
  Testprint.float "b" r2.b;
  Testprint.float "c" r2.c

let () =
  f()
