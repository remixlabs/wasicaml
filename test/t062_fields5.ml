type r =
  { a : float;
    mutable b : float;
    c : float
  }

let f() =
  let r1 = { a = 10.0; b = 11.0; c = 12.0 } in
  Testprint.float "a" r1.a;
  Testprint.float "b" r1.b;
  Testprint.float "c" r1.c;
  r1.b <- 13.0;
  Testprint.float "a" r1.a;
  Testprint.float "b" r1.b;
  Testprint.float "c" r1.c

let () =
  f()
