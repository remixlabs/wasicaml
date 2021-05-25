type r =
  { a : int;
    mutable b : int;
    c : int
  }

let f() =
  let r1 = { a = 10; b = 11; c = 12 } in
  Testprint.int "a" r1.a;
  Testprint.int "b" r1.b;
  Testprint.int "c" r1.c;
  r1.b <- 13;
  Testprint.int "a" r1.a;
  Testprint.int "b" r1.b;
  Testprint.int "c" r1.c

let () =
  f()
