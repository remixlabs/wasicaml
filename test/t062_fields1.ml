type r =
  { a : int;
    b : int;
    c : int
  }

let f() =
  let r1 = { a = 10; b = 11; c = 12 } in
  Testprint.int "a" r1.a;
  Testprint.int "b" r1.b;
  Testprint.int "c" r1.c;
  let r2 = { r1 with b = 12 } in
  Testprint.int "a" r2.a;
  Testprint.int "b" r2.b;
  Testprint.int "c" r2.c

let () =
  f()
