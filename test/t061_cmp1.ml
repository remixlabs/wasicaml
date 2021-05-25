let f (a:int) (b:int) =
  let boolint x = if x then 1 else 0 in
  Testprint.int "x0" (boolint (a = b));
  Testprint.int "x1" (boolint (a = a));
  Testprint.int "x2" (boolint (b = a));

  Testprint.int "x3" (boolint (a < b));
  Testprint.int "x4" (boolint (a < a));
  Testprint.int "x5" (boolint (b < a));

  Testprint.int "x6" (boolint (a <= b));
  Testprint.int "x7" (boolint (a <= a));
  Testprint.int "x8" (boolint (b <= a));

  Testprint.int "x9" (boolint (a > b));
  Testprint.int "x10" (boolint (a > a));
  Testprint.int "x11" (boolint (b > a));

  Testprint.int "x12" (boolint (a >= b));
  Testprint.int "x13" (boolint (a >= a));
  Testprint.int "x14" (boolint (b >= a));

  Testprint.int "x15" (boolint (a <> b));
  Testprint.int "x16" (boolint (a <> a));
  Testprint.int "x17" (boolint (b <> a));

  ()
  
let g (a:int) (b:int) =
  Testprint.int "x0" (if a = b then 1 else 0);
  Testprint.int "x1" (if a = a then 1 else 0);
  Testprint.int "x2" (if b = a then 1 else 0);

  Testprint.int "x3" (if a < b then 1 else 0);
  Testprint.int "x4" (if a < a then 1 else 0);
  Testprint.int "x5" (if b < a then 1 else 0);

  Testprint.int "x6" (if a <= b then 1 else 0);
  Testprint.int "x7" (if a <= a then 1 else 0);
  Testprint.int "x8" (if b <= a then 1 else 0);

  Testprint.int "x9" (if a > b then 1 else 0);
  Testprint.int "x10" (if a > a then 1 else 0);
  Testprint.int "x11" (if b > a then 1 else 0);

  Testprint.int "x12" (if a >= b then 1 else 0);
  Testprint.int "x13" (if a >= a then 1 else 0);
  Testprint.int "x14" (if b >= a then 1 else 0);

  Testprint.int "x15" (if a <> b then 1 else 0);
  Testprint.int "x16" (if a <> a then 1 else 0);
  Testprint.int "x17" (if b <> a then 1 else 0);

  ()
  
let () =
  f 10 11;
  g 10 11;
  f 10 (-11);
  g 10 (-11);
  f (-10) 11;
  g (-10) 11;
  f (-10) (-11);
  g (-10) (-11)
