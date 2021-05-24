let f() =
  let x1 = 1 in
  let x3 = 3 in
  let x4 = 4 in
  let x8 = 8 in
  let x9 = 9 in
  let xm3 = (-3) in
  let xm8 = (-8) in
  let xm11 = (-11) in
  Testprint.int "a" (x3 + x4);
  Testprint.int "b" (x3 - x4);
  Testprint.int "c" (x3 * x4);
  Testprint.int "d" (x8 / x3);
  Testprint.int "a" (x3 + x4);
  Testprint.int "e" (x8 mod x3);
  Testprint.int "f" (x8 mod xm3);
  Testprint.int "g" (xm8 mod x3);
  Testprint.int "h" (xm8 mod xm3);
  Testprint.int "i" (x9 land x3);
  Testprint.int "j" (x8 lor x3);
  Testprint.int "k" (x8 lxor x3);
  Testprint.int "l" (-(x4 + x4));
  Testprint.int "m" (lnot x8);
  Testprint.int "n" (x3 lsl x4);
  Testprint.int "o" (xm11 asr x1);
  Testprint.int "p" (xm11 lsr x1);
  ()

let () = f()
