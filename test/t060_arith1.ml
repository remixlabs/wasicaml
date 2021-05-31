let f_int() =
  Testprint.string "f" "int";
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

let f_int_unclean() =
  Testprint.string "f" "int_unclean";
  (* multiplication results in an RIntUnclean value, and this allows us to
     set the bit 31 to 1
   *)
  let x3 = 4 * 0x2000_0003 in
  let x4 = 4 * 0x2000_0005 in
  let x8 = 4 * 0x2000_0006 in
  let x9 = 4 * 0x2000_0007 in
  let xm3 = (-4) * 0x2000_0002 in
  let xm8 = (-4) * 0x2000_0005 in
  let xm11 = (-4) * 0x2000_0007 in
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
  Testprint.int "n" (x3 lsl 4);
  Testprint.int "o" (xm11 asr 1);
  Testprint.int "p" (xm11 lsr 1);
  ()

let f_intval() =
  Testprint.string "f" "intval";
  (* the result of an addition is always an intval *)
  let x1 = 1 + 0 in
  let x3 = 3 + 0 in
  let x4 = 4 + 0 in
  let x8 = 8 + 0 in
  let x9 = 9 + 0 in
  let xm3 = (-3) + 0 in
  let xm8 = (-8) + 0 in
  let xm11 = (-11) + 0 in
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

let () =
  f_int();
  f_int_unclean();
  f_intval()
