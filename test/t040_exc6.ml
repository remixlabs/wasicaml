let f1() =
  Testprint.string "test" "f1";
  let k = ref 0 in
  ( try
      incr k;
      k := (100 + 200) / 0
    with
      | Division_by_zero -> incr k
  );
  Testprint.int "k" !k

let f2() =
  Testprint.string "test" "f2";
  let k = ref 0 in
  ( try
      incr k;
      k := Bytes.length (Bytes.create (-5))
    with
      | Invalid_argument _ -> incr k
  );
  Testprint.int "k" !k

let () =
  f1();
  f2()
