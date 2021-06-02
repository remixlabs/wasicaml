let f1() =
  Testprint.string "test" "f1";
  let k = ref 0 in
  ( try
      incr k;
      raise Not_found
    with
      | Not_found -> incr k
  );
  Testprint.int "k" !k

let f2() =
  Testprint.string "test" "f2";
  let k = ref 0 in
  ( try
      try
        incr k;
        raise Not_found
      with
        | Exit -> k := 100
    with
      | Not_found -> incr k
  );
  Testprint.int "k" !k

let f3() =
  Testprint.string "test" "f3";
  let k = ref 0 in
  ( try
      try
        incr k;
        raise Not_found
      with
        | e -> k := 10; raise e
    with
      | Not_found -> incr k
  );
  Testprint.int "k" !k

let () =
  f1();
  f2();
  f3()

