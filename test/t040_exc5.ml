(* like t040_exc4.ml but in the main function (which is compiled differently) *)

let () =
  Testprint.string "test" "f1";
  let k = ref 0 in
  ( try
      incr k;
      raise Not_found
    with
      | Not_found -> incr k
  );
  Testprint.int "k" !k;

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
  Testprint.int "k" !k;

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
