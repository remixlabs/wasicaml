let f() =
  let b = Bytes.of_string "hello" in
  Testprint.int "c2" (Char.code (Bytes.get b 2));
  Testprint.int "c2_unsafe" (Char.code (Bytes.unsafe_get b 2));
  let k = 2+1 in
  Testprint.int "c3_k" (Char.code (Bytes.unsafe_get b k));
  Bytes.unsafe_set b 3 'L';
  Bytes.unsafe_set b k 'L';
  Testprint.string "b" (Bytes.to_string b)

let () =
  f()


