let f() =
  let a = [| 10; 11; 12 |] in
  Array.unsafe_set a 1 13;
  Testprint.int "a[1] unsafe" (Array.unsafe_get a 1);
  let k = 1 in
  Array.unsafe_set a k 13;
  Testprint.int "a[k] unsafe" (Array.unsafe_get a k)

let () =
  f()
