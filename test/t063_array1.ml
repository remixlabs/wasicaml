let f() =
  let a = [| 10; 11; 12 |] in
  Testprint.int "len" (Array.length a);
  Testprint.int "a[1]" a.(1);
  Testprint.int "a[1] unsafe" (Array.unsafe_get a 1);
  let k = 1 in
  Testprint.int "a[k]" a.(k);
  Testprint.int "a[k] unsafe" (Array.unsafe_get a k)

let () =
  f()
