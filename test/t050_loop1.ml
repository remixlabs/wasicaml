let f () =
  let x = ref 0 in
  for k = 1 to 10 do
    x := !x + k
  done;
  !x

let () =
  let x = f() in
  Testprint.int "x" x
