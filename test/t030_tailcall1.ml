(* this generates an APPTERM instruction but wasm cannot yet
   support a tail call. Nevertheless test whether the replacement
   is ok
 *)


let f2 b c =
  b + c

let f1 a b c =
  f2 (a*b) (a*c)

let () =
  let x = f1 3 4 5 in
  Testprint.int "x" x
