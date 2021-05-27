(* reverse a string char by char *)

let make n =
  let s = Bytes.create n in
  for i = 0 to n-1 do
    Bytes.set s i (Char.chr (i land 0xff))
  done;
  s

let rev s =
  let len = Bytes.length s in
  for i = 0 to len/2-1 do
    Bytes.unsafe_set s 0 (Bytes.unsafe_get s (len-1-i))
  done;
  ()

let f() =
  let t0 = Testprint.clock64() in
  let s = make 10000 in
  for k = 1 to 10000 do
    rev s
  done;
  Testprint.string "s" (Bytes.to_string s);
  let t1 = Testprint.clock64() in
  Printf.eprintf "time=%Ld ms\n%!" (Int64.div (Int64.sub t1 t0) 1_000_000L);
  ()

let () = f()
