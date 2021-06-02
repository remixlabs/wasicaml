let rec f n =
  if n > 10000 then raise Not_found;
  1 + f (n+1)

let f() =
  try f 0 with Not_found -> (-1)

let () =
  let x = f() in
  Testprint.int "x" x
