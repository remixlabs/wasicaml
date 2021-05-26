let make n =
  Array.init
    n
    (fun i -> Random.int n)

let f() =
  let t0 = Testprint.clock64() in
  Random.init 153873487;
  let n = 500000 in
  let a = make n in
  for k = 0 to 19 do
    Testprint.int "unsorted" a.(k)
  done;
  (* Array.sort is quite slow because it makes heavy use of exceptions *)
  Array.stable_sort (fun p q -> p - q) a;
  for i = 0 to n - 2 do
    assert(a.(i) <= a.(i+1))
  done;
  for k = 0 to 19 do
    Testprint.int "sorted" a.(k)
  done;
  let t1 = Testprint.clock64() in
  Printf.eprintf "time=%Ld ms\n%!" (Int64.div (Int64.sub t1 t0) 1_000_000L)

let () = f()
