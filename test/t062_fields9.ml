let current = ref (fun () -> Testprint.string "fun" "p1")

let f() =
  current := (fun () -> Testprint.string "fun" "p2");
  let h = !current in
  current := (fun () -> Testprint.string "fun" "p3");
  h();
  !current()

let () =
  f()

