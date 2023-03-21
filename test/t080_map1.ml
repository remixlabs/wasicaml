module IMap = Map.Make(Int)

let f() =
  let m =
    List.to_seq [ 10, 1; 4, 2; 6, 3; 20, 4; 8, 5; 16, 6; 2, 7; 0, 8 ]
    |> IMap.of_seq in
  IMap.iter
    (fun x y -> Testprint.int "key" x; Testprint.int "val" y)
    m

let () = f()
