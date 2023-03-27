module IMap = Map.Make(Int)

let f() =
  let m =
    [ 370,0; 921,1; 20,2; 104,3; 500,4; 439,5; 641,6; 182,7; 685,8; 344,9 ]
    |> List.map (fun (k,v) -> Testprint.int "k" k; (k,v))
    |> List.to_seq
    |> IMap.of_seq in
  IMap.iter
    (fun x y -> Testprint.int "key" x; Testprint.int "val" y)
    m

let () = f()
