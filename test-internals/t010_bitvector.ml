open Wc_bitvector
open Printf

let bad = ref false

let subtest name f =
  try
    let r = f() in
    if not r then (
      eprintf "[subtest %s: failed] %!" name;
      bad := true
    );
    ()
  with
    | error ->
        bad := true;
        eprintf "[subtest %s: exception %s] %!" name (Printexc.to_string error)

let () =
  subtest
    "A"
    (fun () ->
      let s =
        empty |> set 5 true |> set 7 false |> set 10 true |> set 20 true in
      elements s = [ 5; 10; 20 ]
    )

let () =
  subtest
    "B"
    (fun () ->
      let s =
        empty |> set 5 true |> set 7 true |> set 5 false |> set 20 true in
      elements s = [ 7; 20 ]
    )

let () =
  subtest
    "C"
    (fun () ->
      let s =
        empty |> set 5 true |> set 7 false |> set 10 true |> set 31 true in
      elements s = [ 5; 10; 31 ]
    )

let () =
  subtest
    "D"
    (fun () ->
      let s =
        empty |> set 65 true in
      elements s = [ 65 ]
    )

let () =
  subtest
    "E"
    (fun () ->
      let s =
        empty |> set 5 true |> set 7 false |> set 10 true |> set 31 true in
      get 5 s && not (get 6 s) && get 31 s
    )

let () =
  subtest
    "F"
    (fun () ->
      let s1 =
        empty |> set 65 true in
      let s2 =
        empty |> set 101 true in
      let s =
        union s1 s2 in
      elements s = [ 65; 101 ]
    )

let () =
  subtest
    "G"
    (fun () ->
      let s1 =
        empty |> set 65 true |> set 101 true in
      let s2 =
        empty |> set 5 true in
      let s =
        union s1 s2 in
      elements s = [ 5; 65; 101 ]
    )

let () =
  exit(if !bad then 1 else 0)
