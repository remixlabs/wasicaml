let f () =
  let s = "20200301125632" in
  let i = Int64.of_string s in
  Testprint.string "i" (Int64.to_string i)

let () = f()
