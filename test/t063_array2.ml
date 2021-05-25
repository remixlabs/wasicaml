let f() =
  let a = [||] in
  Testprint.int "len" (Array.length a)

let () =
  f()
