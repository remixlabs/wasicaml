let f() =
  try
    let a = 0 in
    let b = 0 in
    a / b
  with
    | Division_by_zero -> 50

let g() =
  try
    let a = 0 in
    let b = 0 in
    a mod b
  with
    | Division_by_zero -> 50
                        
let () =
  let x = f() in
  Testprint.int "x" x;
  let y = f() in
  Testprint.int "y" y
