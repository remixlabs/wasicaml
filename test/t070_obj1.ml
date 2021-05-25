let f() =
  let obj =
    object(self)
      method m1 = self#m2
      method m2 = 12
    end in
  Testprint.int "m2" (obj # m2);
  Testprint.int "m1" (obj # m1)

let () = f()
