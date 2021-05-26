external string : string -> string -> unit = "testprint_string"
external int : string -> int -> unit = "testprint_int"
external float : string -> float -> unit = "testprint_float"

external vector : int -> int -> int -> int -> int -> int -> int array = "testprint_vector_byte" "testprint_vector"

external clock : unit -> (int * int) = "testprint_clock"

let clock64() =
  let (sec, nsec) = clock() in
  Int64.add
    (Int64.mul
       (Int64.of_int sec)
       1_000_000_000L)
    (Int64.of_int nsec)
