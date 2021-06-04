(* Copyright (C) 2021 by Figly, Inc.
   This code is free software, see the file LICENSE for details.
 *)

open Printf

type number =
  | I32 of int32
  | I64 of int64
  | F64 of float

let string_of_number = function
  | I32 x -> sprintf "0x%lx" x
  | I64 x -> sprintf "0x%Lx" x
  | F64 x -> sprintf "%h" x
