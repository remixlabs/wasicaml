(* Copyright (C) 2021 by Figly, Inc.
   This code is free software, see the file LICENSE for details.
 *)

open Wc_types

type executable =
  { dll_paths : string list;
    dll_names : string list;
    primitives : string array;
    code : string;
    data : (char, Bigarray.int8_unsigned_elt, Bigarray.c_layout) Bigarray.Array1.t;
    symbols : Symtable.global_map;
    debug : (int, string) Hashtbl.t;
    (* map bytepos/4 of instr to defname of event *)
  }

val read_executable : string -> executable
(* read this file *)

val decode : executable -> Instruct.instruction array * ISet.t * (int -> int)
(* decode the [code] section and return the instructions and
   the set of labels, and the mapping from code position to label
 *)

val defname_of_label : executable -> (int -> int) -> (int -> string)
(* return the definition name of a label, or raise Not_found if not found
   (e.g. no debug section available)
 *)

val map_label_in_instr : (int -> int) -> Instruct.instruction ->
                         Instruct.instruction

val get_labels_in_instr : Instruct.instruction -> int list
