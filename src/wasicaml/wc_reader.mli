open Wc_types

type executable =
  { dll_paths : string list;
    dll_names : string list;
    primitives : string array;
    code : string;
    data : Obj.t array;
    symbols : Symtable.global_map;
  }

val read_executable : string -> executable
(* read this file *)

val decode : executable -> Instruct.instruction array * ISet.t
(* decode the [code] section and return the instructions and
   the set of labels
 *)

val map_label_in_instr : (int -> int) -> Instruct.instruction ->
                         Instruct.instruction

val get_labels_in_instr : Instruct.instruction -> int list
