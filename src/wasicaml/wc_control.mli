(* Copyright (C) 2021 by Figly, Inc.
   This code is free software, see the file LICENSE for details.
 *)

open Wc_types

(* The code is structured as follows:
   - a "letrec" consists of a couple of functions
   - a function has a code block, and there can be "try sections"

  "try sections": Because we have to use a try/catch emulation in Wasm,
  the body of a "try" has to be compiled into a separate Wasm function.
  The body starts after the Kpushtrap and ends just before the corresponding
  Kpoptrap. (NB. This can go away once we have proper exceptions in Wasm.)

  The code is split into basic section, and there are only jumps to the
  beginning of sections, and these jumps must occur at the end of the
  section. This means that the last instruction of a section is either a
  jump to a label, or another type of jump (like Kappterm). In the case
  of a conditional branch, we also keep track of the logical next section
  to execute should the condition not be true.

  In Wasm there are only two type of jumps:
   - A jump back to the beginning of a loop (like "continue" in C)
   - A jump out of a block (like "break" in C)

  As OCaml can only produce well-structured code it is not too complicated
  to put the code into this form.

 *)

type structured_code =
  { functions : func_block IMap.t;  (* by cfg_func_label *)
  }

 and func_block =
   { scope : cfg_scope;
     block : block;
   }

 and block =
   { block_scope : cfg_scope;
     loop_label : int option;
     (* a label pointing to the beginning of this block, for jumping back *)
     instructions : instruction array;
     (* what to execute *)
     break_label : int option;
     (* a label pointing to the end of this block, for jumping out *)
     (* there cannot be both a loop_label and a break_label *)
   }

 and instruction =
   | Block of block
   | Label of int   (* precedes a sequence of Simple instructions *)
   | Simple of I.instruction
   | Trap of full_trap_labels
   | NextMain of int

 and cfg_scope =
  { cfg_letrec_label : int option;
    (* label of first function in "let rec" phrase to which this function
       belongs *)
    cfg_func_label : int;
    (* label of current function, or 0 for the init block *)
    cfg_try_labels : trap_labels list;
    (* surrounding "try" sections, inner to outer *)
    (* cfg_is_pop_label : bool; *)
    (* whether this label is the regular exit of a "try" section, i.e.
       a pop label. Note that the trap_labels have already been removed from
       cfg_try_labels at this point.
     *)
    cfg_main : bool;
    (* this is a main scope *)
  }

 and trap_labels =
   { trylabel: int;
     catchlabel: int;
   }

 and full_trap_labels =
   { labels: trap_labels;
     poplabel: int option
   }

type trap_info =
  | Trap_push of full_trap_labels
  | Trap_pop of trap_labels

(* try/catch:

   outer_1_label:
     Kpushtrap catch_label;
     cfg_trap = Trap_push (try_label, pop_label);
     cfg_succ = [ outer_2_label; catch_label ]
   try_label:
     cfg_try = Try_entry exit_label
     ... finally jumps to pop_label
   pop_label:
     Kpoptrap;
     cfg_trap = Trap_pop(try_label, exit_label)
 *)

type cfg_node =
  { cfg_scope : cfg_scope;
    cfg_node_label : int;
    (* label of this sequence of non-jumping instructions *)
    (* if cfg_node_label = cfg_func_label, this node is the beginning of
       a function/init block *)
    cfg_trap : trap_info option;
    (* whether this is the start of a "try" section *)
    mutable cfg_loops : int list;
    (* this node is member if these loops (inner to outer). Loops are
       identified by the node label of the first node of the loop, which
       is also the node to which it is allowed to jump back *)
    cfg_succ : int list;
    (* Successor nodes *)
    cfg_length : int;
    (* Number of Instruct.instructions (can be 0) *)
    cfg_final : I.instruction option;
    cfg_next_main : int option;
  }

type cfg =
  { mutable nodes : cfg_node IMap.t;  (* by cfg_node_label *)
    mutable code : I.instruction array;
    mutable labels : ISet.t;          (* all labels *)
  }

val create_cfg : Instruct.instruction array -> ISet.t -> cfg
  (* Create the CFG from the instructions and the set of labels
     (as returned by {!Wc_reader.decode}).
   *)

val split_main_function : cfg -> unit
  (* An optional transformation of the CFG: Split the usually very long
     main function (starting at label 0) into a sequence of smaller functions.
     At the end of every such smaller function the cfg_next_main field
     is set to point to the next function (should become a tailcall).
   *)

val recover_structure : cfg -> structured_code
  (* Analyze the loop/jump structure of the code *)

val validate : structured_code -> unit

val string_of_scope : cfg_scope -> string
val dump_block : block -> int -> unit
