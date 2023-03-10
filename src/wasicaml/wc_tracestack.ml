(* Copyright (C) 2021 by Figly, Inc.
   This code is free software, see the file LICENSE for details.
 *)

open Printf
open Wc_types

(* So far, only simple things:
   - figure out the maximum size of the OCaml stack, to get a good value
     for bp
 *)

let local_branch_labels =
  function
  | I.Kbranch l -> [l]
  | Kbranchif l -> [l]
  | Kbranchifnot l -> [l]
  | Kswitch (la1,la2) -> Array.to_list la1 @ Array.to_list la2
  | _ -> []

let max_stack_depth_of_fblock fblock =
  let open Wc_control in
  let depth_table = Hashtbl.create 7 in
  (* maps label to depth of camlstack *)

  let update_depth_table depth labels =
    List.iter
      (fun label ->
        try
          let d = Hashtbl.find depth_table label in
          if d <> depth then (
            eprintf "[DEBUG] Bad function: %d\n" fblock.scope.cfg_func_label;
            eprintf "[DEBUG] Bad label: %d\n" label;
            eprintf "[DEBUG] d=%d depth=%d\n" d depth;
            dump_block fblock.block 0;
            assert false;
          )
        with
          | Not_found ->
              Hashtbl.add depth_table label depth
      )
      labels in

  let rec recurse block =
    Array.fold_left
      (fun (max_depth, depth) instr ->
        match instr with
          | Label label ->
              let depth =
                try Hashtbl.find depth_table label
                with Not_found -> 0 in
              (max max_depth depth, depth)
          | Simple i ->
              let labels = local_branch_labels i in
              update_depth_table depth labels;
              let depth' = Wc_traceinstr.trace_stack_instr depth i in
              (max depth' max_depth, depth')
          | Trap { labels = {trylabel; catchlabel}; poplabel=Some pop } ->
              update_depth_table (depth+4) [ trylabel; pop ];
              update_depth_table depth [ catchlabel ];
              (max_depth, depth)
          | Trap { labels = {trylabel; catchlabel}; poplabel=None } ->
              update_depth_table (depth+4) [ trylabel ];
              update_depth_table depth [ catchlabel ];
              (max_depth, depth)
          | NextMain _ ->
              (max_depth, depth)
          | Block inner ->
              recurse inner
      )
      (0, 0)
      block.instructions in
  recurse fblock.block |> fst
