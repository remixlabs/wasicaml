(* Copyright (C) 2021 by Figly, Inc.
   This code is free software, see the file LICENSE for details.
 *)

open Printf
open Wc_types

let trace_stack_max depth instr =
  (* some instructions need temporarily additional stack positions *)
  match instr with
    | I.Kapply num -> if num < 4 then depth+3 else depth
    | Kappterm(num,slots) -> max depth (depth-slots+num)
    | Kpushtrap _ -> depth+4
    | _ -> depth

let trace_stack_instr depth instr =
  match instr with
    | I.Klabel _ -> assert false
    | Krestart -> depth
    | Kconst _ | Kacc _ | Kassign _ | Kenvacc _ | Kgetglobal _ -> depth
    | Kpush -> depth+1
    | Kpop num -> depth-num
    | ( Knegint | Kboolnot | Koffsetint _ | Koffsetref _ | Kisint
      | Ksetglobal _ | Kgetfield _ | Kgetfloatfield _ | Kvectlength
      ) -> depth
    | ( Kaddint | Ksubint | Kmulint | Kdivint | Kmodint
        | Kandint | Korint | Kxorint | Klslint | Klsrint | Kasrint
        | Kintcomp _ | Kisout | Ksetfield _ | Ksetfloatfield _
        | Kgetvectitem | Kgetstringchar | Kgetbyteschar
      ) -> depth-1
    | ( Ksetvectitem | Ksetbyteschar ) -> depth-2
    | ( Kmakeblock(size, _) | Kmakefloatblock size ) -> depth-(max (size-1) 0)
    | Kccall(_, num) -> depth-(num-1)
    | Kbranch _ | Kbranchif _ | Kbranchifnot _ | Kswitch _ -> depth
    | Kpush_retaddr _ -> depth+3
    | Kapply num -> depth - num - (if num < 4 then 0 else 3)
    | Kappterm _ | Kreturn _ | Kraise _ | Kstop -> depth
    | Kgrab _ -> depth
    | Kclosure(_, num) -> depth-(max (num-1) 0)
    | Kclosurerec(funcs, num) ->
        let num_funcs = List.length funcs in
        depth-(max (num-1) 0)+num_funcs
    | Koffsetclosure _ -> depth
    | Kpushtrap _ -> depth+4
    | Kpoptrap -> depth-4
    | Kcheck_signals -> depth
    | Kgetmethod -> depth
    | Kgetpubmet _ -> depth+1
    | Kgetdynmet -> depth
    | Kevent _ -> depth
    | Kstrictbranchif _ -> assert false
    | Kstrictbranchifnot _ -> assert false


let max_stack_depth_of_instrs instrs =
  let len = Array.length instrs in
  let rec recurse mdepth depth k =
    if k < len then
      let mdep = trace_stack_max depth instrs.(k) in
      let depth' = trace_stack_instr depth instrs.(k) in
      recurse (max mdepth (max mdep depth')) depth' (k+1)
    else
      mdepth in
  recurse 0 0 0
   
