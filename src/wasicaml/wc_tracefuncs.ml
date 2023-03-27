(* Copyright (C) 2023 by Figly, Inc.
   This code is free software, see the file LICENSE for details.
 *)

open Printf
open Wc_types
open Wc_control
open Wc_util

type funcprops =
  { func_arity : int option;
    (* number of args. If you call with this number of args, no GRAB will
       be done.
     *)

    use_alloc : bool;
    (* whether the function allocates under the assumption that no GRAB
       is done (i.e. it is called with the right number of args)
     *)
    use_env : bool;
    (* whether the function accesses its environment under the assumption
       that no GRAB is done
     *)
  }

let empty_funcprops =
  (* pessimistic assumptions: *)
  { func_arity = None;   (* unknown *)
    use_alloc = true;
    use_env = true
  }

let get_funcprops funcprops_table label =
  try Hashtbl.find funcprops_table label
  with Not_found -> empty_funcprops

let instruction_allocates instr =
  match instr with
    | I.Kmakeblock(size, _) ->
        size > 0
    | Kmakefloatblock size ->
        size > 0
    | Kclosure _ | Kclosurerec _ | Kgetfloatfield _ ->
        true

    | Kccall(name, _) ->
        not (Hashtbl.mem Wc_prims.prims_noalloc name)

    | Kapply _ | Kappterm _ ->
        (* for now assume that the called function allocates - we would
           need the call graph for a better decision *)
        true

    | Kgrab _ ->
        (* intentionally ignored *)
        false

    | Krestart | Kconst _ | Kacc _ | Kassign _ | Kenvacc _ | Kgetglobal _
      | Kpush | Kpop _
      | Knegint | Kboolnot | Koffsetint _ | Koffsetref _ | Kisint
      | Ksetglobal _ | Kgetfield _ | Kvectlength
      | Kaddint | Ksubint | Kmulint | Kdivint | Kmodint
      | Kandint | Korint | Kxorint | Klslint | Klsrint | Kasrint
      | Kintcomp _ | Kisout | Ksetfield _ | Ksetfloatfield _
      | Kgetvectitem | Kgetstringchar | Kgetbyteschar
      | Ksetvectitem | Ksetbyteschar
      | Kbranch _ | Kbranchif _ | Kbranchifnot _ | Kswitch _
      | Kpush_retaddr _ | Koffsetclosure _ | Kreturn _
      | Kpushtrap _ | Kpoptrap | Kraise _
      | Kcheck_signals | Kevent _
      | Kgetmethod | Kgetpubmet _ | Kgetdynmet
      | Kstop

    | Klabel _  | Kstrictbranchif _ | Kstrictbranchifnot _ ->
        false

let instruction_accesses_env instr =
  match instr with
    | I.Kenvacc _ | Koffsetclosure _ ->
        true
    | Kapply _ | Kappterm _ ->
        (* for now assume that we need to pass the env *)
        true
    | _ ->
        false

let trace_funcprops_of_fblock funcprops_table fblock =
  let use_alloc = ref false in
  let use_env = ref false in
  let arity = ref None in
  let rec recurse block =
    Array.iter
      (fun instr ->
        match instr with
          | Simple i ->
              if instruction_allocates i then
                use_alloc := true;
              if instruction_accesses_env i then
                use_env := true;
              ( match i with
                  | I.Kgrab n ->
                      arity := Some (n+1)
                  | _ ->
                      ()
              )
          | Block inner ->
              recurse inner
          | Label _ | Trap _ | NextMain _ ->
              ()
      )
      block.instructions in
  (* There's no Kgrab when:
     - the function only takes 1 arg
     - the function is for a "try" body
   *)
  if fblock.scope.cfg_try_labels = [] then
    arity := Some 1;
  recurse fblock.block;
  let funcprops =
    { func_arity = !arity;
      use_alloc = !use_alloc;
      use_env = !use_env
    } in
  Hashtbl.replace funcprops_table fblock.scope.cfg_func_label funcprops

let trace_funcprops scode =
  let funcprops_table = Hashtbl.create 7 in

  IMap.iter
    (fun func_label fblock ->
      trace_funcprops_of_fblock funcprops_table fblock
    )
    scode.functions;

  funcprops_table
