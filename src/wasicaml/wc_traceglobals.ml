(* Copyright (C) 2021 by Figly, Inc.
   This code is free software, see the file LICENSE for details.
 *)

open Printf
open Wc_types
open Wc_control
open Wc_util

(* analyze the initialization code and figure out which globals
   correspond to which global functions
 *)

type initvalue =
  | Unknown
  | Function of { label:int; env_offset:int; env:initvalue array }
  | Block of initvalue array

type shape =
  { stack : initvalue list;
    length : int;
    accu : initvalue
  }

let empty_shape =
  { stack = [];
    length = 0;
    accu = Unknown
  }

let print_initvalue prefix initvalue =
  let rec recurse prefix initvalue =
    match initvalue with
      | Unknown -> ()
      | Function fn ->
          printf "%s = letrec_%d\n" prefix fn.label;
          Array.iteri
            (fun i iv ->
              recurse (sprintf "%s.env[%d]" prefix i) iv
            )
            fn.env
      | Block b ->
          Array.iteri
            (fun i iv ->
              recurse (sprintf "%s[%d]" prefix i) iv
            )
            b in
  recurse prefix initvalue

let rec merge_initvalues v1 v2 =
  match v1, v2 with
    | Function fn1, Function fn2
          when fn1.label = fn2.label &&
                 Array.length fn1.env = Array.length fn2.env ->
        let env = merge_initvalue_arrays fn1.env fn2.env in
        Function { fn1 with env }
    | Block b1, Block b2 when Array.length b1 = Array.length b2 ->
        Block (merge_initvalue_arrays b1 b2)
    | _ ->
        Unknown

and merge_initvalue_arrays a1 a2 =
  Array.mapi
    (fun i bv1 ->
      let bv2 = a2.(i) in
      merge_initvalues bv1 bv2
    )
    a1

let merge_stacks s1 s2 =
  assert(s1.length = s2.length);
  { stack = List.map2 merge_initvalues s1.stack s2.stack;
    length = s1.length;
    accu = merge_initvalues s1.accu s2.accu;
  }

let rec delete n l =
  if n <= 0 then
    l
  else
    match l with
      | x :: l' -> delete (n-1) l'
      | [] -> []

let global_offset ident =
  assert(Ident.global ident);
  let name = Ident.name ident in
  int_of_string name

let trace_globals_of_fblock globals_table fblock =
  let shape_table = Hashtbl.create 7 in
  (* maps label to stack shape of camlstack *)

  let update_shape_table stack labels =
    List.iter
      (fun label ->
        try
          let lstack = Hashtbl.find shape_table label in
          if lstack.length <> stack.length then (
            eprintf "[DEBUG] Bad function: %d\n" fblock.scope.cfg_func_label;
            eprintf "[DEBUG] Bad label: %d\n" label;
            eprintf "[DEBUG] stack.length=%d lstack.length=%d\n"
                    stack.length lstack.length;
            Wc_tracestack.dump fblock.block 0;
            assert false;
          );
          let merged = merge_stacks stack lstack in
          Hashtbl.replace shape_table label merged
        with
          | Not_found ->
              Hashtbl.add shape_table label stack
      )
      labels in

  let trace_instr shape i =
    match i with
      | I.Kacc sp ->
          if sp < shape.length then
            { shape with accu = List.nth shape.stack sp }
          else
            { shape with accu = Unknown }
      | Kpush ->
          { shape with stack = shape.accu :: shape.stack;
                       length = shape.length + 1
          }
      | Kpop n ->
          { shape with stack = delete n shape.stack;
                       length = shape.length - n
          }
      | Kassign sp ->
          let stack =
            List.mapi
              (fun i x -> if i = sp then shape.accu else x)
              shape.stack in
          { shape with stack }
      | Kmakeblock(size, 0) when size > 0 ->
          let block =
            shape.accu :: list_prefix (size-1) shape.stack
            |> Array.of_list
            |> (fun a -> Block a) in
          { stack = delete (size-1) shape.stack;
            length = shape.length - (size-1);
            accu = block
          }
      | Kclosure(label, num) ->
          let env_fields =
            if num=0 then [] else
              shape.accu :: list_prefix (num-1) shape.stack in
          let env =
            Array.of_list (Unknown :: Unknown :: env_fields) in
          let n = max (num-1) 0 in
          { accu = Function { label; env_offset=0; env };
            stack = delete n shape.stack;
            length = shape.length - n
          }
      | Kclosurerec(labs, num) when labs <> [] ->
          let num_labs = List.length labs in
          let env_fields =
            if num=0 then [] else
              shape.accu :: list_prefix (num-1) shape.stack in
          let env_fields_a = Array.of_list env_fields in
          let unknowns = Array.make (2 + 3 * (num_labs-1)) Unknown in
          let env = Array.append unknowns env_fields_a in
          let n = max (num-1) 0 in
          let funcs =
            List.rev labs
            |> List.mapi
                 (fun i label ->
                   let env_offset = 3 * (num_labs-i-1) in
                   Function {label; env_offset; env}
                 ) in
          { stack = funcs @ delete n shape.stack;
            length = shape.length + - n + List.length funcs;
            accu = Unknown
          }
      | Ksetglobal ident ->
          let offset = global_offset ident in
          Hashtbl.replace globals_table offset shape.accu;
          shape
      | _ ->
          let d = Wc_traceinstr.trace_stack_instr 0 i in
          if d > 0 then
            let nstack = Array.make d Unknown |> Array.to_list in
            { stack = nstack @ shape.stack;
              length = shape.length + d;
              accu = Unknown
            }
          else if d < 0 then
            { stack = delete (-d) shape.stack;
              length = shape.length + d;
              accu = Unknown
            }
          else
            { shape with accu = Unknown } in

  let rec recurse block =
    Array.fold_left
      (fun shape instr ->
        match instr with
          | Label label ->
              let stack =
                try Hashtbl.find shape_table label
                with Not_found -> empty_shape in
              stack
          | Simple i ->
              let labels = Wc_tracestack.local_branch_labels i in
              update_shape_table shape labels;
              let stack' = trace_instr shape i in
              stack'
          | Trap { catchlabel; poplabel=Some pop } ->
              update_shape_table shape [ catchlabel; pop ];
              shape
          | Trap { catchlabel; poplabel=None } ->
              update_shape_table shape [ catchlabel ];
              shape
          | TryReturn ->
              shape
          | NextMain _ ->
              shape
          | Block inner ->
              recurse inner
      )
      empty_shape
      block.instructions in
  ignore(recurse fblock.block)

let trace_globals scode =
  let globals_table = Hashtbl.create 7 in
  (* maps global index to initvalue (Unknown not possible) *)

  IMap.iter
    (fun func_label fblock ->
      if fblock.scope.cfg_main then
        trace_globals_of_fblock globals_table fblock
    )
    scode.functions;

  Hashtbl.iter
    (fun i iv ->
      print_initvalue (sprintf "global%d" i) iv
    )
    globals_table;
  flush stdout;

  globals_table

