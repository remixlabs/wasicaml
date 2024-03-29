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
  | Function of { label:int }
  | FuncInEnv of { func_offset:int; env:initvalue array }
  | Block of initvalue array

(* Function is a pointer to the letrec [label].
   FuncInEnv is a function inside an environment. env[func_offset] must
     be a Function.
   Block is a block of values
 *)

let is_function =
  function
  | Function _ -> true
  | _ -> false

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
          printf "%s = letrec_%d\n" prefix fn.label
      | FuncInEnv fenv ->
          Array.iteri
            (fun i iv ->
              recurse (sprintf "%s.env[%d](%d)" prefix i fenv.func_offset) iv
            )
            fenv.env
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
          when fn1.label = fn2.label ->
        v1
    | FuncInEnv fenv1, FuncInEnv fenv2
          when fenv1.func_offset = fenv2.func_offset &&
                 Array.length fenv1.env = Array.length fenv2.env ->
        let env = merge_initvalue_arrays fenv1.env fenv2.env in
        FuncInEnv { fenv1 with env }
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
            Wc_control.dump_block fblock.block 0;
            assert false;
          );
          let merged = merge_stacks stack lstack in
          Hashtbl.replace shape_table label merged
        with
          | Not_found ->
              Hashtbl.add shape_table label stack
      )
      labels in

  let push shape d =
    let nstack = Array.make d Unknown |> Array.to_list in
    { stack = nstack @ shape.stack;
      length = shape.length + d;
      accu = Unknown
    } in

  let pop shape d =
    { stack = delete d shape.stack;
      length = shape.length - d;
      accu = Unknown
    } in

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
            Array.of_list (Function { label } :: Unknown :: env_fields) in
          let n = max (num-1) 0 in
          { accu = FuncInEnv { func_offset=0; env };
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
                   let func_offset = 3 * (num_labs-i-1) in
                   env.(func_offset) <- Function { label };
                   FuncInEnv {func_offset; env}
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
            push shape d
          else if d < 0 then
            pop shape (-d)
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
          | Trap { labels={trylabel; catchlabel}; poplabel=Some pop } ->
              let trap_shape = push shape 4 in
              update_shape_table shape [ catchlabel ];
              update_shape_table trap_shape [ trylabel; pop ];
              trap_shape
          | Trap { labels={trylabel; catchlabel}; poplabel=None } ->
              let trap_shape = push shape 4 in
              update_shape_table shape [ catchlabel ];
              update_shape_table trap_shape [ trylabel ];
              trap_shape
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
(*
  Hashtbl.iter
    (fun i iv ->
      print_initvalue (sprintf "global%d" i) iv
    )
    globals_table;
  flush stdout;
 *)

  globals_table

let derive_glbfun_table globals_table =
  let glbfun_table = Hashtbl.create 7 in
  let rec recurse initvalue =
    match initvalue with
      | Unknown -> ()
      | Block b -> Array.iter recurse b
      | FuncInEnv { env; _ } ->
          Array.iteri
            (fun func_offset env_val ->
              match env_val with
                | Function { label } ->
                    Hashtbl.replace glbfun_table label (func_offset, env)
                | _ -> ()
            )
            env
      | Function _ -> assert false in
  Hashtbl.iter
    (fun glb initvalue ->
      recurse initvalue
    )
    globals_table;
  glbfun_table
