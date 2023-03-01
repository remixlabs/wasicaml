(* Copyright (C) 2021 by Figly, Inc.
   This code is free software, see the file LICENSE for details.
 *)

(* SPILLING:

   - camlstack: must also represent the args (so that args can go into
     locals)

     new invariant: List.length camlstack = camldepth + arity

     * arity should be set early (now easy as we extract Wgrab)

   - create a new type of locals with RValue representation
     names: "arg%d" and "pos%d". "arg%d" is the local for the
     n-th argument. "pos%d" is the local for the n-th pushed value.

     store: LocalPos, LocalArg

     The invariants for camlstack also apply to the corresponding locals.

      * state: add a new field num_val_locals

   - stack descriptor
      * enhance it
      * ensure that all Winstruction have the stack descriptor
      * do the spilling in setup_for_gc/restore_after_gc
      * ensure we know the max stack size

   - function calls:
      * enhance "straighten" so that arg and val locals are written to
        the stack.

        EXPERIMENT: use spilling instead

   - new function: limit_locals
      * if there are more than N locals, stash some locals away so
        we only have N ones
      * if avoid_locals: N = 0

   - branches

   - Arith operation + PUSH:
      * avoid the accu, but put the result directly into the right
        local
 *)

open Printf
open Wc_types
open Wc_instruct
open Wc_util

type state =
  { camlstack : store list;
    (* where the contents are stored that are supposed to be on the
       OCaml stack - in top to bottom order. This also includes the
       args of the function at the very end.

       If an OCaml stack value is stored in the real stack, it is only
       allowed to store it at its original position or at a lower (newer)
       position. Formally, if element k is
       set to store=RealStack(pos), then pos<=-camldepth+k.

       Also, if a stack value is not stored at the original position,
       there must be no other value stored at the original position (so that we
       can always copy the value to its original position without first
       having to move some other value away).

       List.length camlstack = camldepth + arity

       The first element of camlstack is for stack position -camldepth.
       The last element of camlstack is for stack position arity-1.
     *)
    camldepth : int;
    (* the length of the stack except function arguments *)
    accu : store;
    (* where the accu is stored *)
    realaccu : ISet.t;
    (* in which stack positions the "accu" variable must be saved on flush.
       If additionally accu=RealAccu, the "accu" variable also keeps the
       logical accu value.
     *)
    arity : int option;
    (* number of args pf the current function. This is unknown for
       Wasm functions that are generated for "try" bodies, and so
       arity=None in this case.-
     *)
  }

(* IDEA: there is sometimes code setting the accu, but then the accu
   is not accessed. Add to state:

   accu_used : bool ref option

   which is in particular set by straighten_accu. The ref is set to true
   whenever there is a read access of state.accu.
 *)

type lpad =  (* local pad *)
  { letrec_label : int;

    locals : (string, repr) Hashtbl.t;
    (* IDEA: maybe switch to (string, repr * bool ref) Hashtbl.t. The bool
       is set to true when the local is actually used.
     *)

    mutable avoid_locals : bool;
    (* try not to allocate locals in this function *)

    mutable loops : ISet.t;
    (* which labels are loop labels *)

    indegree : (int, int) Hashtbl.t;
    (* how many nodes jump to this node (identified by label). A negative
       indegree means that no optimizations must be done for this label.
     *)

    state_table : (int, state) Hashtbl.t;
    (* the start state by label *)

    mutable globals_table : (int, Wc_traceglobals.initvalue) Hashtbl.t;
    (* knowledge about the globals *)

    mutable environment : Wc_traceglobals.initvalue array;
    (* knowledge about the environment of this function - can be the empty
       array if nothing is known
     *)

    mutable func_offset : int;
    (* knowledge about the environment: this function is at position
       func_offset in the environment (only when environment <> [])
     *)

    enable_returncall : bool;
    (* whether we are allowed to emit tail calls *)
  }

let real_accu =
  RealAccu { no_function = false }

let real_accu_no_func =
  RealAccu { no_function = true }

let empty_state =
  { camlstack = [];
    camldepth = 0;
    accu = Invalid;
    realaccu = ISet.empty;
    arity = None;
  }

let empty_lpad ~enable_returncall letrec_label =
  { locals = Hashtbl.create 7;
    avoid_locals = false;
    loops = ISet.empty;
    indegree = Hashtbl.create 7;
    state_table = Hashtbl.create 7;
    globals_table = Hashtbl.create 1;
    environment = [| |];
    func_offset = 0;
    enable_returncall;
    letrec_label;
  }

let string_of_state state =
  let camlstack =
    List.map string_of_store state.camlstack
    |> String.concat ", " in
  let realaccu =
    ISet.elements state.realaccu
    |> List.map string_of_int
    |> String.concat ", " in
  sprintf "{camldepth=%d; arity=%s; camlstack=[%s]; accu=%s; realaccu={%s}"
          state.camldepth
          (match state.arity with None -> "n/a" | Some n -> string_of_int n)
          camlstack
          (string_of_store state.accu)
          realaccu

let new_local lpad repr =
  let k = Hashtbl.length lpad.locals in
  let s = sprintf "x%d" k in
  Hashtbl.add lpad.locals s repr;
  s

let stack_descr state =
  let cd = state.camldepth in
  let rd = ref 0 in
  let stack = Array.make cd false in
  List.iter
    (fun st ->
      match st with
        | RealStack pos ->
            assert(pos >= (-cd));
            if pos < 0 then (
              stack.(pos + cd) <- true;
              rd := max !rd (-pos)
            )
        | _ ->
            ()
    )
    state.camlstack;
  let uninit = ref [] in
  for i = Array.length stack - 1 downto 0 do
    let used = stack.(i) in
    if not used && i >= cd - !rd then
      uninit := (-cd+i) :: !uninit;
  done;
  let stack_save_accu =
    state.realaccu <> ISet.empty ||
      ( match state.accu with RealAccu _ -> true | _ -> false) in
  { stack_uninit = !uninit;
    stack_depth = !rd;
    stack_save_accu
  }

let set_camlstack pos store state =
  let cd = state.camldepth in
  if pos >= (-cd) && pos <= (-1) then
    let camlstack =
      List.mapi
        (fun i old_store ->
          if (-cd+i) = pos then store else old_store
        )
        state.camlstack in
    camlstack
  else
    (* Cannot set pos >= 0 - these are the function args *)
    state.camlstack

let pop_camlstack state =
  let cd = state.camldepth in
  match state.camlstack with
    | RealStack pos :: tl ->
        { state with
          camlstack = tl;
          camldepth = cd - 1;
        }
    | (RealAccu _):: tl ->
        let cpos = (-cd) in
        { state with
          camlstack = tl;
          camldepth = cd - 1;
          realaccu = ISet.remove cpos state.realaccu;
        }
    | (Const _ | Local _ | Atom _ | TracedGlobal _ | Invalid) :: tl ->
        { state with camlstack = tl; camldepth = cd - 1 }
    | [] ->
        assert false

let rec popn_camlstack number state =
  if number = 0 then
    state
  else
    popn_camlstack (number-1) (pop_camlstack state)

let push_camlstack store state =
  let cd = state.camldepth in
  match store with
    | RealStack pos ->
        { state with
          camlstack = store :: state.camlstack;
          camldepth = cd + 1;
        }
    | RealAccu _ ->
        let cpos = (-cd-1) in
        { state with
          camlstack = store :: state.camlstack;
          camldepth = cd + 1;
          realaccu = ISet.add cpos state.realaccu;
        }
    | (Const _ | Local _ | Atom _ | TracedGlobal _) ->
        { state with
          camlstack = store :: state.camlstack;
          camldepth = cd + 1;
        }
    | Invalid ->
        assert false

let flush_accu lpad state =
  (* If the accu is not yet saved, do this now. This function needs to be called
     before setting the accu to a new value. *)
  let instrs_rev =
    ISet.fold
      (fun pos instr_acc ->
        let instr =
          Wcopy { src=real_accu; dest=RealStack pos } in
        instr :: instr_acc
      )
      state.realaccu
      [] in
  let instrs =
    List.rev instrs_rev in
  let cd = state.camldepth in
  let camlstack =
    List.mapi
      (fun i old ->
        let pos = (-cd+i) in
        if ISet.mem pos state.realaccu then (
          RealStack pos
        ) else
          old
      )
      state.camlstack in
  let state =
    { state with
      camlstack;
      realaccu = ISet.empty;
    } in
  (state, instrs)

let straighten_accu lpad state =
  (* if the accu is stored elsewhere, load it now into "accu". This function
     needs to be called before doing something with the value in the accu
   *)
  let state, instrs_flush = flush_accu lpad state in
  match state.accu with
    | Invalid ->
        (state, [])
    | RealAccu _ ->
        (state, instrs_flush)
    | store ->
        let instr = Wcopy { src=store; dest=real_accu } in
        let state = { state with accu=real_accu } in
        (state, instrs_flush @ [instr])

let straighten_accu_when_on_stack lpad state =
  (* Load the accu if it is stored in the real stack. This is needed before
     renaming the accu into another stack position, in order to enforece
     the "no swapping of stack positions" rule.
   *)
  match state.accu with
    | (RealStack _) ->
        straighten_accu lpad state
    | _ ->
        (state, [])

let straighten_accu_for_branch lpad state =
  match state.accu with
    | RealStack _ | Const _ | Atom _ | TracedGlobal _ ->
        straighten_accu lpad state
    | Local _ | RealAccu _ | Invalid ->
        (state, [])

let pop_real_stack lpad state num =
  (* Ensure that values stored at the num bottom positions of the stack
     are saved to their real stores, in preparation of popping these
     positions. This can only be the accu.
   *)
  let cd = state.camldepth in
  match state.accu with
    | (RealStack pos) when pos >= (-cd) && pos <= (-cd+num-1) ->
        straighten_accu lpad state
        (* NB. this doesn't pop from camlstack *)
    | _ ->
        (state, [])

let flush_real_stack_only_accu_at lpad state pos =
  match state.accu with
    | (RealStack p) when p = pos ->
        straighten_accu lpad state
     | _ ->
        (state, [])

let patch state positions =
  (* Fix up camlstack and set the positions to RealStack *)
  let rec recurse camlstack pos patches =
    match patches with
      | [] -> camlstack
      | next_patch :: patches' ->
          if pos = next_patch then
            match camlstack with
              | _ :: tl ->
                  RealStack pos :: (recurse tl (pos+1) patches')
              | [] -> assert false
          else
            match camlstack with
              | hd :: tl -> hd :: recurse tl (pos+1) patches
              | [] -> assert false in
  let cd = state.camldepth in
  let camlstack =
    recurse state.camlstack (-cd) (ISet.elements positions) in
  (* NB. exploiting that ISet.elements returns the elements in ascending
     order *)
  { state with camlstack }

let flush_real_stack_at lpad state pos =
  (* Ensure that any value at the stack position pos is saved to
     its original position
   *)
  let cd = state.camldepth in
  let state, instrs1 =
    flush_real_stack_only_accu_at lpad state pos in
  let _, positions =
    List.fold_left
      (fun (q, acc) store ->
        match store with
          | RealStack p when p = pos && q <> p ->
              assert(p > q);
              let acc' = ISet.add q acc in
              (q+1, acc')
          | _ ->
              (q+1, acc)
      )
      (-cd, ISet.empty)
      state.camlstack in
  let instrs2 =
    positions
    |> ISet.elements
    |> List.map (fun q -> Wcopy { src=RealStack pos; dest=RealStack q }) in
  let state =
    patch state positions in
  (state, instrs1 @ instrs2)

(*
let flush_real_stack_intval_at lpad state pos =
  (* additionally assumes that the stack contains an integer value at pos *)
  let cd = state.camldepth in
  let state, instrs1 =
    flush_real_stack_only_accu_at lpad state pos in
  let local = new_local lpad RIntVal in
  let _, positions =
    List.fold_left
      (fun (q, acc) store ->
        match store with
          | RealStack p when p = pos && q <> p ->
              assert(p > q);
              let acc' = ISet.add q acc in
              (q+1, acc')
          | _ ->
              (q+1, acc)
      )
      (-cd, ISet.empty)
      state.camlstack in
  let src = RealStack pos in
  let instrs2 =
    [ Wcopy { src; dest=Local(RIntVal, local)} ] in
  let camlstack =
    List.mapi
      (fun i old ->
        let q = -cd + i in
        if ISet.mem q positions then
          Local(RIntVal, local)
        else
          old
      )
      state.camlstack in
  let state =
    { state with camlstack } in
  (state, instrs1 @ instrs2)
 *)

let straighten_stack_at lpad state pos =
  (* ensure that the caml stack for pos is set to the real stack *)
  if pos >= 0 then
    (state, [ Wcomment (sprintf "****** STRANGE CASE: straighten_stack_at pos=%d *****" pos) ])
  else
    let k = pos + state.camldepth in
    let store = List.nth state.camlstack k in
    match store with
      | RealAccu _ ->
          assert(ISet.mem pos state.realaccu);
          let state, instrs = flush_accu lpad state in
          (state, instrs)
      | RealStack p when p = pos ->
          (state, [])
      | _ ->
          assert(not (List.mem (RealStack pos) state.camlstack));
          let instrs = [ Wcopy { src=store; dest=RealStack pos } ] in
          let state =
            { state with
              camlstack = set_camlstack pos (RealStack pos) state;
            } in
          (state, instrs)

let straighten_stack_multi lpad state pos_list =
  (* TODO: this can be optimized by doing it in one go *)
  let state, rev_acc =
    List.fold_left
      (fun (state, rev_acc) pos ->
        let (state, instrs) = straighten_stack_at lpad state pos in
        (state, List.rev_append instrs rev_acc)
      )
      (state, [])
      pos_list in
  (state, List.rev rev_acc)

let accu_is_realaccu state =
  match state.accu with
    | RealAccu _ -> true
    | _ -> false

let accu_is_realaccu_or_invalid state =
  match state.accu with
    | RealAccu _ | Invalid -> true
    | _ -> false

let straighten_stack lpad state =
  let state, instrs1 = flush_accu lpad state in
  let cd = state.camldepth in
  let pos_list =
    state.camlstack
    |> List.mapi (fun i store -> (i,store))
    |> List.concat_map
         (fun (i,store) ->
           match store with
             | RealStack p when p <> -cd+i ->
                 [-cd+i]
             | _ ->
                 [-cd+i]
         ) in
  let state, instrs2 = straighten_stack_multi lpad state pos_list in
  (state, instrs1 @ instrs2)

let straighten_all lpad state =
  let state, instrs1 = straighten_accu lpad state in
  assert(accu_is_realaccu_or_invalid state && state.realaccu = ISet.empty);
  let state, instrs2 = straighten_stack lpad state in
  (state, instrs1 @ instrs2)

let spill_for_apply lpad state =
  (* TODO: once there are RValue locals, these need to be moved to the
     stack; and we need an "unspill" counterpart *)
  let sdescr = stack_descr state in
  let instrs =
    List.map
      (fun pos ->
        Wcopy { src = Const 0; dest = RealStack pos }
      )
      sdescr.stack_uninit in
  (instrs, sdescr.stack_depth)

let localize_accu lpad state repr =
  match state.accu with
    | RealStack _ | RealAccu _ ->
        let local = new_local lpad repr in
        let lstore = Local(repr, local) in
        let instrs = [ Wcopy { src=state.accu; dest=lstore } ] in
        let cd = state.camldepth in
        let positions =
          List.mapi
            (fun i st -> if st = state.accu then i else (-1))
            state.camlstack
          |> List.filter (fun i -> i >= 0)
          |> List.map (fun i -> -cd + i) in
        let state =
          List.fold_left
            (fun state pos ->
              { state with camlstack = set_camlstack pos lstore state }
            )
            state
            positions in
        let state = { state with accu = lstore } in
        (state, instrs)
    | _ ->
        (state, [])

let unary_operation ?(no_function=false) lpad state op_repr op_code =
  let src1 = state.accu in
  let op_repr =
    if lpad.avoid_locals then RValue else op_repr in
  match op_repr with
    | RValue ->
        (* result goes into accu *)
        let state, instrs_flush = flush_accu lpad state in
        let state = { state with accu = real_accu } in
        let instrs_op = [ Wunary { op=op_code; src1; dest=real_accu }] in
        (state, instrs_flush @ instrs_op)
    | _ ->
        let dest_name = new_local lpad op_repr in
        let dest = Local(op_repr, dest_name) in
        let state = { state with accu = dest } in
        let instrs_op = [ Wunary { op=op_code; src1; dest }] in
        (state, instrs_op)

let unary_effect lpad state op_code =
  let src1 = state.accu in
  let state = { state with accu = Const 0 } in
  let instrs_op = [ Wunaryeffect { op=op_code; src1 }] in
  (state, instrs_op)

let binary_operation ?(no_function=false) lpad state op_repr op_code =
  let src1 = state.accu in
  let src2 = List.hd state.camlstack in
  let op_repr =
    if lpad.avoid_locals then RValue else op_repr in
  match op_repr with
    | RValue ->
        (* result goes into accu *)
        let state, instrs_flush = flush_accu lpad state in
        let state =
          { state with accu = real_accu }
          |> pop_camlstack in
        let instrs_op = [ Wbinary { op=op_code; src1; src2; dest=real_accu }] in
        (state, instrs_flush @ instrs_op)
    | _ ->
        let dest_name = new_local lpad op_repr in
        let dest = Local(op_repr, dest_name) in
        let state =
          { state with accu = dest }
          |> pop_camlstack in
        let instrs_op = [ Wbinary { op=op_code; src1; src2; dest }] in
        (state, instrs_op)

let binary_effect lpad state op_code =
  let src1 = state.accu in
  let src2 = List.hd state.camlstack in
  let state = { state with accu = Const 0 } |> pop_camlstack in
  let instrs_op = [ Wbinaryeffect { op=op_code; src1; src2 }] in
  (state, instrs_op)

let ternary_effect lpad state op_code =
  let src1 = state.accu in
  let src2 = List.hd state.camlstack in
  let src3 = List.hd (List.tl state.camlstack) in
  let state = { state with accu = Const 0 } |> pop_camlstack |> pop_camlstack in
  let instrs_op = [ Wternaryeffect { op=op_code; src1; src2; src3 }] in
  (state, instrs_op)

let global_offset ident =
  assert(Ident.global ident);
  let name = Ident.name ident in
  int_of_string name

let make_label lpad label =
  if ISet.mem label lpad.loops then
    Loop label
  else
    Label label

let validate state =
  ( match state.arity with
      | None -> ()
      | Some n ->
          assert(List.length state.camlstack = state.camldepth + n)
  );
  let d = state.camldepth in
  List.iteri
    (fun i st ->
      match st with
        | RealStack pos -> assert(pos = (-d+i))
        | _ -> assert false
    )
    state.camlstack;
  assert(match state.accu with
           | RealAccu { no_function = true } -> false
           | RealAccu _ | Local _ | Invalid -> true
           | _ -> false
        );
  assert(state.realaccu = ISet.empty)

let update_state_table lpad state label =
  try
    let s = Hashtbl.find lpad.state_table label in
    assert(s.camldepth = state.camldepth)
  with
    | Not_found ->
        Hashtbl.add lpad.state_table label state

let norm_accu state =
  match state.accu with
    | RealAccu { no_function=true } ->
        { state with accu = real_accu }
    | _ ->
        state

let branch lpad state label =
  let state = norm_accu state in
  let instr_br = Wbranch { label=make_label lpad label } in
  try
    let dest_state = Hashtbl.find lpad.state_table label in
    validate dest_state;
    let (state, instrs1) = straighten_accu_for_branch lpad state in
    let (state, instrs2) = straighten_stack lpad state in
    validate state;
    assert(state.camldepth = dest_state.camldepth);
    (* state and dest_state can at most differ in accu *)
    let instrs =
      if state.accu = dest_state.accu || dest_state.accu = Invalid then
        instrs1 @ instrs2 @ [ instr_br ]
      else
        instrs1
        @ instrs2
        @ [ Wcopy { src=state.accu; dest=dest_state.accu };
            instr_br
          ] in
    (state, instrs)
  with
    | Not_found -> (* from Hashtbl.find *)
        let n = try Hashtbl.find lpad.indegree label with Not_found -> 0 in
        if n < 0 then (
          let state, instrs1 = straighten_all lpad state in
          let state =
            if n = (-2) then (* it's a loop *)
              { state with accu = Invalid }
            else
              state in
          Hashtbl.add lpad.state_table label state;
          (state, instrs1 @ [ instr_br ])
        ) else if n <= 1 then (
          let (state, instrs1) = straighten_accu_for_branch lpad state in
          Hashtbl.add lpad.state_table label state;
          (state, instrs1 @ [ instr_br ])
        ) else (
          let (state, instrs1) = straighten_accu_for_branch lpad state in
          let (state, instrs2) = straighten_stack lpad state in
          Hashtbl.add lpad.state_table label state;
          (state, instrs1 @ instrs2 @ [ instr_br ])
        )

let transl_instr lpad state instr =
  match instr with
    | I.Klabel _ -> assert false
    | Kconst (Lambda.Const_base (Asttypes.Const_int k)) ->
        ( { state with accu = Const k }, [])
    | Kconst _ ->
        assert false
    | Kacc sp ->
        (*
        ( match state.arity with
            | Some n ->
                if sp < 0 || sp >= state.camldepth + n then (
                  eprintf "[DEBUG] Assertion failed in letrec%d, sp=%d\n" lpad.letrec_label sp;
                  eprintf "[DEBUG] state=%s\n%!" (string_of_state state);
                  assert false;
                )
            | None -> ()
        );
         *)
        let state =
         (*  match state.arity with
            | Some _ ->
                { state with accu = List.nth state.camlstack sp }
            | None -> *)
                if sp < state.camldepth then
                  { state with accu = List.nth state.camlstack sp }
                else
                  { state with accu = RealStack (-state.camldepth + sp) } in
        (state, [])
    | Kpush ->
        let state = push_camlstack state.accu state in
        (state, [ Wcomment (sprintf "(depth=%d)" state.camldepth) ])
    | Kpush_retaddr lab ->
        let state = push_camlstack (Const 0) state in
        let state = push_camlstack (Const 0) state in
        let state = push_camlstack (Const 0) state in
        (state, [])
    | Kpop num ->
        let state, instrs = pop_real_stack lpad state num in
        let state = popn_camlstack num state in
        (state, instrs @ [ Wcomment (sprintf "(depth=%d)" state.camldepth) ])
    | Kassign sp ->
        let cd = state.camldepth in
        let state, instrs_flush =
          flush_real_stack_at lpad state (-cd+sp) in
        let state =
          { state with
            camlstack = set_camlstack (-cd+sp) (RealStack (-cd+sp)) state
          } in
        let instrs_copy =
          [ Wcopy { src=state.accu; dest=RealStack (-cd+sp) } ] in
        (state, instrs_flush @ instrs_copy)
    | Kenvacc field ->
        let state, instrs_flush = flush_accu lpad state in
        let real_field = lpad.func_offset + field in
        if real_field < Array.length lpad.environment &&
             lpad.environment.(real_field) <> Unknown then (
          let efield = lpad.environment.(real_field) in
          let state = { state with
                        accu = TracedGlobal(Env 0,
                                            [field],
                                            efield)
                      } in
          (state, instrs_flush)
        ) else
            let accu = real_accu in
            let state = { state with accu } in
            let instrs_op = [ Wenv { field } ] in
            (state, instrs_flush @ instrs_op)
    | Kgetglobal ident ->
        let offset = global_offset ident in
        assert(offset >= 0);
        let state, instrs_flush = flush_accu lpad state in
        ( match Hashtbl.find lpad.globals_table offset with
            | initvalue ->
                let accu = TracedGlobal(Glb offset, [], initvalue) in
                let state = { state with accu } in
                (state, instrs_flush)
            | exception Not_found ->
                let accu = real_accu in
                let state = { state with accu } in
                let instrs_op = [ Wgetglobal { src=Global offset } ] in
                (state, instrs_flush @ instrs_op)
        )
    | Knegint ->
        unary_operation lpad state RIntVal Pnegint
    | Kboolnot ->
        unary_operation lpad state RIntVal Pboolnot
    | Koffsetint offset ->
        (* localize_accu is beneficial for "for" loops *)
        let (state, instrs1) =
          if lpad.avoid_locals then (state, []) else
            localize_accu lpad state RIntVal in
        let (state, instrs2) =
          unary_operation lpad state RIntVal (Poffsetint offset) in
        (state, instrs1 @ instrs2)
    | Koffsetref offset ->
        unary_effect lpad state (Poffsetref offset)
    | Kisint ->
        unary_operation lpad state RInt Pisint
    | Ksetglobal ident ->
        let offset = global_offset ident in
        unary_effect lpad state (Psetglobal (Global offset))
    | Kgetfield field ->
        ( match state.accu with
            | TracedGlobal(glb, path, initvalue) ->
                let new_path = path @ [field] in
                let new_initvalue =
                  match initvalue with
                    | Block b when field < Array.length b ->
                        b.(field)
                    | _ ->
                        Unknown in
                let accu = TracedGlobal(glb, new_path, new_initvalue) in
                let state = { state with
                              accu;
                            } in
                (state, [])
            | _ ->
                unary_operation lpad state RValue (Pgetfield field)
        )
    | Kvectlength ->
        unary_operation lpad state RIntVal Pvectlength
    | Kgetpubmet k ->
        let state = push_camlstack state.accu state in
        unary_operation lpad state RValue (Pgetpubmet k)
    | Kaddint ->
        binary_operation lpad state RIntVal Paddint
    | Ksubint ->
        binary_operation lpad state RIntVal Psubint
    | Kmulint ->
        binary_operation lpad state RIntUnclean Pmulint
    | Kdivint ->
        binary_operation lpad state RInt Pdivint
    | Kmodint ->
        binary_operation lpad state RInt Pmodint
    | Kandint ->
        binary_operation lpad state RIntVal Pandint
    | Korint ->
        binary_operation lpad state RIntVal Porint
    | Kxorint ->
        binary_operation lpad state RIntVal Pxorint
    | Klslint ->
        binary_operation lpad state RIntVal Plslint
    | Klsrint ->
        binary_operation lpad state RIntVal Plsrint
    | Kasrint ->
        binary_operation lpad state RIntVal Pasrint
    | Kintcomp cmp ->
        binary_operation lpad state RInt (Pintcomp cmp)
    | Kisout ->
        binary_operation lpad state RInt (Puintcomp Clt)
    | Ksetfield field ->
        binary_effect lpad state (Psetfield field)
    | Ksetfloatfield field ->
        binary_effect lpad state (Psetfloatfield field)
    | Kgetvectitem ->
        binary_operation lpad state RValue Pgetvectitem
    | Kgetstringchar ->
        binary_operation lpad state RInt Pgetstringchar
    | Kgetbyteschar ->
        binary_operation lpad state RInt Pgetbyteschar
    | Kgetmethod ->
        let state = push_camlstack (List.hd state.camlstack) state in
        binary_operation lpad state RValue Pgetmethod
    | Kgetdynmet ->
        let state = push_camlstack (List.hd state.camlstack) state in
        binary_operation lpad state RValue Pgetdynmet
    | Ksetvectitem ->
        ternary_effect lpad state Psetvectitem
    | Ksetbyteschar ->
        ternary_effect lpad state Psetbyteschar
    | Kgetfloatfield field ->
        (* do the allocation right here and don't postpone it - we are not
           yet prepared for doing allocations on demand later
         *)
        let src1 = state.accu in
        let state, instrs_flush = flush_accu lpad state in
        let temp_name = new_local lpad RFloat in
        let temp = Local(RFloat, temp_name) in
        let descr = stack_descr state in
        let accu = real_accu_no_func in
        let instrs =
          instrs_flush
          @ [ Wunary { op=Pgetfloatfield field; src1; dest=temp; };
              Walloc { src=temp; dest=accu; descr }
            ] in
        let state = { state with accu } in
        (state, instrs)
    | Kmakeblock(0, tag) ->
        let state = { state with accu = Atom tag } in
        (state, [])
    | Kmakeblock(size, tag) ->
        let state, instrs_flush = flush_accu lpad state in
        let src1 = state.accu in
        let src = src1 :: list_prefix (size-1) state.camlstack in
        let descr = stack_descr state in
        let accu = real_accu_no_func in
        let instrs =
          instrs_flush
          @ [ Wmakeblock { tag; src; descr } ] in
        let state =
          { state with accu}
          |> popn_camlstack (size-1) in
        (state, instrs)
    | Kmakefloatblock size ->
        assert(size > 0);
        let state, instrs_flush = flush_accu lpad state in
        let src1 = state.accu in
        let src = src1 :: list_prefix (size-1) state.camlstack in
        let descr = stack_descr state in
        let instrs =
          instrs_flush
          @ [ Wmakefloatblock { src; descr } ] in
        let accu = real_accu_no_func in
        let state =
          { state with accu }
          |> popn_camlstack (size-1) in
        (state, instrs)
    | Kccall (name, num) when num <= 5 ->
        let state, instrs_flush = flush_accu lpad state in
        let src1 = state.accu in
        let src = src1 :: list_prefix (num-1) state.camlstack in
        let descr = stack_descr state in
        let instrs =
          instrs_flush
          @ [ Wccall { name; src; descr } ] in
        let no_function = Hashtbl.mem Wc_prims.prims_non_func_result name in
        let state =
          { state with accu = RealAccu { no_function }}
          |> popn_camlstack (num-1) in
        (state, instrs)
    | Kccall (name, num) ->
        let state, instrs_str = straighten_all lpad state in
        let descr = stack_descr state in
        let depth = state.camldepth+1 in
        let instrs =
          instrs_str
          @ [ Wcopy { src=real_accu; dest=RealStack(-depth) };
              Wccall_vector { name; numargs=num; depth; descr }] in
        let no_function = Hashtbl.mem Wc_prims.prims_non_func_result name in
        let state =
          { state with accu = RealAccu { no_function }}
          |> popn_camlstack (num-1) in
        (state, instrs)
    | Kbranch label ->
        branch lpad state label
    | Kbranchif label ->
        let (br_state, instrs) = branch lpad state label in
        (state, [ Wif { src=state.accu; neg=false; body=instrs }])
    | Kbranchifnot label ->
        let (br_state, instrs) = branch lpad state label in
        (state, [ Wif { src=state.accu; neg=true; body=instrs }])
    | Kswitch (labels_int, labels_blk) ->
        let state = norm_accu state in
        let state, instrs_str = straighten_all lpad state in
        validate state;
        Array.iter (update_state_table lpad state) labels_int;
        Array.iter (update_state_table lpad state) labels_blk;
        let labels_int = Array.map (make_label lpad) labels_int in
        let labels_blk = Array.map (make_label lpad) labels_blk in
        let src = state.accu in
        (state, instrs_str @ [ Wswitch{labels_int; labels_blk; src} ])
    | Kapply num when num <= 3 ->
        (* In the num <= 3 case, there is no reserved stack position for
           saving env yet. Because of this, we cannot use "straighten"
           to put the args onto the stack - they would end up on the
           wrong positions. Instead, copy the args with crafted
           Wcopy instructions to the right positions, and leave a gap
           of one position for env.
         *)
        let direct_opt = extract_directly_callable_function state.accu in
        let state =
          match direct_opt with
            | Some _ ->
                (* because Wapply_direct loads the function into accu: *)
                { state with accu = real_accu }
            | None -> state in
        let state, instrs_accu = straighten_accu lpad state in
        let instrs_spill, actual_depth = spill_for_apply lpad state in
        let delta = 1 in  (* make room for one additional position (env) *)
        let instrs_move =
          enum 0 num
          |> List.map
               (fun k ->
                 let src = List.nth state.camlstack k in
                 Wcopy { src; dest=RealStack(-actual_depth - num + k - delta) }
               ) in
        let depth = actual_depth+num+delta in
        let instr_apply =
          match direct_opt with
            | Some (global, path, funlabel, _) ->
                Wapply_direct { global; path; funlabel; numargs=num; depth }
            | None ->
                Wapply { numargs=num; depth } in
        let instrs =
          instrs_accu
          @ instrs_spill
          @ instrs_move
          @ [ (* Wcopy { src=Const 0; dest=RealStack(-actual_depth-1) };
                 -- the position for env - it is initialized by Wapply anyway
               *)
              instr_apply
            ] in
        let state = state |> popn_camlstack num in
        (state, instrs)
    | Kapply num ->
        (* In the num > 3 case, there was already a Kpush_retaddr that
           made space for 3 additional values on the stack. Note that we
           don't need 3 values but just one, so the other 2 positions are
           unused. - We can use "straighten" here to ensure that the
           args go to right stack positions. - Also note that the same
           code as far num <= 3 would work as well, though I don't expect
           that it saves instructions. (This might change when we pass
           args to functions via wasm parameters, and not via the stack.)
         *)
        let direct_opt = extract_directly_callable_function state.accu in
        let state =
          match direct_opt with
            | Some _ ->
                (* because Wapply_direct loads the function into accu: *)
                { state with accu = real_accu }
            | None -> state in
        let state, instrs_str = straighten_all lpad state in
        let depth = state.camldepth in
        let instr_apply =
          match direct_opt with
            | Some (global, path, funlabel, _) ->
                Wapply_direct { global; path; funlabel; numargs=num; depth }
            | None ->
                Wapply { numargs=num; depth } in
        let instrs =
          instrs_str
          @ [ instr_apply ] in
        let state = state |> popn_camlstack (num+3) in
        (state, instrs)
    | Kappterm(num, slots) when num <= 10 && lpad.enable_returncall ->
        (* Here we pick the args of the function call up where they are
           (register, accu, stack) - in contrast to Wappterm which assumes
           that the args are already on the stack. We only have to ensure
           that boxed numbers are already in the boxed representaion and
           not in registers (allocation is not possible when Wappterm_args
           is executed because the stack is not proper).
         *)
        let direct_opt =
          extract_directly_callable_function state.accu in
        let camlstack = Array.of_list state.camlstack in
        let alloc_positions =
          enum 0 num
          |> List.filter (fun k ->
                 camlstack.(k) |> repr_of_store |> repr_needs_alloc
               )
          |> List.map (fun k -> k - state.camldepth) in
        let state, instrs_stack =
          straighten_stack_multi lpad state alloc_positions in
        let instr_appterm =
          match direct_opt with
            | Some (global, path, funlabel, _) ->
                Wappterm_direct_args { global; path; funlabel;
                                       funsrc=state.accu;
                                       argsrc=list_prefix num state.camlstack;
                                       oldnumargs=(slots - state.camldepth);
                                       depth=state.camldepth
                                     }
            | None ->
                Wappterm_args { funsrc=state.accu;
                                argsrc=list_prefix num state.camlstack;
                                oldnumargs=(slots - state.camldepth);
                                depth=state.camldepth
                              } in
        let instrs =
          instrs_stack @ [ instr_appterm ] in
        (state, instrs)
    | Kappterm(num, slots) ->
        let direct_opt =
          extract_directly_callable_function state.accu
          |> (function
              | Some info when lpad.enable_returncall -> Some info
              | Some _ -> None
              | None -> None
             ) in
        let state =
          match direct_opt with
            | Some _ ->
                (* because Wappterm_direct loads the function into accu: *)
                { state with accu = real_accu }
            | None -> state in
        let state, instrs_accu =
          straighten_accu lpad state in
        let state, instrs_stack =
          straighten_stack_multi lpad state (enum (-state.camldepth) num) in
        let instr_appterm =
          match direct_opt with
            | Some (global, path, funlabel, _) ->
                Wappterm_direct { global; path; funlabel; numargs=num;
                                  oldnumargs=(slots - state.camldepth);
                                  depth=state.camldepth
                                }
            | None ->
                Wappterm { numargs=num; oldnumargs=(slots - state.camldepth);
                           depth=state.camldepth
                         } in
        let instrs =
          instrs_accu
          @ instrs_stack
          @ [ instr_appterm ] in
        (state, instrs)
    | Kreturn slots ->
        ( match state.arity with
            | Some n ->
                if state.camldepth + n <> slots then (
                  eprintf "[DEBUG] Assertion failed in letrec%d: Kreturn, arity=%d slots=%d\n" lpad.letrec_label n slots;
                  assert false;
                );
                (state, [ Wreturn { src=state.accu; arity=n } ])
            | None ->
                (* Kreturn not allowed in "try" body *)
                eprintf "[DEBUG] Assertion failed in letrec%d: Kreturn in try body\n" lpad.letrec_label;
                assert false
        )
    | Kgrab num ->
        ( match state.arity with
            | Some n ->
                if n <> num+1 then (
                  eprintf "[DEBUG] Assertion failed in letrec%d: Kgrab, arity=%d num=%d\n" lpad.letrec_label n num;
                  assert false;
                );
                (state, [ Wcomment (sprintf "omitted Kgrab %d" num) ])
            | None ->
                eprintf "[DEBUG] Assertion failed in letrec%d: Kgrab in try body\n" lpad.letrec_label;
                assert false;
        )
    | Kclosure(lab, num) ->
        let state, instrs_flush = flush_accu lpad state in
        let src =
          if num = 0 then
            []
          else
            state.accu :: list_prefix (num-1) state.camlstack in
        let descr = stack_descr state in
        let accu = real_accu in
        let instrs =
          instrs_flush
          @ [ Wclosurerec { src; dest=[ accu, lab ]; descr }] in
        let state =
          { state with accu }
          |> popn_camlstack (max (num-1) 0) in
        (state, instrs)
    | Kclosurerec(funcs, num) ->
        let state, instrs_flush = flush_accu lpad state in
        (* flush_accu because we allow the accu to be used *)
        let descr = stack_descr state in
        let num_stack = max (num-1) 0 in
        let src =
          if num = 0 then
            []
          else
            state.accu :: list_prefix num_stack state.camlstack in
        let state = state |> popn_camlstack num_stack in
        let start_dest = -state.camldepth-1 in
        let dest =
          List.mapi
            (fun i lab ->
              (RealStack(start_dest - i), lab)
            )
            funcs in
        let state =
          List.fold_left
            (fun state (store,_) -> push_camlstack store state)
            state
            dest in
        let state = { state with accu = Const 0 } in
        let instrs =
          instrs_flush
          @ [ Wclosurerec { src; dest; descr } ] in
        (state, instrs)
    | Koffsetclosure offset ->
        let state, instrs_flush = flush_accu lpad state in
        let real_field = lpad.func_offset + offset in
        let n = Array.length lpad.environment in
        if n > 0 && real_field < Array.length lpad.environment &&
             (let _ = assert(real_field >= 0) in true) &&
             Wc_traceglobals.is_function lpad.environment.(real_field) then (
          let efield = Wc_traceglobals.FuncInEnv { func_offset=real_field;
                                                   env=lpad.environment
                                                 } in
          let state = { state with
                        accu = TracedGlobal(Env offset,
                                            [],
                                            efield)
                      } in
          (state, instrs_flush)
        ) else
          let accu = real_accu in
          let state = { state with accu } in
          let instrs =
            instrs_flush
            @ [ Wcopyenv { offset } ] in
          (state, instrs)
    | Krestart ->
        (state, [])
    | Kpushtrap _ | Kpoptrap ->
        (state, [])
    | Kraise kind ->
        (state, [ Wraise { src=state.accu; kind }])
    | Kstop ->
        (state, [ Wstop ])
    | Kcheck_signals | Kevent _ ->
        (state, [])
    | Kstrictbranchif _ -> assert false
    | Kstrictbranchifnot _ -> assert false

let local_branch_labels =
  function
  | I.Kbranch l -> [l]
  | Kbranchif l -> [l]
  | Kbranchifnot l -> [l]
  | Kswitch (la1,la2) -> Array.to_list la1 @ Array.to_list la2
  | _ -> []

let transl_fblock lpad fblock =
  let open Wc_control in
  let indegree = Hashtbl.create 7 in
  let state_table = Hashtbl.create 7 in
  let lpad = { lpad with indegree; state_table } in

  let incr_indegree lab =
    let n = try Hashtbl.find indegree lab with Not_found -> 0 in
    if n >= 0 then
      Hashtbl.replace indegree lab (n+1) in

  let disable_indegree lab =
    Hashtbl.replace indegree lab (-1) in

  let loop_indegree lab =
    Hashtbl.replace indegree lab (-2) in

  let rec count_indegree block =
    ( match block.loop_label with
        | Some lab -> loop_indegree lab
        | None -> ()
    );
    Array.iter
      (function
       | Block b ->
           count_indegree b
       | Trap { catchlabel } ->
           incr_indegree catchlabel
       | Simple i ->
           (match i with
              | (I.Kbranch lab | Kbranchif lab | Kbranchifnot lab) ->
                  incr_indegree lab
              | Kswitch (la1, la2) ->
                  Array.iter disable_indegree la1;
                  Array.iter disable_indegree la2;
              | _ ->
                  ()
             )
       | Label _ | TryReturn | NextMain _ ->
           ()
      )
      block.instructions in

  let rec extract_arity block =
    extract_arity_instrs block block.instructions 0

  and extract_arity_instrs block instrs k =
    if k >= Array.length instrs then
      raise Not_found;
    match instrs.(k) with
      | Label _ | Simple (Klabel _) ->
          extract_arity_instrs block instrs (k+1)
      | Simple (Kgrab num) ->
          num+1
      | Simple _ ->
          raise Not_found
      | Trap _ | TryReturn ->
          raise Not_found
      | Block inner ->
          extract_arity inner
      | NextMain _ ->
          raise Not_found in

  let arity =
    try
      Some(extract_arity fblock.block)
    with
      | Not_found ->
          (* There's no Kgrab when:
              - the function only takes 1 arg
              - the function is for a "try" body
           *)
          if fblock.scope.cfg_try_labels = [] then
            Some 1
          else
            None (* "try" body *) in

  let init_state =
    let camlstack =
      match arity with
        | None -> []
        | Some n -> enum 0 n |> List.map (fun k -> RealStack k) in
    { empty_state with
      arity;
      camlstack;
    } in

  let get_state label =
    try  Hashtbl.find state_table label
    with Not_found -> init_state in

  let rec transl_block block loops =
    let state = init_state in
    (* eprintf "BLOCK\n%!";*)
    let upd_loops =
      match block.loop_label with
        | Some lab -> ISet.add lab loops
        | _ -> loops in
    let state, instrs_rev =
      Array.fold_left
        (fun (state, acc) instr ->
          match instr with
            | Label label ->
                (* eprintf "LABEL %d\n" label; *)
                let state = get_state label in
                let comment = Wcomment (sprintf "Label %d (depth=%d)" label state.camldepth) in
                (state, comment :: acc)
            | Simple i ->
                lpad.loops <- upd_loops;
                let next_state, instrs = transl_instr lpad state i in
                (*
                eprintf "%s predepth=%d postdepth=%d\n%!"
                        (Wc_util.string_of_instruction i)
                        (state.camldepth)
                        (next_state.camldepth);
                 *)
                let comment =
                  Wcomment ("***" ^ Wc_util.string_of_kinstruction i) in
                (next_state, List.rev_append (comment :: instrs) acc)
            | Trap { trylabel; catchlabel; poplabel } ->
                let state, instrs_str = straighten_all lpad state in
                let accu = real_accu in
                let state = { state with accu } in
                let instrs =
                  [ Wcomment (sprintf "***Trap(try=%d,catch=%d)" trylabel catchlabel) ]
                  @ instrs_str
                  @ [ Wtrap { trylabel;
                              catchlabel;
                              depth=state.camldepth
                            };
                      ( match poplabel with
                          | None ->
                              (* e.g. "try raise Foo with Foo -> ..." *)
                              Wunreachable
                          | Some pop ->
                              Wbranch { label=Label pop }
                      )
                    ] in
                update_state_table lpad state catchlabel;
                Option.iter
                  (update_state_table lpad state)
                  poplabel;
                (state, List.rev_append instrs acc)
            | TryReturn ->
                let instrs =
                  [ Wcomment "***TryReturn";
                    Wtryreturn { src=state.accu }
                  ] in
                (state, List.rev_append instrs acc)
            | Block inner ->
                let instrs = transl_block inner upd_loops in
                (state, List.rev_append instrs acc)
            | NextMain label ->
                let instrs =
                  [ Wnextmain { label } ] in
                (state, List.rev_append instrs acc)
        )
        (state, [])
        block.instructions in
    let instrs = List.rev instrs_rev in
    match block.loop_label, block.break_label with
      | Some _, Some _ -> assert false
      | Some label, None ->
          [ Wblock { label=Loop label; body=instrs } ]
      | None, Some label ->
          [ Wblock { label=Label label; body=instrs } ]
      | None, None ->
          instrs in
  count_indegree fblock.block;
  let comment = Wcomment (sprintf "FBLOCK %s" (Wc_control.string_of_scope fblock.scope)) in
  let instrs = comment :: transl_block fblock.block ISet.empty in
  ( match arity with
      | Some n when n > 1 ->
          Wgrab { arity=n } :: instrs
      | _ ->
          instrs
  )

