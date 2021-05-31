open Printf
open Wc_types
open Wc_instruct
open Wc_util

type state =
  { camlstack : store list;
    (* where the contents are stored that are supposed to be on the
       OCaml stack - in top to bottom order.

       If an OCaml stack value is stored in the real stack, it is only
       allowed to store it at its original position or at a lower (newer)
       position. Formally, if element k is
       set to store=RealStack(pos), then pos<=-camldepth+k.

       Also, if a stack value is not stored at the original position,
       there must be no other value stored at the original position (so that we
       can always copy the value to its original position without first
       having to move some other value away).
     *)
    camldepth : int;
    (* = List.length camlstack *)
    accu : store;
    (* where the accu is stored *)
    realaccu : ISet.t;
    (* in which stack positions the "accu" variable must be saved on flush.
       If additionally accu=RealAccu, the "accu" variable also keeps the
       logical accu value.
     *)
    arity : int;
    (* number of args pf the current function - first set after Kgrab *)
  }

(* IDEA: there is sometimes code setting the accu, but then the accu
   is not accessed. Add to state:

   accu_used : bool ref option

   which is in particular set by straighten_accu. The ref is set to true
   whenever there is a read access of state.accu.
 *)

type lpad =  (* local pad *)
  { locals : (string, repr) Hashtbl.t;
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
    arity = 1;
  }

let empty_lpad() =
  { locals = Hashtbl.create 7;
    avoid_locals = false;
    loops = ISet.empty;
    indegree = Hashtbl.create 7;
    state_table = Hashtbl.create 7;
  }

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
  Array.iteri
    (fun i used ->
      if not used && i >= cd - !rd then
        uninit := (-cd+i) :: !uninit
    )
    stack;
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
    state.camlstack

let pop_camlstack state =
  let cd = state.camldepth in
  let cpos = (-cd) in
  match state.camlstack with
    | RealStack pos :: tl ->
        { state with
          camlstack = tl;
          camldepth = cd - 1;
        }
    | (RealAccu _):: tl ->
        { state with
          camlstack = tl;
          camldepth = cd - 1;
          realaccu = ISet.remove cpos state.realaccu;
        }
    | (Const _ | Local _ | Atom _ | Invalid) :: tl ->
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
  let cpos = (-cd-1) in
  match store with
    | RealStack pos ->
        { state with
          camlstack = store :: state.camlstack;
          camldepth = cd + 1;
        }
    | RealAccu _ ->
        { state with
          camlstack = store :: state.camlstack;
          camldepth = cd + 1;
          realaccu = ISet.add cpos state.realaccu;
        }
    | (Const _ | Local _ | Atom _) ->
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
  let camlstack =
    List.mapi
      (fun i old ->
        let pos = (-state.camldepth+i) in
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
    | RealStack _ | Const _ | Atom _ ->
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

let patch camlstack depth patches =
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
  recurse camlstack (-depth) (ISet.elements patches)
    (* NB. exploiting that ISet.elements returns the elements in ascending
       order *)

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
  let camlstack =
    patch state.camlstack cd positions in
  let state =
    { state with
      camlstack;
    } in
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
        let state =
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
        let accu = real_accu in
        let state = { state with accu } in
        let instrs_op = [ Wenv { field } ] in
        (state, instrs_flush @ instrs_op)
    | Kgetglobal ident ->
        let offset = global_offset ident in
        assert(offset >= 0);
        let state, instrs_flush = flush_accu lpad state in
        let accu = real_accu in
        let state = { state with accu } in
        let instrs_op = [ Wgetglobal { src=Global offset } ] in
        (state, instrs_flush @ instrs_op)
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
        unary_operation lpad state RValue (Pgetfield field)
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
        (* TODO: the args don't need to be straightened - they are duplicated
           anyway *)
        let state, instrs_str = straighten_all lpad state in
        let cd = state.camldepth in
        let instrs_move =
          enum 0 num
          |> List.map
               (fun k ->
                 Wcopy { src=RealStack(-cd+k); dest=RealStack(-cd+k-3) }
               ) in
        let instrs =
          instrs_str
          @ instrs_move
          @ [ Wcopy { src=Const 0; dest=RealStack(-cd+num-3) };
              Wcopy { src=Const 0; dest=RealStack(-cd+num-2) };
              Wcopy { src=Const 0; dest=RealStack(-cd+num-1) };
              Wapply { numargs=num; depth=cd+3; src=real_accu }
            ] in
        let state = state |> popn_camlstack num in
        (state, instrs)
    | Kapply num ->
        let state, instrs_str = straighten_all lpad state in
        let instrs =
          instrs_str
          @ [ Wapply { numargs=num; depth=state.camldepth; src=real_accu }
            ] in
        let state = state |> popn_camlstack (num+3) in
        (state, instrs)
    | Kappterm(num, slots) ->
        let state, instrs_accu =
          straighten_accu lpad state in
        let state, instrs_stack =
          straighten_stack_multi lpad state (enum (-state.camldepth) num) in
        let instrs =
          instrs_accu
          @ instrs_stack
          @ [ Wappterm { numargs=num; oldnumargs=(slots - state.camldepth);
                         depth=state.camldepth
                       }
            ] in
        (state, instrs)
    | Kreturn slots ->
        assert(state.camldepth + state.arity = slots);
        (state, [ Wreturn { src=state.accu; arity=state.arity } ])
    | Kgrab num ->
        ({state with arity = num+1}, [ Wgrab { numargs=num }])
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

  let get_state label =
    try  Hashtbl.find state_table label
    with Not_found -> empty_state in

  let rec transl_block block loops =
    let state = empty_state in
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
  transl_block fblock.block ISet.empty

