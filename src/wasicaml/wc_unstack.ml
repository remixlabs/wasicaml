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
    realstack : ISet.t;
    (* which real stack positions have been initialized *)
    accu : store option;
    (* where the accu is stored *)
    realaccu : ISet.t;
    (* in which stack positions the "accu" variable must be saved on flush.
       If additionally accu=RealAccu, the "accu" variable also keeps the
       logical accu value.
     *)
  }

type lpad =  (* local pad *)
  { locals : (string, repr) Hashtbl.t;
    (* IDEA: maybe switch to (string, repr * bool ref) Hashtbl.t. The bool
       is set to true when the local is actually used.
     *)
    mutable loops : ISet.t;
    (* which labels are loop labels *)
  }

let empty_state =
  { camlstack = [];
    camldepth = 0;
    realstack = ISet.empty;
    accu = Some RealAccu;
    realaccu = ISet.empty;
  }

let empty_lpad() =
  { locals = Hashtbl.create 7;
    loops = ISet.empty;
  }

let new_local lpad repr =
  let k = Hashtbl.length lpad.locals in
  let s = sprintf "x%d" k in
  Hashtbl.add lpad.locals s repr;
  s

let realdepth state =
  (* how many stack positions are really used, counted from the bottom? *)
  match ISet.min_elt_opt state.realstack with
    | None ->
        0
    | Some pos ->
        assert(pos < 0);
        -pos

let stack_descr state =
  let stack_save_accu =
    state.realaccu <> ISet.empty || state.accu = Some RealAccu in
  { stack_init = state.realstack;
    stack_depth = realdepth state;
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
          realstack =
            if pos = cpos || not(List.mem (RealStack pos) tl) then
              ISet.remove pos state.realstack
            else
              state.realstack
        }
    | RealAccu :: tl ->
        { state with
          camlstack = tl;
          camldepth = cd - 1;
          realaccu = ISet.remove cpos state.realaccu;
        }
    | (Const _ | Local _ | Atom _) :: tl ->
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
          realstack = ISet.add pos state.realstack;
        }
    | RealAccu ->
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

let flush_accu lpad state =
  (* If the accu is not yet saved, do this now. This function needs to be called
     before setting the accu to a new value. *)
  let instrs_rev =
    ISet.fold
      (fun pos instr_acc ->
        let instr =
          Wcopy { src=RealAccu; dest=RealStack pos } in
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
        if ISet.mem pos state.realaccu then
          RealStack pos
        else
          old
      )
      state.camlstack in
  let state =
    { state with
      camlstack;
      realaccu = ISet.empty;
      realstack = ISet.union state.realstack state.realaccu;
    } in
  (state, instrs)

let accu_store state =
  match state.accu with
    | Some store -> store
    | None -> assert false   (* uninitialized accu *)

let straighten_accu lpad state =
  (* if the accu is stored elsewhere, load it now into "accu". This function
     needs to be called before doing something with the value in the accu
   *)
  let state, instrs_flush = flush_accu lpad state in
  match state.accu with
    | Some RealAccu ->
        (state, instrs_flush)
    | Some store ->
        let instr =
          Wcopy { src=store; dest=RealAccu } in
        let state = { state with accu = Some RealAccu } in
        (state, instrs_flush @ [instr])
    | None ->
        (state, instrs_flush)

let straighten_accu_when_on_stack lpad state =
  (* Load the accu if it is stored in the real stack. This is needed before
     renaming the accu into another stack position, in order to enforece
     the "no swapping of stack positions" rule.
   *)
  match state.accu with
    | Some (RealStack _) ->
        straighten_accu lpad state
    | _ ->
        (state, [])

let pop_real_stack lpad state num =
  (* Ensure that values stored at the num bottom positions of the stack
     are saved to their real stores, in preparation of popping these
     positions. This can only be the accu.
   *)
  let cd = state.camldepth in
  match state.accu with
    | Some (RealStack pos) when pos >= (-cd) && pos <= (-cd+num-1) ->
        straighten_accu lpad state
        (* NB. this doesn't pop from camlstack *)
    | _ ->
        (state, [])

let flush_real_stack_only_accu_at lpad state pos =
  match state.accu with
    | Some (RealStack p) when p = pos ->
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
      realstack = ISet.union positions state.realstack
    } in
  (state, instrs1 @ instrs2)

let straighten_stack_at lpad state pos =
  (* ensure that the caml stack for pos is set to the real stack *)
  let k = pos + state.camldepth in
  let store = List.nth state.camlstack k in
  match store with
    | RealAccu ->
        assert(ISet.mem pos state.realaccu);
        flush_accu lpad state
    | RealStack p when p = pos ->
        (state, [])
    | _ ->
        assert(not (List.mem (RealStack pos) state.camlstack));
        let instrs = [ Wcopy { src=store; dest=RealStack pos } ] in
        let state =
          { state with
            camlstack = set_camlstack pos (RealStack pos) state;
            realstack = ISet.add pos state.realstack;
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

let straighten_all lpad state =
  let cd = state.camldepth in
  let pos_list =
    state.camlstack
    |> List.filteri
         (fun i store ->
           match store with
             | RealStack p -> p <> -cd+i
             | _ -> true
         )
    |> List.map (function RealStack p -> p | _ -> assert false) in
  straighten_stack_multi lpad state pos_list

let unary_operation lpad state op_repr op_code =
  let src1 = accu_store state in
  match op_repr with
    | RValue ->
        (* result goes into accu *)
        let state, instrs_flush = flush_accu lpad state in
        let state = { state with accu = Some RealAccu } in
        let instrs_op = [ Wunary { op=op_code; src1; dest=RealAccu }] in
        (state, instrs_flush @ instrs_op)
    | _ ->
        let dest_name = new_local lpad op_repr in
        let dest = Local(op_repr, dest_name) in
        let state = { state with accu = Some dest } in
        let instrs_op = [ Wunary { op=op_code; src1; dest }] in
        (state, instrs_op)

let unary_effect lpad state op_code =
  let src1 = accu_store state in
  let state = { state with accu = None } in
  let instrs_op = [ Wunaryeffect { op=op_code; src1 }] in
  (state, instrs_op)

let binary_operation lpad state op_repr op_code =
  let src1 = accu_store state in
  let src2 = List.hd state.camlstack in
  match op_repr with
    | RValue ->
        (* result goes into accu *)
        let state, instrs_flush = flush_accu lpad state in
        let state =
          { state with accu = Some RealAccu }
          |> pop_camlstack in
        let instrs_op = [ Wbinary { op=op_code; src1; src2; dest=RealAccu }] in
        (state, instrs_flush @ instrs_op)
    | _ ->
        let dest_name = new_local lpad op_repr in
        let dest = Local(op_repr, dest_name) in
        let state =
          { state with accu = Some dest }
          |> pop_camlstack in
        let instrs_op = [ Wbinary { op=op_code; src1; src2; dest }] in
        (state, instrs_op)

let binary_effect lpad state op_code =
  let src1 = accu_store state in
  let src2 = List.hd state.camlstack in
  let state = { state with accu = None } |> pop_camlstack in
  let instrs_op = [ Wbinaryeffect { op=op_code; src1; src2 }] in
  (state, instrs_op)

let ternary_effect lpad state op_code =
  let src1 = accu_store state in
  let src2 = List.hd state.camlstack in
  let src3 = List.hd (List.tl state.camlstack) in
  let state = { state with accu = None } |> pop_camlstack |> pop_camlstack in
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

let transl_instr lpad state instr =
  match instr with
    | I.Klabel _ -> assert false
    | Kconst (Lambda.Const_base (Asttypes.Const_int k)) ->
        ( { state with accu = Some(Const k) }, [])
    | Kconst _ ->
        assert false
    | Kacc sp ->
        let state =
          if sp < state.camldepth then
            { state with accu = Some(List.nth state.camlstack sp) }
          else
            { state with accu = Some (RealStack (-state.camldepth + sp)) } in
        (state, [])
    | Kpush ->
        let state = push_camlstack (accu_store state) state in
        (state, [])
    | Kpush_retaddr lab ->
        let state = push_camlstack (Const 1) state in
        let state = push_camlstack (Local(RValue, "env")) state in
        let state = push_camlstack (Const 1) state in
        (state, [])
    | Kpop num ->
        let state, instrs = pop_real_stack lpad state num in
        let state = popn_camlstack num state in
        (state, instrs)
    | Kassign sp ->
        let cd = state.camldepth in
        let state, instrs_flush =
          flush_real_stack_at lpad state (-cd+sp) in
        let instrs_copy =
          [ Wcopy { src=(accu_store state); dest=RealStack (-cd+sp) } ] in
        (state, instrs_flush @ instrs_copy)
    | Kenvacc field ->
        let state, instrs_flush = flush_accu lpad state in
        let state = { state with accu = Some RealAccu } in
        let instrs_op = [ Wenv { field } ] in
        (state, instrs_flush @ instrs_op)
    | Kgetglobal ident ->
        let offset = global_offset ident in
        assert(offset >= 0);
        let state, instrs_flush = flush_accu lpad state in
        let state = { state with accu = Some RealAccu } in
        let instrs_op = [ Wgetglobal { src=Global offset } ] in
        (state, instrs_flush @ instrs_op)
    | Knegint ->
        unary_operation lpad state RIntVal Pnegint
    | Kboolnot ->
        unary_operation lpad state RIntVal Pboolnot
    | Koffsetint offset ->
        unary_operation lpad state RIntVal (Poffsetint offset)
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
        unary_operation lpad state RValue (Pgetpubmet k)
    | Kaddint ->
        binary_operation lpad state RIntVal Paddint
    | Ksubint ->
        binary_operation lpad state RIntVal Psubint
    | Kmulint ->
        binary_operation lpad state RInt Pmulint
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
        binary_operation lpad state RInt Plslint
    | Klsrint ->
        binary_operation lpad state RInt Plsrint
    | Kasrint ->
        binary_operation lpad state RInt Pasrint
    | Kintcomp cmp ->
        binary_operation lpad state RInt (Pintcomp cmp)
    | Kisout ->
        binary_operation lpad state RInt (Puintcomp Cgt)
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
        binary_operation lpad state RValue Pgetmethod
    | Kgetdynmet ->
        binary_operation lpad state RValue Pgetdynmet
    | Ksetvectitem ->
        ternary_effect lpad state Psetvectitem
    | Ksetbyteschar ->
        ternary_effect lpad state Psetbyteschar
    | Kgetfloatfield field ->
        (* do the allocation right here and don't postpone it - we are not
           yet prepared for doing allocations on demand later
         *)
        let src1 = accu_store state in
        let state, instrs_flush = flush_accu lpad state in
        let temp_name = new_local lpad RFloat in
        let temp = Local(RFloat, temp_name) in
        let descr = stack_descr state in
        let instrs =
          instrs_flush
          @ [ Wunary { op=Pgetfloatfield field; src1; dest=temp; };
              Walloc { src=temp; dest=RealAccu; descr }
            ] in
        let state = { state with accu = Some RealAccu } in
        (state, instrs)
    | Kmakeblock(0, tag) ->
        let state = { state with accu = Some(Atom tag) } in
        (state, [])
    | Kmakeblock(size, tag) ->
        let state, instrs_flush = flush_accu lpad state in
        let src1 = accu_store state in
        let src = src1 :: list_prefix (size-1) state.camlstack in
        let descr = stack_descr state in
        let instrs =
          instrs_flush
          @ [ Wmakeblock { tag; src; descr } ] in
        let state =
          { state with accu = Some RealAccu }
          |> popn_camlstack (size-1) in
        (state, instrs)
    | Kmakefloatblock size ->
        assert(size > 0);
        let state, instrs_flush = flush_accu lpad state in
        let src1 = accu_store state in
        let src = src1 :: list_prefix (size-1) state.camlstack in
        let descr = stack_descr state in
        let instrs =
          instrs_flush
          @ [ Wmakefloatblock { src; descr } ] in
        let state =
          { state with accu = Some RealAccu }
          |> popn_camlstack (size-1) in
        (state, instrs)
    | Kccall (name, num) when num <= 5 ->
        let state, instrs_flush = flush_accu lpad state in
        let src1 = accu_store state in
        let src = src1 :: list_prefix (num-1) state.camlstack in
        let descr = stack_descr state in
        let instrs =
          instrs_flush
          @ [ Wccall { name; src; descr } ] in
        let state =
          { state with accu = Some RealAccu }
          |> popn_camlstack (num-1) in
        (state, instrs)
    | Kccall (name, num) ->
        let state, instrs_flush = flush_accu lpad state in
        let descr = stack_descr state in
        let depth = state.camldepth in
        let instrs =
          instrs_flush
          @ [ Wccall_vector { name; numargs=num; depth; descr }] in
        let state =
          { state with accu = Some RealAccu }
          |> popn_camlstack (num-1) in
        (state, instrs)
    | Kbranch label ->
        let state, instrs_str = straighten_all lpad state in
        (state, instrs_str @ [ Wbranch{label=make_label lpad label} ])
    | Kbranchif label ->
        let state, instrs_str = straighten_all lpad state in
        let src = accu_store state in
        (state, instrs_str @ [ Wbranchif{label=make_label lpad label; src} ])
    | Kbranchifnot label ->
        let state, instrs_str = straighten_all lpad state in
        let src = accu_store state in
        (state, instrs_str @ [ Wbranchifnot{label=make_label lpad label; src} ])
    | Kswitch (labels_int, labels_blk) ->
        let labels_int = Array.map (make_label lpad) labels_int in
        let labels_blk = Array.map (make_label lpad) labels_blk in
        let state, instrs_str = straighten_all lpad state in
        let src = accu_store state in
        (state, instrs_str @ [ Wswitch{labels_int; labels_blk; src} ])
    | Kapply num when num <= 3 ->
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
          @ [ Wcopy { src=Const 1; dest=RealStack(-cd+num-3) };
              Wcopy { src=Local(RValue, "env"); dest=RealStack(-cd+num-2) };
              Wcopy { src=Const 1; dest=RealStack(-cd+num-1) };
            ]
          @ instrs_move
          @ [ Wapply { numargs=num; depth=cd+3; src=RealAccu }
            ] in
        let state = state |> popn_camlstack num in
        (state, instrs)
    | Kapply num ->
        let state, instrs_str = straighten_all lpad state in
        let instrs =
          instrs_str
          @ [ Wapply { numargs=num; depth=state.camldepth; src=RealAccu }
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
        (state, [ Wreturn { src=accu_store state } ])
    | Kgrab num ->
        (state, [ Wgrab { numargs=num }])
    | Kclosure(lab, num) ->
        let state, instrs_flush = flush_accu lpad state in
        let src1 = accu_store state in
        let src = src1 :: list_prefix (num-1) state.camlstack in
        let descr = stack_descr state in
        let instrs =
          instrs_flush
          @ [ Wclosurerec { src; dest=[ RealAccu, make_label lpad lab ]; descr }] in
        let state =
          { state with accu = Some RealAccu }
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
            accu_store state :: list_prefix num_stack state.camlstack in
        let state = state |> popn_camlstack num_stack in
        let start_dest = -state.camldepth-1 in
        let dest =
          List.mapi
            (fun i lab ->
              (RealStack(start_dest - i), make_label lpad lab)
            )
            funcs in
        let state =
          List.fold_left
            (fun state (store,_) -> push_camlstack store state)
            state
            dest in
        let state = { state with accu = None } in
        let instrs =
          instrs_flush
          @ [ Wclosurerec { src; dest; descr } ] in
        (state, instrs)
    | Koffsetclosure offset ->
        let state, instrs_flush = flush_accu lpad state in
        let instrs =
          instrs_flush
          @ [ Wcopyenv { offset } ] in
        (state, instrs)
    | Krestart ->
        (state, [])
    | Kpushtrap _ | Kpoptrap ->
        (state, [])
    | Kraise kind ->
        (state, [ Wraise { src=accu_store state; kind }])
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
  let depth_table = Hashtbl.create 7 in
  (* maps label to depth of camlstack *)

  let get_state label =
    let camldepth =
      try Hashtbl.find depth_table label
      with Not_found -> 0 in
    let camlstack =
      enum (-camldepth) camldepth
      |> List.map (fun pos -> RealStack pos) in
    let realstack =
      enum (-camldepth) camldepth
      |> List.fold_left
           (fun acc pos -> ISet.add pos acc)
           ISet.empty in
    { empty_state with
      camlstack;
      camldepth;
      realstack;
    } in

  let update_depth_table state labels =
    List.iter
      (fun label ->
        try
          let d = Hashtbl.find depth_table label in
          assert(d = state.camldepth)
        with
          | Not_found ->
              Hashtbl.add depth_table label state.camldepth
      )
      labels in

  let rec transl_block block loops =
    let state = { empty_state with accu = None } in
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
                let comment = Wcomment (sprintf "Label %d" label) in
                (get_state label, comment :: acc)
            | Simple i ->
                let labels = local_branch_labels i in
                update_depth_table state labels;
                lpad.loops <- upd_loops;
                let (next_state, instrs) =
                  transl_instr lpad state i in
                (*
                eprintf "%s predepth=%d postdepth=%d\n%!"
                        (Wc_util.string_of_instruction i)
                        (state.camldepth)
                        (next_state.camldepth);
                 *)
                let comment =
                  Wcomment ("***" ^ Wc_util.string_of_instruction i) in
                (next_state, List.rev_append (comment :: instrs) acc)
            | Trap { trylabel; catchlabel } ->
                let state, instrs_str = straighten_all lpad state in
                let state = { state with accu = Some RealAccu } in
                let instrs =
                  [ Wcomment (sprintf "Kpushtrap(try=%d,catch=%d)" trylabel catchlabel) ]
                  @ instrs_str
                  @ [ Wtrap { trylabel=make_label lpad trylabel;
                              catchlabel=make_label lpad catchlabel;
                              depth=state.camldepth
                            }
                    ] in
                (state, List.rev_append instrs acc)
            | TryReturn ->
                let instrs =
                  [ Wcomment "tryreturn";
                    Wtryreturn { src=accu_store state }
                  ] in
                (state, List.rev_append instrs acc)
            | Block inner ->
                let instrs = transl_block inner upd_loops in
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
  transl_block fblock.block ISet.empty

