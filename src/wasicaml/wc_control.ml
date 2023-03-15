(* Copyright (C) 2021 by Figly, Inc.
   This code is free software, see the file LICENSE for details.
 *)

open Wc_types
open Printf

type structured_code =
  { functions : func_block IMap.t;
  }

 and func_block =
   { scope : cfg_scope;
     block : block;
   }

 and block =
   { block_scope : cfg_scope;
     loop_label : int option;
     instructions : instruction array;
     break_label : int option;
   }

 and instruction =
   | Block of block
   | Label of int
   | Simple of I.instruction
   | Trap of full_trap_labels
   | NextMain of int

 and cfg_scope =
  { cfg_letrec_label : int option;
    cfg_func_label : int;
    cfg_try_labels : trap_labels list;
    cfg_main : bool;
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

type cfg_node =
  { cfg_scope : cfg_scope;
    cfg_node_label : int;
    cfg_trap : trap_info option;
    mutable cfg_loops : int list;
    cfg_succ : int list;
    cfg_length : int;
    cfg_final : I.instruction option;
    cfg_next_main : int option;
  }

type cfg =
  { mutable nodes : cfg_node IMap.t;
    mutable code : I.instruction array;
    mutable labels : ISet.t;
  }

let string_of_labels lab =
  sprintf "[try:%d,catch:%d]" lab.trylabel lab.catchlabel

let string_of_scope s =
  sprintf "[letrec=%s,func=%d,try=%s]"
          ( match s.cfg_letrec_label with
              | None -> "none"
              | Some l -> string_of_int l
          )
          s.cfg_func_label
          (List.map string_of_labels s.cfg_try_labels |> String.concat ",")

let rec dump_block block indent =
  let open Wc_util in
  let istr = String.make (4*indent) ' ' in
  eprintf "%sBLOCK %s%s\n"
          istr
          ( match block.loop_label with
              | Some l -> sprintf "loop=%d" l
              | None -> ""
          )
          ( match block.break_label with
              | Some l -> sprintf "break=%d" l
              | None -> ""
          );
  Array.iter
    (function
     | Label label ->
         eprintf "%sLABEL %d\n" istr label
     | Simple i ->
         eprintf "%s%s\n" istr (string_of_kinstruction i)
     | Trap { labels = { trylabel; catchlabel }; poplabel } ->
         eprintf "%sTrap try=%d catch=%d pop=%s\n" istr trylabel catchlabel
                 (Option.map string_of_int poplabel |> Option.value ~default:"")
     | NextMain label ->
         eprintf "%sNextMain %d\n" istr label
     | Block inner ->
         dump_block inner (indent+1)
    )
    block.instructions

let detect_loops ctx =
  let visited = ref ISet.empty in
  let trails = ref IMap.empty in
  let rec recurse exectrail label =
    let node =
      try IMap.find label ctx.nodes
      with Not_found -> assert false in
    if not (ISet.mem label !visited) then (
      (* first-time visit *)
      visited := ISet.add label !visited;
      trails := IMap.add label exectrail !trails;
      let exectrail' = ISet.add label exectrail in
      List.iter (recurse exectrail') node.cfg_succ
    ) else (
      let loop_label_opt =
        if ISet.mem label exectrail then
          (* already visited, and the node is in the execution trail => loop *)
          Some label
        else
          (* already visited => alternate execution paths *)
          ( match node.cfg_loops with
              | [] -> None
              | l :: _ ->
                  (* if the following assertion fails, we found a
                     jump into the loop body *)
                  assert(ISet.mem l exectrail);
                  Some l   (* hitting another loop *)
          ) in
      match loop_label_opt with
        | Some loop_label ->
            let old_trail =
              try IMap.find loop_label !trails
              with Not_found -> assert false in
            let loops =
              if node.cfg_loops = [] || List.hd node.cfg_loops <> loop_label then
                loop_label :: node.cfg_loops
              else
                node.cfg_loops in
            ISet.iter
              (fun lab ->
                let n =
                  try IMap.find lab ctx.nodes
                  with Not_found -> assert false in
                if not(List.mem loop_label n.cfg_loops) then
                  n.cfg_loops <- loops
              )
              (ISet.diff exectrail old_trail)
        | None ->
            ()
    )
    in
  IMap.iter
    (fun label node ->
        let is_entry =
          label = node.cfg_scope.cfg_func_label in
      if is_entry then
        recurse ISet.empty label
    )
    ctx.nodes

let func_labels instr =
  match instr with
    | I.Kclosure(l,_) -> [l]
    | Kclosurerec(ll, _) -> ll
    | _ -> []

let jump_labels instr =
  match instr with
    | I.Kbranch l -> [l]
    | Kbranchif l -> [l]
    | Kbranchifnot l -> [l]
    | Kswitch (la1,la2) -> Array.to_list la1 @ Array.to_list la2
    | Kpushtrap l -> [l]
    | _ -> []

let not_continuing instr =
  match instr with
    | I.Kbranch _ -> true
    | Kreturn _ -> true
    | Kappterm _ -> true
    | Kswitch _ -> true
    | Kraise _ -> true
    | Kstop -> true
    | _ -> false

let match_pushtrap instr =
  match instr with
    | I.Kpushtrap catchlabel -> Some catchlabel
    | _ -> None

let create_cfg code labels =
  let labels = ref labels in
  let nodes = Hashtbl.create 7 in
  let queued = Hashtbl.create 7 in
  let pushtraps = ref IMap.empty in
  let todo = Queue.create() in
  let m = ref 0 in
  let init_scope =
    { cfg_letrec_label = None;
      cfg_func_label = 0;
      cfg_try_labels = [];
      cfg_main = true;
    } in
  Queue.add (init_scope, None, 0) todo;
  let add where scope popfrom start =
    try
      let node = Hashtbl.find nodes start in
      if node.cfg_scope <> scope then (
        eprintf "[DEBUG] where=%d start=%d ex_scope=%s new_scope=%s\n%!"
                where
                start
                (string_of_scope node.cfg_scope)
                (string_of_scope scope);
        failwith "bad scoping";
      )
    with Not_found ->
      if not (Hashtbl.mem queued start) then (
        Queue.add (scope, popfrom, start) todo;
        Hashtbl.add queued start ();
      ) in
  while not (Queue.is_empty todo) do
    let (cfg_scope, popfrom, start) = Queue.take todo in
    Hashtbl.remove queued start;
    if start > !m then (
      (* printf "start=%d\n%!" start; *)
      m := start;
    );
    let n = ref 0 in
    let eon = ref false in
    while not !eon do
      let jlabs = jump_labels code.(start + !n) in
      let flabs = func_labels code.(start + !n) in
      let no_cont = not_continuing code.(start + !n) in
      incr n;
      let end_of_try =
        start + !n < Array.length code &&
          code.(start + !n) = Kpoptrap in
      eon := no_cont || jlabs <> [] || ISet.mem (start + !n) !labels || end_of_try;
      List.iter
        (fun flab ->
          let scope =
            { cfg_letrec_label = Some(List.hd flabs);
              cfg_func_label = flab;
              cfg_try_labels = [];
              cfg_main = false;
            } in
          add (start + !n - 1) scope None flab
        )
        flabs
    done;
    let pushtrap_opt = match_pushtrap code.(start + !n - 1) in
    let is_poptrap = code.(start) = Kpoptrap in
    let last_no_cont = not_continuing code.(start + !n - 1) in
    let last_clabs = if last_no_cont then [] else [start + !n] in
    let last_jlabs = jump_labels code.(start + !n - 1) in
    let cfg_succ = last_clabs @ last_jlabs in
    let cfg_final, cfg_length =
      match code.(start + !n - 1) with
        | Kbranch lab as instr ->
            (Some instr, !n-1)
        | _ ->
            if last_no_cont || pushtrap_opt <> None then
              (None, !n)
            else
              (Some (I.Kbranch (start + !n)), !n) in
    if pushtrap_opt <> None then
      pushtraps := IMap.add (start + !n) start !pushtraps;
    let cfg_trap =
      match pushtrap_opt with
        | Some catchlabel ->
            let labels = { trylabel=start + !n; catchlabel } in
            Some (Trap_push { labels; poplabel=None })
        | None ->
            if is_poptrap then
              match popfrom with
                | None ->
                    eprintf "[DEBUG] start=%d n=%d\n%!" start !n;
                    assert false
                | Some labels ->
                    (* Fix up in the corresponding Trap_push node: *)
                    let push_label =
                      try IMap.find labels.trylabel !pushtraps
                      with Not_found -> assert false in
                    let push_node = Hashtbl.find nodes push_label in
                    let push_node' =
                      { push_node with
                        cfg_trap = ( match push_node.cfg_trap with
                                       | Some (Trap_push { labels }) ->
                                           Some (Trap_push { labels; poplabel = Some start })
                                       | _ -> assert false
                                   );
                      } in
                    Hashtbl.replace nodes push_label push_node';
                    Some (Trap_pop labels)
            else
              None in
    let cfg_node =
      { cfg_scope;
        cfg_node_label = start;
        cfg_trap;
        cfg_loops = [];  (* later *)
        cfg_succ;
        cfg_length;
        cfg_final;
        cfg_next_main = None;
      } in
    Hashtbl.replace nodes start cfg_node;
    if not last_no_cont then (
      let next = start + !n in
      let scope =
        match pushtrap_opt with
          | Some catchlabel ->
              let labels = { trylabel = next; catchlabel } in
              { cfg_scope with
                cfg_try_labels = labels :: cfg_scope.cfg_try_labels
              }
          | None ->
              if code.(next) = Kpoptrap then
                { cfg_scope with
                  cfg_try_labels = List.tl cfg_scope.cfg_try_labels
                }
              else
                cfg_scope in
      let popfrom =
        if code.(next) = Kpoptrap then
          Some (List.hd cfg_scope.cfg_try_labels)
        else
          None in
      add next scope popfrom next
    );
    List.iter
      (fun lab ->
        let scope, popfrom =
          if code.(lab) = Kpoptrap then
            { cfg_scope with cfg_try_labels = List.tl cfg_scope.cfg_try_labels },
            Some (List.hd cfg_scope.cfg_try_labels)
          else
            cfg_scope, None in
        add (start + !n - 1) scope popfrom lab
      )
      last_jlabs
  done;
  let nodemap = ref IMap.empty in
   Hashtbl.iter
     (fun label node ->
       (* eprintf "LABEL %d: SCOPE %s\n" label (string_of_scope node.cfg_scope); *)
       nodemap := IMap.add label node !nodemap
     )
     nodes;
  let cfg =
    { nodes = !nodemap;
      code;
      labels = !labels;
    } in
  detect_loops cfg;
  cfg


let split_main_function cfg =
  (* We are interested in nodes that
      - are part of the main function (initializer)
      - can only be reached via Kbranch instructions (no condition, no switch)
      - is not part of a loop
      - does not start/end a "try" block
      - start with a stack depth of 0
    Such nodes are moved into a new function - to avoid very long wasm
    functions.
   *)
  let depth_table = Hashtbl.create 7 in
  let excluded = Hashtbl.create 7 in
  let exclude p =
    Hashtbl.replace excluded p true in
  let rec set_depth depth label =
    if not (Hashtbl.mem depth_table label) then (
      Hashtbl.add depth_table label depth;
      let node =
        try IMap.find label cfg.nodes
        with Not_found -> assert false in
      assert(node.cfg_scope.cfg_letrec_label = None);
      assert(node.cfg_scope.cfg_main);
      if node.cfg_loops <> [] then exclude label;
      if node.cfg_trap <> None then exclude label;
      let d = ref depth in
      for lab = label to label + node.cfg_length - 1 do
        let instr = cfg.code.(lab) in
        d := Wc_traceinstr.trace_stack_instr !d instr;
        match instr with
          | I.Kbranch _ -> assert false
          | I.Kbranchif p | I.Kbranchifnot p ->
              exclude p
          | I.Kswitch(plist,qlist) ->
              Array.iter exclude plist;
              Array.iter exclude qlist;
          | I.Kpushtrap lab ->
              set_depth (!d-4) lab
          | _ ->
              ()
      done;
      List.iter (set_depth !d) node.cfg_succ
    ) in
  let starts_another_function label =
    Hashtbl.find depth_table label = 0 &&
      not (Hashtbl.mem excluded label) in
  (* now split the main function up: *)
  let visited = Hashtbl.create 7 in
  let rec split_function new_letrec_label label =
    if not (Hashtbl.mem visited label) then (
      Hashtbl.add visited label true;
      let node =
        try IMap.find label cfg.nodes
        with Not_found -> assert false in
      let cfg_scope =
        { node.cfg_scope with
          cfg_letrec_label = new_letrec_label;
          cfg_func_label = new_letrec_label |> Option.value ~default:0;
        } in
      let cfg_final, cfg_next_main =
        match node.cfg_final with
          | Some (I.Kbranch lab) when starts_another_function lab ->
              (None, Some lab)
          | _ ->
              (node.cfg_final, None) in
      let next_funcs, cfg_succ =
        List.partition starts_another_function node.cfg_succ in
      let node =
        { node with
          cfg_scope; cfg_succ; cfg_final; cfg_next_main
        } in
      cfg.nodes <- IMap.add label node cfg.nodes;
      List.iter (split_function new_letrec_label) cfg_succ;
      List.iter (fun lab -> split_function (Some lab) lab) next_funcs
    ) in
  let start_label = 0 in
  let start_node =
    try IMap.find start_label cfg.nodes
    with Not_found -> assert false in
  assert(start_node.cfg_scope.cfg_letrec_label = None);
  set_depth 0 start_label;
  split_function None start_label


let is_node_in_loop ctx loop_label label =
  let node =
    try IMap.find label ctx.nodes
    with Not_found -> assert false in
  List.mem loop_label node.cfg_loops

let recover_structure ctx =
  let in_degree = ref IMap.empty in
  let rec inc_degree trail label =
    let deg =
      try IMap.find label !in_degree
      with Not_found -> 0 in
   in_degree := IMap.add label (deg+1) !in_degree;
   if deg = 0 then (
     let node =
       try IMap.find label ctx.nodes
       with Not_found -> assert false in
     let trail' = ISet.add label trail in
     let eff_succ =
       List.filter
         (fun lab ->
           not (ISet.mem lab trail')
         )
         node.cfg_succ in
     List.iter (inc_degree trail') eff_succ
   ) in
  let rec dec_degree trail label =
    let deg =
      try IMap.find label !in_degree
      with Not_found -> assert false in
    in_degree := IMap.add label (deg-1) !in_degree;
    if deg = 1 then (
      let node =
        try IMap.find label ctx.nodes
        with Not_found -> assert false in
      let trail' = ISet.add label trail in
      let eff_succ =
        List.filter
          (fun lab ->
            not (ISet.mem lab trail')
          )
          node.cfg_succ in
      label :: List.concat_map (dec_degree trail') eff_succ
    ) else
      [] in

  let rec build_block prev_loop loops_started inner labels =
    match labels with
      | label1 :: labels' ->
          let node =
            try IMap.find label1 ctx.nodes
            with Not_found -> assert false in
          let node_loop =
            match node.cfg_loops with
              | [] -> None
              | lab :: _ -> Some lab in
          let inner_instructions =
            List.map (fun b -> Block b) inner
            |> Array.of_list in
          let node_instructions =
            (if node.cfg_length = 0 then
               [| |]
             else
               Array.sub ctx.code label1 node.cfg_length
            )
            |> Array.map
                 (fun instr ->
                   match instr with
                     | I.Kpushtrap catchlabel ->
                         ( match node.cfg_trap with
                             | Some(Trap_push { labels; poplabel }) ->
                                 Trap { labels; poplabel }
                             | _ ->
                                 assert false
                         )
                     | _ -> Simple instr
                 )
            |> (fun a1 ->
              match node.cfg_final with
                | None -> a1
                | Some instr ->
                    assert(node.cfg_next_main = None);
                    Array.append a1 [| Simple instr |]
            )
            |> (fun a1 ->
              match node.cfg_next_main with
                | None -> a1
                | Some label ->
                    assert(node.cfg_final = None);
                    Array.append a1 [| NextMain label |]
            )
            |> (fun a ->
              Array.append [| Label label1 |] a)
          in
          if prev_loop = node_loop || List.mem node_loop loops_started then
            let instructions =
              Array.append inner_instructions node_instructions in
            let break_label =
              match labels' with
                | [] -> None
                | lab :: _ -> Some lab in
            let b =
              { block_scope = node.cfg_scope;
                loop_label = None;
                break_label;
                instructions
              } in
            let inner' = [ b ] in
            build_block node_loop loops_started inner' labels'
          else
            let loop_start_label =
              match node_loop with
                | Some lab -> lab
                | None -> assert false in
            let loop_labels, other_labels =
              List.partition
                (is_node_in_loop ctx loop_start_label)
                labels in
            let loop_body =
              build_block node_loop (node_loop :: loops_started) [] loop_labels in
            let loop_block =
              { block_scope = node.cfg_scope;
                loop_label = node_loop;
                break_label = None;
                instructions = [| Block loop_body |]
              } in
            let loop_instructions =
              [| Block loop_block |] in
            let instructions =
              Array.append inner_instructions loop_instructions in
            let break_label =
              match other_labels with
                | [] -> None
                | lab :: _ -> Some lab in
            let inner' =
              [ { block_scope = node.cfg_scope;
                  loop_label = None;
                  break_label;
                  instructions;
                }
              ] in
            build_block None loops_started inner' other_labels
      | [] ->
          ( match inner with
              | [ b ] -> b
              | _ ->
                  let instructions =
                    List.map (fun b -> Block b) inner
                    |> Array.of_list in
                  { block_scope = (List.hd inner).block_scope;
                    loop_label = None;
                    break_label = None;
                    instructions
                  }
          ) in
  let functions =
    IMap.filter_map
      (fun label node ->
        let is_entry =
          label = node.cfg_scope.cfg_func_label in
        if is_entry then (
          inc_degree ISet.empty label;
          let sorted_block_labels = dec_degree ISet.empty label in
          let block = build_block None [None] [] sorted_block_labels in
          Some { scope = node.cfg_scope;
                 block
               }
        )
        else None
      )
      ctx.nodes in
  { functions }

let validate scode =
  let error func_label last_label message =
    failwith
      (sprintf "validation error function %d near %d: %s"
               func_label last_label message) in

  let rec validate_block labels_in_scope func_label last_label block =
    if block.loop_label <> None && block.break_label <> None then
      error func_label last_label "both loop_label and break_label";
    let last_label' =
      match block.loop_label, block.break_label with
        | Some lab, _ -> lab
        | _, Some lab -> lab
        | _ -> last_label in
    let labels_in_scope' =
      match block.loop_label with
        | None -> labels_in_scope
        | Some lab ->
            if ISet.mem lab labels_in_scope then
              error func_label last_label' "loop_label not new";
            ISet.add lab labels_in_scope in
    let labels_in_scope'' =
      match block.break_label with
        | None -> labels_in_scope'
        | Some lab ->
            if ISet.mem lab labels_in_scope' then
              error func_label last_label' "break_label not new";
            ISet.add lab labels_in_scope' in
    Array.iter
      (validate_instruction labels_in_scope'' func_label last_label')
      block.instructions

  and validate_instruction labels_in_scope func_label last_label instruction =
    match instruction with
      | Block block ->
          validate_block labels_in_scope func_label last_label block
      | Simple instr ->
          ( match instr with
              | I.Kclosure (lab, _) ->
                  validate_function_label func_label last_label lab
              | I.Kclosurerec (labl, _) ->
                  List.iter
                    (validate_function_label func_label last_label)
                    labl
              | _ ->
                  let labels = Wc_reader.get_labels_in_instr instr in
                  List.iter (validate_label labels_in_scope func_label last_label) labels
          )
      | Trap { labels = { catchlabel } } ->
          validate_label labels_in_scope func_label last_label catchlabel
      | Label _ ->
          ()
      | NextMain _ ->
          ()

  and validate_label labels_in_scope func_label last_label label =
    if not (ISet.mem label labels_in_scope) then
      error func_label last_label (sprintf "label not defined: %d" label)

  and validate_function_label func_label last_label label =
    if not (IMap.mem label scode.functions) then
      error func_label last_label (sprintf "function not defined: %d" label)
  in

  let validate_fblock label fblock =
    validate_block ISet.empty label (-1) fblock.block
  in

  IMap.iter
    validate_fblock
    scode.functions

