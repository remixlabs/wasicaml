open Wc_types

type structured_code =
  { functions : func_block IMap.t;  (* by cfg_func_label *)
  }

 and func_block =
   { scope : cfg_scope;
     block : block;
   }

 and block =
   { loop_label : int option;
     (* a label pointing to the beginning of this block, for jumping back *)
     instructions : instruction array;
     (* what to execute *)
     break_label : int option;
     (* a label pointing to the end of this block, for jumping out *)
     (* there cannot be both a loop_label and a break_label *)
   }

 and instruction =
   | Block of block
   | Simple of I.instruction

 and cfg_scope =
  { cfg_letrec_label : int option;
    (* label of first function in "let rec" phrase to which this function
       belongs *)
    cfg_func_label : int;
    (* label of current function or "try" section, or 0 for the init block *)
    cfg_try_labels : int list;
    (* surrounding "try" sections, inner to outer *)
  }

type trap_info =
  | Trap_push of int
  | Trap_pop of int * int

type try_info =
  | Try_entry of int
  | Try_exit

(* try/catch:

   outer_1_label:
     Kpushtrap catch_label;
     cfg_trap = Trap_push try_label;
     cfg_succ = [ outer_2_label; catch_label ]
   try_label:
     cfg_try = Try_entry exit_label
     ... finally any jump to outer_2_label is replaced by new exit_label
   exit_label:
     cfg_try = Try_exit
     no instructions (symbolic node)
   outer_2_label:
     Kpoptrap;
     cfg_trap = Trap_pop(try_label, exit_label)
 *)

type cfg_node =
  { cfg_scope : cfg_scope;
    cfg_node_label : int;
    (* label of this sequence of non-jumping instructions *)
    (* if cfg_node_label = cfg_func_label, this node is the beginning of
       a function/init block *)
    cfg_try : try_info option;
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
  }

type cfg_context =
  { mutable nodes : cfg_node IMap.t;  (* by cfg_node_label *)
    mutable code : I.instruction array;
    mutable labels : ISet.t;          (* all labels *)
  }

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

let is_trapping instr =
  match instr with
    | I.Kpushtrap _ -> true
    | _ -> false

let create_context code labels =
  let labels = ref labels in
  let max_label = ref (Array.length code - 1) in
  let new_label () =
    let label = !max_label + 1 in
    labels := ISet.add label !labels;
    max_label := label;
    label in
  let nodes = ref IMap.empty in
  let todo = Queue.create() in
  let init_scope =
    { cfg_letrec_label = None;
      cfg_func_label = 0;
      cfg_try_labels = []
    } in
  Queue.add (init_scope, None, 0) todo;
  let add scope popfrom start =
    try
      let node = IMap.find start !nodes in
      if node.cfg_scope <> scope then
        failwith "bad scoping";
    with Not_found ->
      Queue.add (scope, popfrom, start) todo in
  while not (Queue.is_empty todo) do
    let (cfg_scope, popfrom, start) = Queue.take todo in
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
              cfg_try_labels = []
            } in
          add scope None flab
        )
        flabs
    done;
    let last_no_cont = not_continuing code.(start + !n - 1) in
    let last_clabs = if last_no_cont then [] else [start + !n] in
    let last_jlabs = jump_labels code.(start + !n - 1) in
    let cfg_succ = last_clabs @ last_jlabs in
    let is_pushtrap = is_trapping code.(start + !n - 1) in
    let is_poptrap = code.(start) = Kpoptrap in
    let cfg_final =
      if last_no_cont || is_pushtrap then
        None
      else
        Some (I.Kbranch (start + !n)) in
    let cfg_try =
      match cfg_scope.cfg_try_labels with
        | lab :: _ when lab = start ->
            let exit_label = new_label () in
            let exit_node =
              { cfg_scope;
                cfg_node_label = exit_label;
                cfg_try = Some Try_exit;
                cfg_trap = None;
                cfg_loops = [];  (* later *)
                cfg_succ = [];
                cfg_length = 0;
                cfg_final = None;
              } in
            nodes := IMap.add exit_label exit_node !nodes;
            Some (Try_entry exit_label)
        | _ -> None in
    let cfg_trap =
      if is_pushtrap then
        Some (Trap_push (start + !n))
      else if is_poptrap then
        match popfrom with
          | None -> assert false
          | Some lab ->
              (* Fix up cfg_succ in the corresponding Trap_push node: *)
              (* FIXME: this is crazy code *)
              let push_label =
                IMap.fold
                  (fun l n acc ->
                    if acc < 0 && n.cfg_trap = Some(Trap_push lab) then l else acc
                  )
                  !nodes
                  (-1) in
              assert(push_label >= 0);
              let push_node = IMap.find push_label !nodes in
              let push_node' =
                { push_node with
                  cfg_succ = start :: List.tl push_node.cfg_succ
                } in
              nodes := IMap.add push_label push_node' !nodes;
              let try_node = IMap.find lab !nodes in
              let exit_node =
                match try_node.cfg_try with
                  | Some (Try_entry l) -> l
                  | _ -> assert false in
              Some (Trap_pop (lab, exit_node))
      else
        None in
    let cfg_node =
      { cfg_scope;
        cfg_node_label = start;
        cfg_try;
        cfg_trap;
        cfg_loops = [];  (* later *)
        cfg_succ;
        cfg_length = !n;
        cfg_final;
      } in
    nodes := IMap.add start cfg_node !nodes;
    if not last_no_cont then (
      let next = start + !n in
      let scope =
        if is_pushtrap then
          { cfg_scope with cfg_try_labels = next :: cfg_scope.cfg_try_labels }
        else
          if code.(next) = Kpoptrap then
            { cfg_scope with cfg_try_labels = List.tl cfg_scope.cfg_try_labels }
          else
            cfg_scope in
      let popfrom =
        if code.(next) = Kpoptrap then
          Some (List.hd cfg_scope.cfg_try_labels)
        else
          None in
      add scope popfrom next
    );
    List.iter
      (fun lab ->
        add cfg_scope None lab
      )
      last_jlabs
  done;
  (* Fixup: all jumps to a Trap_pop are changed to the Try_exit node
     (except in the Trap_push node)
   *)
  let map_label lab =
    let n =
      try IMap.find lab !nodes
      with Not_found -> assert false in
    match n.cfg_trap with
      | Some (Trap_pop(_, exit_label)) ->
          exit_label
      | _ ->
          lab in
  nodes :=
    IMap.mapi
      (fun label node ->
        match node.cfg_trap with
          | Some (Trap_push _) ->
              (* there's only the Kpushtrap in it, which needs not to be
                 fixed up *)
              node
          | _ ->
              { node with
                cfg_succ = List.map map_label node.cfg_succ;
                cfg_final = ( match node.cfg_final with
                                | None -> None
                                | Some instr ->
                                    Some (Wc_reader.map_label_in_instr map_label instr)
                            );
              }
      )
      !nodes;
  let code = Array.map (Wc_reader.map_label_in_instr map_label) code in
  { nodes = !nodes;
    code;
    labels = !labels;
  }

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
    ) else if ISet.mem label exectrail then (
      (* already visited, and the node is in the execution trail => loop *)
      let old_trail =
        try IMap.find label !trails
        with Not_found -> assert false in
      ISet.iter
        (fun lab ->
          let n =
            try IMap.find lab ctx.nodes
            with Not_found -> assert false in
          if n.cfg_loops = [] || List.hd n.cfg_loops <> label then
            n.cfg_loops <- label :: n.cfg_loops
        )
        (ISet.diff exectrail old_trail)
    ) (* else: already visited => joining execution paths *)
    in
  IMap.iter
    (fun label node ->
        let is_entry =
          label = node.cfg_scope.cfg_func_label ||
            match node.cfg_try with
              | Some (Try_entry _) -> true
              | _ -> false in
      if is_entry then
        recurse ISet.empty label
    )
    ctx.nodes

    (*
let starts_try_section ctx label =
  let node =
    try IMap.find label ctx.nodes
    with Not_found -> assert false in
  node.cfg_try
     *)

let is_node_in_loop ctx loop_label label =
  let node =
    try IMap.find label ctx.nodes
    with Not_found -> assert false in
  List.mem loop_label node.cfg_loops

let recover_structure ctx =
  let visited = ref ISet.empty in
  let in_degree = ref IMap.empty in
  let rec inc_degree label =
    visited := ISet.add label !visited;
    let deg =
      try IMap.find label !in_degree
      with Not_found -> 0 in
    in_degree := IMap.add label (deg+1) !in_degree;
    if deg = 0 then (
      let node =
        try IMap.find label ctx.nodes
        with Not_found -> assert false in
      let eff_succ =
        List.filter
          (fun lab ->
            not (ISet.mem lab !visited)
          )
          node.cfg_succ in
      List.iter inc_degree eff_succ
    ) in
  let rec dec_degree label =
    visited := ISet.add label !visited;
    let deg =
      try IMap.find label !in_degree
      with Not_found -> assert false in
    in_degree := IMap.add label (deg-1) !in_degree;
    if deg = 1 then (
      let node =
        try IMap.find label ctx.nodes
        with Not_found -> assert false in
      let eff_succ =
        List.filter
          (fun lab ->
            not (ISet.mem lab !visited)
          )
          node.cfg_succ in
      ( [label] :: List.map dec_degree eff_succ )
      |> List.flatten
    ) else
      [] in

  let rec build_block loop_opt inner labels =
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
            |> Array.map (fun instr -> Simple instr)
            |> (fun a1 ->
              match node.cfg_final with
                | None -> a1
                | Some instr -> Array.append a1 [| Simple instr |]
            )
          in
          if loop_opt = node_loop || node_loop = None then
            let instructions =
              Array.append inner_instructions node_instructions in
            let break_label =
              match labels' with
                | [] -> None
                | lab :: _ ->
                    (* no break label when a loop follows *)
                    let n =
                      try IMap.find lab ctx.nodes
                      with Not_found -> assert false in
                    let n_loop =
                      match n.cfg_loops with
                        | [] -> None
                        | l :: _ -> Some l in
                    if node_loop = n_loop || n_loop = None then
                      Some lab
                    else
                      None in
            let inner' =
              [ { loop_label = None;
                  break_label;
                  instructions
                }
              ] in
            build_block node_loop inner' labels'
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
              build_block node_loop [] loop_labels in
            let loop_block =
              { loop_label = node_loop;
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
              [ { loop_label = None;
                  break_label;
                  instructions;
                }
              ] in
            build_block None inner' other_labels
      | [] ->
          ( match inner with
              | [ b ] -> b
              | _ ->
                  let instructions =
                    List.map (fun b -> Block b) inner
                    |> Array.of_list in
                  { loop_label = None;
                    break_label = None;
                    instructions
                  }
          ) in
  let functions =
    IMap.filter_map
      (fun label node ->
        let is_entry =
          label = node.cfg_scope.cfg_func_label ||
            match node.cfg_try with
              | Some (Try_entry _) -> true
              | _ -> false in
        if is_entry then (
          visited := ISet.empty;
          inc_degree label;
          visited := ISet.empty;
          let sorted_block_labels = dec_degree label in
          let block = build_block None [] sorted_block_labels in
          Some { scope = node.cfg_scope;
                 block
               }
        )
        else None
      )
      ctx.nodes in
  { functions }


