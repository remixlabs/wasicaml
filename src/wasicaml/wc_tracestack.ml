open Printf
open Wc_types

(* So far, only simple things:
   - figure out the maximum size of the OCaml stack, to get a good value
     for bp
 *)

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
    | Kpushtrap _ | Kpoptrap -> depth
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
      let depth' = trace_stack_instr depth instrs.(k) in
      recurse (max mdepth depth') depth' (k+1)
    else
      mdepth in
  recurse 0 0 0

let local_branch_labels =
  function
  | I.Kbranch l -> [l]
  | Kbranchif l -> [l]
  | Kbranchifnot l -> [l]
  | Kswitch (la1,la2) -> Array.to_list la1 @ Array.to_list la2
  | _ -> []

let rec dump block indent =
  let open Wc_control in
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
     | Trap { trylabel; catchlabel; poplabel } ->
         eprintf "%sTrap try=%d catch=%d pop=%s\n" istr trylabel catchlabel
                 (Option.map string_of_int poplabel |> Option.value ~default:"")
     | TryReturn ->
         eprintf "%sTryReturn\n" istr
     | Block inner ->
         dump inner (indent+1)
    )
    block.instructions

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
            dump fblock.block 0;
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
              let depth' = trace_stack_instr depth i in
              (max depth' max_depth, depth')
          | Trap { catchlabel; poplabel=Some pop } ->
              update_depth_table depth [ catchlabel; pop ];
              (max_depth, depth)
          | Trap { catchlabel; poplabel=None } ->
              update_depth_table depth [ catchlabel ];
              (max_depth, depth)
          | TryReturn ->
              (max_depth, depth)
          | Block inner ->
              recurse inner
      )
      (0, 0)
      block.instructions in
  recurse fblock.block |> fst
