(* Copyright (C) 2021 by Figly, Inc.
   This code is free software, see the file LICENSE for details.
 *)

open Wc_types
open Printf

type repr =
  | RValue
    (* a pointer to an OCaml block, or an OCaml integer when LSB=1 *)
  | RInt
    (* an I32 integer that needs to be converted to an OCaml integer.
       This is a clean 31 bit value with bit 30 = Bit 31.
     *)
  | RIntUnclean
    (* an I32 integer that needs to be converted to an OCaml integer.
       Here, the MSB (sign bit) is bit 30, and bit 31 is random.
     *)
  | RIntVal
    (* an I32 integer that is already converted to an OCaml integer *)
  | RNatInt
    (* an I32 integer that will be stored as nativeint block *)
  | RInt32
    (* an I32 integer that will be stored as int32 block *)
  | RInt64
    (* an I64 integer that will be stored as int64 block *)
  | RFloat
    (* an F64 number that will be stored as float block *)

type global_lookup =
  | Glb of int   (* the global is only accessble via this global *)
  | Env of int   (* the global is also in the local environment here *)

type store =
  | RealStack of int
    (* it's on the real OCaml stack at the given position, i.e. at fp[pos] *)
  | RealAccu of { no_function : bool }
    (* stored in the accu variable as RValue.
       no_function: if true, it is guaranteed that the value is not a closure.
       So far used for optimizing returns after C calls.
     *)
  | Const of int
    (* it's a constant *)
  | LocalPos of int
    (* it's in the local variable corresponding to RealStack. The name is
       arg%d if the position is positive (function arguments), and
       pos(-%d) otherwise (local stack positions). Representation is RValue.
     *)
  | Local of repr * string
    (* stored in a local variable with the given name.
       There cannot be heap-allocated values in local variables, i.e.
       repr=RValue is forbidden.
     *)
  | Atom of int
    (* an atom with this tag *)
  | TracedGlobal of global_lookup * int list * Wc_traceglobals.initvalue
    (* TracedGlobal(glb, path, value, env_opt): the global with index [glb],
       and here the value in the subblock as denoted by [path]. This position is
       known to contain [value].
     *)
  | Invalid
    (* an uninitialized value *)

(* how to include unboxing for float/int32/int64:

   Combined of { repr : repr;
                 local : string;
                 stack : int;
                 local_used : bool ref;
                 stack_used : bool ref
               }

   Code can be produced that stores the value in [local] (with representation
   [repr]) and on the stack (as RValue). The former is done when [local_used]
   is to true by some user, and latter when [stack_used] gets true. If there
   is no code that sets [stack_used] the allocation can be avoided. If there
   is such code, the allocation is only done once right at the beginning.

   With Combined we could even put RValues into local variables. Just add
     local_incompat : bool ref
   and set this when an incompatibility is found.
 *)

(* maybe also add RBool as representations - with only 0 and 1 as values,
   after a comparison. Converting RBool to RValue is a bit cheaper
   (can use "select" instruction)
 *)

type global = Global of int
type label = Label of int | Loop of int
(* only local labels *)

type stack_descriptor =
  { stack_uninit : int list;
    (* List of uninitialized stack positions (in the range -stack_depth to -1),
       in ascending order *)
    stack_depth : int;
    (* Depth of the stack - ignoring the uninitialized block at the top *)
    stack_save_accu : bool;
    (* whether to save the accu before the GC *)
    stack_save_locals : ISet.t;
    (* Which local variables (pos%d and arg%d) must be saved to the stack
       before GC *)
  }

type unop =
  | Pnegint
  | Pboolnot
  | Poffsetint of int
  | Pisint
  | Pgetfield of int
  | Pgetfloatfield of int
  | Pvectlength
  | Pgetpubmet of int

type uneffect =
  | Poffsetref of int
  | Psetglobal of global

type binop =
  | Paddint
  | Psubint
  | Pmulint
  | Pdivint
  | Pmodint
  | Pandint
  | Porint
  | Pxorint
  | Plslint
  | Plsrint
  | Pasrint
  | Pintcomp of Lambda.integer_comparison
  | Puintcomp of Lambda.integer_comparison
  | Pgetvectitem
  | Pgetstringchar
  | Pgetbyteschar
  | Pgetmethod
  | Pgetdynmet

type bineffect =
  | Psetfield of int
  | Psetfloatfield of int

type terneffect =
  | Psetvectitem
  | Psetbyteschar

type winstruction =
  | Wcomment of string
  | Wblock of { label:label; scope:Wc_control.cfg_scope; body:winstruction list }
  | Wcond of { cond:bool ref; ontrue:winstruction; onfalse:winstruction }
  | Wcopy of { src:store; dest:store }   (* only non-allocating copies *)
  | Walloc of { src:store; dest:store; descr:stack_descriptor }
  | Wenv of { field:int }         (* dest is always RealAccu *)
  | Wcopyenv of { offset:int }    (* dest is always RealAccu *)
  | Wgetglobal of { src:global }  (* dest is always RealAccu *)
  | Wunary of { op:unop; src1:store; dest:store }
  | Wunaryeffect of { op:uneffect; src1:store }
  | Wbinary of { op:binop; src1:store; src2:store; dest:store }
  | Wbinaryeffect of { op:bineffect; src1:store; src2:store }
  | Wternaryeffect of { op:terneffect; src1:store; src2:store; src3:store }
  | Wmakeblock of { tag:int; src:store list;
                    descr:stack_descriptor  }  (* dest is always RealAccu *)
  | Wmakefloatblock of { src:store list;
                         descr:stack_descriptor  } (* dest is always RealAccu *)
  | Wccall of { name:string; src:store list;
                descr:stack_descriptor option }
    (* up to 5 args; dest is always RealAccu *)
  | Wccall_vector of { name:string; numargs:int; depth:int;
                       descr:stack_descriptor }
    (* more than 5 args; dest is always RealAccu *)
  | Wbranch of { label:label }
  | Wif of { src:store; neg:bool; body:winstruction list }
  | Wswitch of { labels_int:label array; labels_blk:label array; src:store }
  | Wapply of { numargs:int; depth:int }  (* src=accu *)
  | Wapply_direct of { funlabel:int; global:global_lookup; path:int list;
                       numargs:int; depth:int }
  | Wappterm of { numargs:int; oldnumargs:int; depth:int } (* src=accu *)
  | Wappterm_args of { argsrc:store list; funsrc:store;
                       oldnumargs:int; depth:int }
    (* Wappterm_args is only used when tail-calls are enabled *)
  | Wappterm_direct of { funlabel:int; global:global_lookup; path:int list;
                         numargs:int; oldnumargs:int; depth:int } (* src=accu *)
    (* Wappterm_direct is only used when tail-calls are enabled *)
  | Wappterm_direct_args of { funlabel:int; global:global_lookup; path:int list;
                              argsrc:store list; funsrc:store;
                              oldnumargs:int; depth:int }
    (* Wappterm_direct_args is only used when tail-calls are enabled *)
  | Wreturn of { src:store; arity:int }
  | Wgrab of { arity:int }
  | Wclosurerec of { src:store list; dest:(store * int) list;
                     descr:stack_descriptor (* also "accu" can be used *)
                   }
  | Wraise of { src:store; kind:Lambda.raise_kind }
  | Wtryreturn of { src:store }
  | Wnextmain of { label:int }
  | Wstop
  | Wnop
  | Wunreachable

let repr_comparable_as_i32 r1 r2 =
  match r1, r2 with
    | (RValue | RIntVal), (RValue | RIntVal) -> true
    | (RInt | RNatInt | RInt32), (RInt | RNatInt | RInt32) -> true
    | _ -> false

let repr_of_store =
  function
  | RealStack _ | LocalPos _ -> RValue
  | Const _ -> RInt
  | Local(repr, _) -> repr
  | RealAccu _ -> RValue
  | Atom _ -> RValue
  | TracedGlobal _ -> RValue
  | Invalid -> assert false

(* whether an allocation is needed in order to turn this represnetaion
   into a proper OCaml value
 *)
let repr_needs_alloc =
  function
  | RNatInt | RInt32 | RInt64 | RFloat ->
      true
  | _ ->
      false

let empty_descr =
  { stack_uninit = [];
    stack_depth = 0;
    stack_save_accu = false;
    stack_save_locals = ISet.empty;
  }

let string_of_localPos pos =
  if pos <= 0 then sprintf "pos%d" (-pos) else sprintf "arg%d" pos

let string_of_store =
  function
  | RealStack pos -> sprintf "fp[%d]" pos
  | RealAccu _ -> "accu"
  | LocalPos pos -> string_of_localPos pos
  | Const k -> sprintf "%d" k
  | Local(repr, name) -> name
  | Atom k -> sprintf "atom%d" k
  | TracedGlobal(glb,path,_) ->
      let s_glb =
        match glb with
          | Glb i -> sprintf "global%d" i
          | Env i -> sprintf "env+%d" i in
      let s_path =
        if path = [] then "" else
          List.map (fun d -> sprintf "[%d]" d) path |> String.concat "" in
      sprintf "%s%s" s_glb s_path
  | Invalid -> "invalid"

let string_label =
  function
  | Label k -> sprintf "label%d" k
  | Loop k -> sprintf "loop%d" k


let extract_directly_callable_function st =
  match st with
    | TracedGlobal(glb, path, FuncInEnv {func_offset; env}) ->
        (* eprintf "EXTRACT: %s\n%!" (string_of_store st); *)
        let func = env.(func_offset) in
        ( match func with
            | Function { label } ->
                Some (glb, path, label, env)
            | _ ->
                (* FIXME: occasionally we get here *)
                (* assert false *)
                None
        )
    | _ ->
        None

let rec string_of_winstruction =
  function
  | Wcomment s -> s
  | Wblock arg ->
      ( match arg.label with
          | Label k ->
              sprintf "Wblock(..., label%d)" k
          | Loop k ->
              sprintf "Wblock(loop%d, ...)" k
      )
  | Wcond { cond; ontrue; onfalse } ->
      string_of_winstruction (if !cond then ontrue else onfalse)
  | Wcopy arg ->
      sprintf "Wcopy(%s = %s)"
              (string_of_store arg.dest) (string_of_store arg.src)
  | Walloc arg ->
      sprintf "Walloc(%s = %s)"
              (string_of_store arg.dest) (string_of_store arg.src)
  | Wenv arg ->
      sprintf "Wenv(accu = env[%d])" arg.field
  | Wcopyenv arg ->
      sprintf "Wcopyenv(accu = env+%d)" arg.offset
  | Wgetglobal { src = Global k } ->
      sprintf "Wgetglobal(accu = global%d)" k
  | Wunary arg ->
      sprintf "Wunary(%s = f(%s))"
              (string_of_store arg.dest) (string_of_store arg.src1)
  | Wunaryeffect arg ->
      sprintf "Wunaryeffect(f(%s))"
              (string_of_store arg.src1)
  | Wbinary arg ->
      sprintf "Wbinary(%s = f(%s, %s))"
              (string_of_store arg.dest) (string_of_store arg.src1)
              (string_of_store arg.src2)
  | Wbinaryeffect arg ->
      sprintf "Wbinaryeffect(f(%s, %s))"
              (string_of_store arg.src1)
              (string_of_store arg.src2)
  | Wternaryeffect arg ->
      sprintf "Wbinaryeffect(f(%s, %s, %s))"
              (string_of_store arg.src1)
              (string_of_store arg.src2)
              (string_of_store arg.src3)
  | Wmakeblock arg ->
      sprintf "Wmakeblock(tag=%d, accu=[%s])"
              arg.tag
              (List.map string_of_store arg.src |> String.concat ", ")
  | Wmakefloatblock arg ->
      sprintf "Wmakefloatblock(accu=[%s])"
              (List.map string_of_store arg.src |> String.concat ", ")
  | Wccall arg ->
      sprintf "Wccall(accu = %S(%s))"
              arg.name
              (List.map string_of_store arg.src |> String.concat ", ")
  | Wccall_vector arg ->
      sprintf "Wccall_vector(accu = %S(fp[%d]...fp[%d]))"
              arg.name (-arg.depth) (-arg.depth+arg.numargs-1)
  | Wbranch arg ->
      sprintf "Wbranch(%s)" (string_label arg.label)
  | Wif arg ->
      sprintf "Wif(if%s %s -> %s )"
              (if arg.neg then "not" else "")
              (string_of_store arg.src)
              (List.map string_of_winstruction arg.body |> String.concat ", ")
  | Wswitch arg ->
      let s1 =
        Array.to_list arg.labels_int
        |> List.map string_label
        |> String.concat "," in
      let s2 =
        Array.to_list arg.labels_blk
        |> List.map string_label
        |> String.concat "," in
      sprintf "Wswitch(%s;%s on %s)" s1 s2 (string_of_store arg.src)
  | Wapply arg ->
      sprintf "Wapply(f=accu, args=[fp[%d]...fp[%d]])"
              (-arg.depth) (-arg.depth+arg.numargs-1)
  | Wapply_direct arg ->
      sprintf "Wapply_direct(f=letrec%d, args=[fp[%d]...fp[%d]])"
              arg.funlabel
              (-arg.depth) (-arg.depth+arg.numargs-1)
  | Wappterm arg ->
      sprintf "Wappterm(f=accu, args=[fp[%d]...fp[%d]], oldnum=%d)"
              (-arg.depth) (-arg.depth+arg.numargs-1)
              arg.oldnumargs
  | Wappterm_args arg ->
      sprintf "Wappterm_args(f=%s, args=[%s], oldnum=%d)"
              (string_of_store arg.funsrc)
              (List.map string_of_store arg.argsrc |> String.concat ", ")
              arg.oldnumargs
  | Wappterm_direct arg ->
      sprintf "Wappterm(f=letrec%d, args=[fp[%d]...fp[%d]], oldnum=%d)"
              arg.funlabel (-arg.depth) (-arg.depth+arg.numargs-1)
              arg.oldnumargs
  | Wappterm_direct_args arg ->
      sprintf "Wappterm_args(f=letrec%d, args=[%s], oldnum=%d)"
              arg.funlabel
              (List.map string_of_store arg.argsrc |> String.concat ", ")
              arg.oldnumargs
  | Wreturn arg ->
      sprintf "Wreturn(%s)" (string_of_store arg.src)
  | Wgrab arg ->
      sprintf "Wgrab(arity=%d)" arg.arity
  | Wclosurerec arg ->
      sprintf "Wclosurerec(env=[%s], dest=[%s])"
              (List.map string_of_store arg.src |> String.concat ", ")
              (List.map
                 (fun (s, lab) ->
                   sprintf "%s=label%d" (string_of_store s) lab
                 )
                 arg.dest
               |> String.concat ",")
  | Wraise arg ->
      sprintf "Wraise(%s)" (string_of_store arg.src)
  | Wtryreturn arg ->
      sprintf "Wtryreturn(%s)" (string_of_store arg.src)
  | Wnextmain arg ->
      sprintf "Wnextmain(letrec_main%d)" arg.label
  | Wstop ->
      "Wstop"
  | Wnop ->
      "Wnop"
  | Wunreachable ->
      "Wunreachable"
