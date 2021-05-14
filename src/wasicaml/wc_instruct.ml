open Wc_types
open Printf

type repr =
  | RValue
    (* a pointer to an OCaml block, or an OCaml integer when LSB=1 *)
  | RInt
    (* an I32 integer that needs to be converted to an OCaml integer *)
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

type store =
  | RealStack of int
    (* it's on the real OCaml stack at the given position, i.e. at fp[pos] *)
  | RealAccu
    (* stored in the accu variable as RValue *)
  | Const of int
    (* it's a constant *)
  | Local of repr * string
    (* stored in a local variable with the given name.
       There cannot be heap-allocated values in local variables, i.e.
       repr=RValue is forbidden.
     *)
  | Atom of int
     (* an atom with this tag *)

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


type global = Global of int
type label = Label of int | Loop of int
(* only local labels *)

type stack_descriptor =
  { stack_init : ISet.t;
    stack_depth : int;
    stack_save_accu : bool;
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
  | Wblock of { label:label; body:winstruction list }
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
                descr:stack_descriptor }
    (* up to 5 arga; dest is always RealAccu *)
  | Wccall_vector of { name:string; numargs:int; depth:int;
                       descr:stack_descriptor }
    (* more than 5 args; dest is always RealAccu *)
  | Wbranch of { label:label }
  | Wbranchif of { label:label; src:store }
  | Wbranchifnot of { label:label; src:store }
  | Wswitch of { labels_int:label array; labels_blk:label array; src:store }
  | Wapply of { numargs:int; depth:int; src:store }
  | Wappterm of { numargs:int; oldnumargs:int; depth:int } (* src=accu *)
  | Wreturn of { src:store }
  | Wgrab of { numargs:int }
  | Wclosurerec of { src:store list; dest:(store * int) list;
                     descr:stack_descriptor (* also "accu" can be used *)
                   }
  | Wraise of { src:store; kind:Lambda.raise_kind }
  | Wtrap of { trylabel:int; catchlabel:int; depth:int }
  | Wtryreturn of { src:store }
  | Wstop

let repr_of_store =
  function
  | RealStack _ -> RValue
  | Const _ -> RInt
  | Local(repr, _) -> repr
  | RealAccu -> RValue
  | Atom _ -> RValue

let empty_descr =
  { stack_init = ISet.empty;
    stack_depth = 0;
    stack_save_accu = false;
  }

let string_of_store =
  function
  | RealStack pos -> sprintf "fp[%d]" pos
  | RealAccu -> "accu"
  | Const k -> sprintf "%d" k
  | Local(repr, name) -> name
  | Atom k -> sprintf "atom%d" k

let string_label =
  function
  | Label k -> sprintf "label%d" k
  | Loop k -> sprintf "loop%d" k




let string_of_winstruction =
  function
  | Wcomment s -> s
  | Wblock arg ->
      ( match arg.label with
          | Label k ->
              sprintf "Wblock(..., label%d)" k
          | Loop k ->
              sprintf "Wblock(loop%d, ...)" k
      )
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
  | Wbranchif arg ->
      sprintf "Wbranch(%s if %s)"
              (string_label arg.label) (string_of_store arg.src)
  | Wbranchifnot arg ->
      sprintf "Wbranch(%s ifnot %s)"
              (string_label arg.label) (string_of_store arg.src)
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
      sprintf "Wapply(f=%s, args=[fp[%d]...fp[%d]])"
              (string_of_store arg.src)
              (-arg.depth) (-arg.depth+arg.numargs-1)
  | Wappterm arg ->
      sprintf "Wappterm(f=accu, args=[fp[%d]...fp[%d]], oldnum=%d)"
              (-arg.depth) (-arg.depth+arg.numargs-1)
              arg.oldnumargs
  | Wreturn arg ->
      sprintf "Wreturn(%s)" (string_of_store arg.src)
  | Wgrab arg ->
      sprintf "Wgrab(num=%d)" arg.numargs
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
  | Wtrap arg ->
      sprintf "Wtrap(try label%d catch label%d, depth=%d)"
              arg.trylabel arg.catchlabel arg.depth
  | Wtryreturn arg ->
      sprintf "Wtryreturn(%s)" (string_of_store arg.src)
  | Wstop ->
      "Wstop"
