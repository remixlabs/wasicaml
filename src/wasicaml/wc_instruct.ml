open Wc_types

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

type instruction =
  | Wcomment of string
  | Wblock of { label:label; body:instruction list }
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
