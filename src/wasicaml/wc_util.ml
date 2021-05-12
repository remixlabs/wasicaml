open Wc_types
open Printf

let string_of_raise_kind =
  function
  | Lambda.Raise_regular -> "regular"
  | Raise_reraise -> "reraise"
  | Raise_notrace -> "notrace"

let string_of_int_comparison =
  function
  | Lambda.Ceq -> "eq"
  | Cne -> "ne"
  | Clt -> "lt"
  | Cgt -> "gt"
  | Cle -> "le"
  | Cge -> "ge"

let string_of_instruction =
  function
  | I.Klabel label -> sprintf "Klabel(label%d)" label
  | Kacc k -> sprintf "Kacc(%d)" k
  | Kenvacc k -> sprintf "Kenvacc(%d)" k
  | Kpush -> "Kpush"
  | Kpop k -> sprintf "Kpop(%d)" k
  | Kassign k -> sprintf "Kassign(%d)" k
  | Kpush_retaddr label -> sprintf "Kpush_retaddr(label%d)" label
  | Kapply k -> sprintf "Kapply(%d)" k
  | Kappterm(k1,k2) -> sprintf "Kappterm(%d,%d)" k1 k2
  | Kreturn k -> sprintf "Kreturn(%d)" k
  | Krestart -> "Krestart"
  | Kgrab k -> sprintf "Kgrab(%d)" k
  | Kclosure(label, k) -> sprintf "Kclosure(label%d,%d)" label k
  | Kclosurerec(labels, k) ->
      let s = List.map (sprintf "label%d") labels |> String.concat "," in
      sprintf "Kclosurerec(%s;%d)" s k
  | Koffsetclosure k -> sprintf "Koffsetclosure(%d)" k
  | Kgetglobal id -> sprintf "Kgetglobal(%s)" (Ident.name id)
  | Ksetglobal id -> sprintf "Ksetglobal(%s)" (Ident.name id)
  | Kconst (Lambda.Const_base (Asttypes.Const_int k)) ->
      sprintf "Kconst(%d)" k
  | Kconst _ ->
      "Kconst(undecodable)"
  | Kmakeblock(k1,k2) -> sprintf "Kmakeblock(%d,%d)" k1 k2
  | Kmakefloatblock k -> sprintf "Kmakefloatblock(%d)" k
  | Kgetfield k -> sprintf "Kgetfield(%d)" k
  | Ksetfield k -> sprintf "Ksetfield(%d)" k
  | Kgetfloatfield k -> sprintf "Kgetfloatfield(%d)" k
  | Ksetfloatfield k -> sprintf "Ksetfloatfield(%d)" k
  | Kvectlength -> "Kvectlength"
  | Kgetvectitem -> "Kgetvectitem"
  | Ksetvectitem -> "Ksetvectitem"
  | Kgetstringchar -> "Kgetstringchar"
  | Kgetbyteschar -> "Kgetbyteschar"
  | Ksetbyteschar -> "Ksetbyteschar"
  | Kbranch label -> sprintf "Kbranch(label%d)" label
  | Kbranchif label -> sprintf "Kbranchif(label%d)" label
  | Kbranchifnot label -> sprintf "Kbranchifnot(label%d)" label
  | Kstrictbranchif _ -> assert false
  | Kstrictbranchifnot _ -> assert false
  | Kswitch(labels1, labels2) ->
      let s1 =
        Array.to_list labels1
        |> List.map (sprintf "label%d")
        |> String.concat "," in
      let s2 =
        Array.to_list labels2
        |> List.map (sprintf "label%d")
        |> String.concat "," in
      sprintf "Kswitch(%s;%s)" s1 s2
  | Kboolnot -> "Kboolnot"
  | Kpushtrap label -> sprintf "Kpushtrap(label%d)" label
  | Kpoptrap -> "Kpoptrap"
  | Kraise kind -> sprintf "Kraise(%s)" (string_of_raise_kind kind)
  | Kcheck_signals -> "Kcheck_signals"
  | Kccall(name, k) -> sprintf "Kccall(%s,%d)" name k
  | Knegint -> "Knegint"
  | Kaddint -> "Kaddint"
  | Ksubint -> "Ksubint"
  | Kmulint -> "Kmulint"
  | Kdivint -> "Kdivint"
  | Kmodint -> "Kmodint"
  | Kandint -> "Kandint"
  | Korint -> "Korint"
  | Kxorint -> "Kxorint"
  | Klslint -> "Klslint"
  | Klsrint -> "Klsrint"
  | Kasrint -> "Kasrint"
  | Kintcomp cmp -> sprintf "Kintcomp(%s)" (string_of_int_comparison cmp)
  | Koffsetint k -> sprintf "Koffsetint(%d)" k
  | Koffsetref k -> sprintf "Koffsetref(%d)" k
  | Kisint -> "Kistin"
  | Kisout -> "Kisout"
  | Kgetmethod -> "Kgetmethod"
  | Kgetpubmet k -> sprintf "Kgetpubmet(%d)" k
  | Kgetdynmet -> "Kgetdynmet"
  | Kevent _ -> "Kevent"
  | Kstop -> "Kstop"

let hexdigits = [| '0'; '1'; '2'; '3'; '4'; '5'; '6'; '7';
                   '8'; '9'; 'a'; 'b'; 'c'; 'd'; 'e'; 'f' |]

let rec enum k n =
  if n > 0 then
    k :: enum (k+1) (n-1)
  else
    []

let rec list_prefix n l =
  if n=0 then
    []
  else
    match l with
      | x :: l' -> x :: (list_prefix (n-1) l')
      | [] -> assert false

              
