open Printf
open Wc_number

type sexp =
  | K of string (* keyword *)
  | ID of string (* $id *)
  | N of number
  | S of string (* "string" *)
  | C of string  (* (;comment;) *)
  | BR           (* suggestion for first break *)
  | L of sexp list

let itable = Hashtbl.create 7

let indentation k =
  try
    Hashtbl.find itable k
  with
    | Not_found ->
        let s = String.make (k/8) '\t' ^ String.make (k mod 8) ' '  in
        Hashtbl.add itable k s;
        s

let rec string_of_sexp sexp =
  match sexp with
    | K x -> x
    | ID x -> "$" ^ x
    | S x -> "\"" ^ x ^ "\""   (* FIXME *)
    | N x -> string_of_number x
    | C x -> sprintf "(;%s;)" x
    | BR -> ""
    | L l ->
        let l = List.filter (fun e -> e <> BR) l in
        "(" ^ string_of_sexp_list l ^ ")"
and string_of_sexp_list l =
  List.map string_of_sexp l |> String.concat " "

let fits_onto_line width sexp =
  let rec single width sexp =
    match sexp with
      | K _ | ID _ | S _ | N _ | C _ | BR ->
          string_of_sexp sexp |> String.length
      | L l ->
          if width >= 2 then
            list (width-2) l + 2
          else
            width+1
  and list width l =
    match l with
      | BR :: l' ->
          list width l'
      | x :: l' ->
          let len = single width x in
          let len = (if l' <> [] then len+1 else len) in
          if len >= width then
            width+1
          else
            list (width-len) l' + len
      | [] ->
          0 in
  single width sexp <= width

let break l =
  let rec recurse acc l =
    match l with
      | BR :: l' ->
          (List.rev acc, l')
      | x :: l' ->
          recurse (x :: acc) l'
      | [] ->
          ([], List.rev acc) in
  recurse [] l

let rec print_indented f indent width sexp =
  match sexp with
    | K _ | ID _ | S _ | N _ | C _ | BR ->
        fprintf f "%s%s" (indentation indent) (string_of_sexp sexp)
    | L l ->
        if width > indent && fits_onto_line (width - indent) sexp then
          fprintf f "%s%s" (indentation indent) (string_of_sexp sexp)
        else
          let l_nobreak, l_break = break l in
          let first_line, tail =
            match l_nobreak, l_break with
              | [], [] -> [], []
              | [], (x::l) -> [x], l
              | _ -> l_nobreak, l_break in
          fprintf f "%s(%s"
                  (indentation indent) (string_of_sexp_list first_line);
          List.iter
            (fun elem ->
              fprintf f "\n";
              print_indented f (indent+2) width elem
            )
            tail;
          fprintf f ")"
