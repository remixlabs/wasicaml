(* Copyright (C) 2021 by Figly, Inc.
   This code is free software, see the file LICENSE for details.
 *)

(* write the WAT-type S-expression as a .s file that is understood
   by LLVM
 *)

open Wc_sexp
open Printf

let rec ascii s =
  let b = Buffer.create 80 in
  Buffer.add_string b "\t.ascii \"";
  String.iteri
    (fun i c ->
      if i > 0 && i mod 32 = 0 then (
        Buffer.add_string b "\"\n\t.ascii \"";
      );
      if c = '"' || c = '\\' || Char.code c < 32 || Char.code c >= 127 then (
        (* (* this doesn't work because the assembler eats more than 2 hex digits *)
        Buffer.add_char b '\\';
        Buffer.add_char b 'x';
        Buffer.add_char b Wc_util.hexdigits.(Char.code c lsr 4);
        Buffer.add_char b Wc_util.hexdigits.(Char.code c land 15);
         *)
        (* octal *)
        Buffer.add_char b '\\';
        Buffer.add_char b Wc_util.hexdigits.(Char.code c lsr 6);
        Buffer.add_char b Wc_util.hexdigits.((Char.code c lsr 3) land 7);
        Buffer.add_char b Wc_util.hexdigits.(Char.code c land 7);
      ) else
        Buffer.add_char b c
    )
    s;
  Buffer.add_string b "\"\n";
  Buffer.contents b

let extract_func_type l =
  let rec scan params results l =
    match l with
      | (L [K "param"; K ty]) :: l' ->
          scan (ty :: params) results l'
      | (L [K "result"; K ty]) :: l' ->
          scan params (ty :: results) l'
      | other ->
          let llvm_type =
          sprintf "(%s) -> (%s)"
                  (String.concat ", " (List.rev params))
                  (String.concat ", " (List.rev results)) in
          (llvm_type, other) in
  scan [] [] l

let abbrev_empty_type s =
  if s = "() -> ()" then
    ""
  else
    s

let llvm_type_of_func_type typeuse =
  let (llvm_type, rest) = extract_func_type typeuse in
  if rest <> [] then
    failwith ("llvm_type_of_func_type: " ^ string_of_sexp (L rest));
  llvm_type

let extract_label l =
  match l with
    | (ID label) :: rest -> (label, rest)
    | _ -> ("<empty>", l)

let extract_load_store_params l =
  let rec recurse offset align l =
    match l with
      | (K tok) :: rest ->
          (try
             let offset = Scanf.sscanf tok "offset=%i" (fun x -> x) in
             recurse offset align rest
           with End_of_file | Scanf.Scan_failure _ ->
             (try
                let align = Scanf.sscanf tok "align=%i" (fun x -> x) in
                recurse offset align rest
              with End_of_file | Scanf.Scan_failure _ -> raise Not_found
             )
          )
      | _ :: _ -> raise Not_found
      | [] -> (offset, align) in
  recurse 0 0 l

let find_label name l =
  let rec recurse i l =
    match l with
      | x :: l' ->
          if name = x then i else recurse (i+1) l'
      | [] ->
          raise Not_found in
  recurse 0 l

let itable = Hashtbl.create 7

let indentation k =
  try
    Hashtbl.find itable k
  with
    | Not_found ->
        let s = String.make (k/8) '\t' ^ String.make (k mod 8) ' '  in
        Hashtbl.add itable k s;
        s

let write_func_body f func_name locals_table sexpl =
  let rec next labels depth sexpl =
    let bad() =
      failwith (sprintf "bad code - function %s - %s"
                        func_name (string_of_sexp (L sexpl))) in
    let indent = indentation (4*depth) in
    match sexpl with
      | L [ K ("local.get"|"local.set"|"local.tee" as instr);
            ID name
          ] :: rest ->
          let index =
            try Hashtbl.find locals_table name
            with Not_found ->
              failwith (sprintf "local not found - function %s local %s"
                                func_name name) in
          fprintf f "\t%s%s %d   # $%s\n" indent instr index name;
          next labels depth rest
      | L (K ("i32.load"|"i32.store"|"i64.load"|"i64.store"|
              "f32.load"|"f32.store"|"f64.load"|"f64.store"
                                                as instr) :: inner) :: rest ->
          let offset, align =
            try extract_load_store_params inner
            with Not_found -> bad() in
          fprintf f "\t%s%s 0x%x:p2align=%d\n" indent instr offset align;
          next labels depth rest
      | L (K ("i32.load8_u" | "i32.load8_s" | "i32.store8"
                                              as instr) ::  inner) :: rest ->
          let offset, align =
            try extract_load_store_params inner
            with Not_found -> bad() in
          if align <> 0 then
            bad();
          fprintf f "\t%s%s 0x%x\n" indent instr offset;
          next labels depth rest
      | L (K ("br"|"br_if"|"br_table" as instr) :: inner) :: rest ->
          let lab_names =
            List.map
              (function
               | ID lab -> lab
               | bad ->
                  failwith (sprintf "bad code - function %s - %s"
                                    func_name (string_of_sexp bad))
              )
              inner in
          let all_lab_names =
            lab_names |> String.concat "," in
          let lab_nums =
            List.map
              (fun name ->
                try find_label name labels
                with Not_found ->
                  failwith (sprintf "label not found - function %s label %s"
                                    func_name name)
              )
              lab_names in
          let branches =
            List.map string_of_int lab_nums |> String.concat ", " in
          let opt_curlies =
            if instr = "br_table" then
              "{ " ^ branches ^ " }"
            else
              branches in
          fprintf f "\t%s%s %s   # %s\n" indent instr opt_curlies all_lab_names;
          next labels depth rest
      | L (K ("call_indirect"|"return_call_indirect" as instr) :: N (I32 table) :: inner) :: rest ->
          let ty, inner2 = extract_func_type inner in
          if inner2 <> [] then
            failwith (sprintf "bad code - function %s - %s"
                              func_name (string_of_sexp (L sexpl)));
          fprintf f "\t%s%s %s\n" indent instr ty;
          next labels depth rest
      | L (K "block" :: inner) :: rest ->
          let label, inner2 = extract_label inner in
          let ty, body = extract_func_type inner2 in
          let ty = abbrev_empty_type ty in
          fprintf f "\t%sblock %s # label %s\n" indent ty label;
          next (label :: labels) (depth+1) body;
          fprintf f "\t%send_block # label %s\n" indent label;
          next labels depth rest
      | L (K "loop" :: inner) :: rest ->
          let label, inner2 = extract_label inner in
          let ty, body = extract_func_type inner2 in
          let ty = abbrev_empty_type ty in
          fprintf f "\t%sloop %s # label %s\n" indent ty label;
          next (label :: labels) (depth+1) body;
          fprintf f "\t%send_loop # label %s\n" indent label;
          next labels depth rest
      | L (K "if" :: inner) :: rest ->
          let label, inner2 = extract_label inner in
          let ty, body = extract_func_type inner2 in
          let ty = abbrev_empty_type ty in
          ( match body with
              | [ L (K "then" :: then_sexpl) ] ->
                  fprintf f "\t%sif %s # label %s\n" indent ty label;
                  next (label :: labels) (depth+1) then_sexpl;
                  fprintf f "\t%send_if # label %s\n" indent label;
              | [ L (K "then" :: then_sexpl); L (K "else" :: else_sexpl) ] ->
                  fprintf f "\t%sif %s # label %s\n" indent ty label;
                  next (label :: labels) (depth+1) then_sexpl;
                  fprintf f "\t%selse\n" indent;
                  next (label :: labels) (depth+1) else_sexpl;
                  fprintf f "\t%send_if # label %s\n" indent label;
              | _ ->
                  failwith (sprintf "bad code - function %s - %s"
                                    func_name (string_of_sexp (L body)))
          );
          next labels depth rest
      | L [ K instr ] :: rest ->
          fprintf f "\t%s%s\n" indent instr;
          next labels depth rest
      | L [ K instr; N (I32 x) ] :: rest ->
          fprintf f "\t%s%s 0x%lx\n" indent instr x;
          next labels depth rest
      | L [ K instr; N (I64 x) ] :: rest ->
          fprintf f "\t%s%s 0x%Lx\n" indent instr x;
          next labels depth rest
      | L [ K instr; N (F64 x) ] :: rest ->
          fprintf f "\t%s%s %h\n" indent instr x;
          next labels depth rest
      | L [ K instr; ID name ] :: rest ->
          fprintf f "\t%s%s %s\n" indent instr name;
          next labels depth rest
      | _ :: _ ->
          bad()
      | [] ->
          () in
  next [] 0 sexpl

let rec remove_stuff l =
  let rec f =
    function
     | BR -> None
     | C _ -> None
     | L l -> Some (L (remove_stuff l))
     | other -> Some other in
  List.filter_map f l

let write_file f filename sexpl =
  let import sym mod_name obj_name =
    fprintf f "\t.import_module %s, %s\n" sym mod_name;
    fprintf f "\t.import_name %s, %s\n" sym obj_name in
  let export sym obj_name =
    fprintf f "\t.export_name %s, %s\n" sym obj_name in
  let global glb_name w_type =
    let glb_type, is_mut =
      ( match w_type with
          | L [K "mut"; K ty] :: _ ->
              ty, true
          | K ty :: _ ->
              ty, false
          | _ ->
              failwith (sprintf "bad global %s" glb_name)
      ) in
    (* llvm-11 seems not to support "immutable" but it is already in master *)
    fprintf f "\t.globaltype %s,%s%s\n"
            glb_name glb_type
            ""
  (* (if is_mut then "" else ",immutable") *) in
  let datasection name =
    fprintf f "\t.section .rodata.%s,\"\",@\n" name;
    fprintf f "\t.hidden %s\n" name;
    fprintf f "\t.type %s,@object\n" name;
    fprintf f "%s:\n" name in
  let emitdata l =
    let have_section = ref None in
    let size_section = ref 0 in
    let print_size() =
      match !have_section with
        | Some name ->
            fprintf f "\t.size %s,%d\n" name !size_section
        | None ->
            () in
    List.iter
      (function
       | L (K "memory" :: _) -> ()
       | L (K "offset" :: _) -> ()
       | ID name ->
           print_size();
           datasection name;
           have_section := Some name
       | S data ->
           ( match !have_section with
               | None ->
                   print_size();
                   datasection "__anondata";
                   have_section := Some "__anondata"
               | Some _ ->
                   ()
           );
           output_string f (ascii data);
           size_section := !size_section + String.length data
       | _ ->
           failwith ("cannot decode 'data'")
      )
      l;
  print_size() in

  let rec func_1 func_name exp_name_opt params results locals sexpl =
    match sexpl with
      | L [ K "export"; S n ] :: rest ->
          func_1 func_name (Some n) params results locals rest
      | L [ K "param"; ID n; K ty ] :: rest ->
          func_1 func_name exp_name_opt ((n,ty)::params) results locals rest
      | L [ K "result"; K ty ] :: rest ->
          func_1 func_name exp_name_opt params (ty::results) locals rest
      | L [ K "local"; ID n; K ty ] :: rest ->
          func_1 func_name exp_name_opt params results ((n,ty)::locals) rest
      | body ->
          fprintf f "\n\t.section .text.%s,\"\",@\n" func_name;
          ( match exp_name_opt with
              | None ->
                  fprintf f "\t.hidden %s\n" func_name
              | Some exp_name ->
                  export func_name exp_name
          );
          fprintf f "\t.globl %s\n" func_name;
          fprintf f "\t.type %s,@function\n" func_name;
          fprintf f "%s:\n" func_name;
          fprintf f "\t.functype %s (%s) -> (%s)\n"
                  func_name
                  (List.rev params |> List.map snd |> String.concat ", ")
                  (List.rev results |> String.concat ", ");
          if locals <> [] then
            fprintf f "\t.local %s\n"
                    (List.rev locals |> List.map snd |> String.concat ", ");
          let locals_table = Hashtbl.create 7 in
          Array.iteri
            (fun i (n,_) ->
              Hashtbl.add locals_table n i
            )
            (Array.append
               (List.rev params |> Array.of_list)
               (List.rev locals |> Array.of_list));
          write_func_body f func_name locals_table body;
          fprintf f "\tend_function\n" in
  let func func_name sexpl =
    match sexpl with
      | K "export" :: S obj_name :: rest ->
          func_1 func_name (Some obj_name) [] [] [] rest
      | _ ->
          func_1 func_name None [] [] [] sexpl in
  let rec func_type_1 func_name params results sexpl =
    match sexpl with
      | L [ K "export"; S n ] :: rest ->
          func_type_1 func_name params results rest
      | L [ K "param"; ID n; K ty ] :: rest ->
          func_type_1 func_name ((n,ty)::params) results rest
      | L [ K "result"; K ty ] :: rest ->
          func_type_1 func_name params (ty::results) rest
      | L [ K "local"; ID n; K ty ] :: rest ->
          func_type_1 func_name params results rest
      | body ->
          fprintf f "\t.functype %s (%s) -> (%s)\n"
                  func_name
                  (List.rev params |> List.map snd |> String.concat ", ")
                  (List.rev results |> String.concat ", ") in
  let func_type func_name sexpl =
    match sexpl with
      | K "export" :: S _ :: rest ->
          func_type_1 func_name [] [] rest
      | _ ->
          func_type_1 func_name [] [] sexpl in

  let sexpl1 = remove_stuff sexpl in

  fprintf f "\t.text\n";
  fprintf f "\t.file %S\n" filename;
  List.iter
    (fun mod_sexp ->
      match mod_sexp with
        | L [ K "import"; S mod_name; S obj_name; L imp_sexpl ] ->
            ( match imp_sexpl with
                | (K "memory") :: _ ->
                    (* ignored, memory is handled by the linker *)
                    ()
                | (K "table") :: _ ->
                    (* ignored, table is handled by the linker *)
                    ()
                | (K "func") :: (ID func_name) :: w_type ->
                    let func_type = llvm_type_of_func_type w_type in
                    fprintf f "\t.functype %s %s\n" func_name func_type;
                    import func_name mod_name obj_name
                | (K "global") :: (ID glb_name) :: w_type ->
                    global glb_name w_type;
                    import glb_name mod_name obj_name
                | _ ->
                    failwith ("write_file: bad import: " ^
                                string_of_sexp (L imp_sexpl))
            )
        | L (K "func" :: ID func_name :: rest) ->
            (* llvm-16 requires that the .functype directive precedes
               any call to the function
             *)
            func_type func_name rest
        | _ ->
            ()
    )
    sexpl1;
  List.iter
    (fun mod_sexp ->
      match mod_sexp with
        | L [ K "import"; _; _; _ ] ->
            ()
        | L (K ("memory" | "table") :: _) ->
            ()
        | L (K "func" :: ID func_name :: rest) ->
            func func_name rest
        | L ( K "global" :: (ID glb_name) :: w_type) ->
            (* TODO: inline export *)
            (* FIXME: no initializer supported by llvm *)
            global glb_name w_type;
            fprintf f "%s:\n" glb_name;
        | L  (K "data" :: inner) ->
            emitdata inner
        | L [ K "export"; ID obj_name; L descr ] ->
            ( match descr with
                | [ K ("func" | "global"); ID sym_name ] ->
                    export sym_name obj_name
                | (K ("memory" | "table") :: _) ->
                    ()
                | _ ->
                    failwith ("write_file: bad export: " ^
                                string_of_sexp (L descr))
            )
        | _ ->
            failwith ("write_file: bad definition: " ^
                        string_of_sexp mod_sexp)
    )
    sexpl1
