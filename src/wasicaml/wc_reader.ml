open Printf
open Wc_types

type executable =
  { dll_paths : string list;
    dll_names : string list;
    primitives : string array;
    code : string;
    data : Obj.t array;
    symbols : Symtable.global_map;
  }

let input_stringlist f section =
  try
    let s = Bytesections.read_section_string f section in
    let l = String.split_on_char '\000' s in
    List.filter (fun elem -> elem <> "") l
  with
    | Not_found -> []

let input_objarray f section : Obj.t array =
  try
    let _len = Bytesections.seek_section f section in
    Marshal.from_channel f
  with
    | Not_found -> [| |]

let input_symbols f section : Symtable.global_map =
  try
    let _len = Bytesections.seek_section f section in
    Marshal.from_channel f
  with
    | Not_found -> Symtable.empty_global_map
                 
let read_executable name =
  let f = open_in name in
  Bytesections.read_toc f;
  (* sections:
     - "CODE": bytecode
     - "DLPT": DLL paths (optional)
     - "DLLS": DLL names
     - "PRIM": primitives
     - "DATA": global data
     - "SYMB": global identifiers
     - "CRCS": checksums
     - "DBUG": debug section
   *)
  let dll_paths = input_stringlist f "DLPT" in
  let dll_names = input_stringlist f "DLLS" in
  let primitives = input_stringlist f "PRIM" |> Array.of_list in
  let code = Bytesections.read_section_string f "CODE" in
  let data = input_objarray f "DATA" in
  let symbols = input_symbols f "SYMB" in
  close_in f;
  { dll_paths;
    dll_names;
    primitives;
    code;
    data;
    symbols
  }

type format =
  | C    (* int constant *)
  | L    (* label *)
  | LO   (* label origin for LL *)
  | N    (* length field *)
  | NN   (* two 16 bit length fields (LSB: first, MSB: second) *)
  | LL   (* label list *)

let instructions exec =
  [ O.opACC0, [], (fun args -> [I.Kacc 0]);
    O.opACC1, [], (fun args -> [I.Kacc 1]);
    O.opACC2, [], (fun args -> [I.Kacc 2]);
    O.opACC3, [], (fun args -> [I.Kacc 3]);
    O.opACC4, [], (fun args -> [I.Kacc 4]);
    O.opACC5, [], (fun args -> [I.Kacc 5]);
    O.opACC6, [], (fun args -> [I.Kacc 6]);
    O.opACC7, [], (fun args -> [I.Kacc 7]);
    O.opACC, [C], (fun args -> [I.Kacc args.(0)]);
    O.opPUSH, [], (fun args -> [I.Kpush]);
    O.opPUSHACC0, [], (fun args -> [I.Kpush; I.Kacc 0]);
    O.opPUSHACC1, [], (fun args -> [I.Kpush; I.Kacc 1]);
    O.opPUSHACC2, [], (fun args -> [I.Kpush; I.Kacc 2]);
    O.opPUSHACC3, [], (fun args -> [I.Kpush; I.Kacc 3]);
    O.opPUSHACC4, [], (fun args -> [I.Kpush; I.Kacc 4]);
    O.opPUSHACC5, [], (fun args -> [I.Kpush; I.Kacc 5]);
    O.opPUSHACC6, [], (fun args -> [I.Kpush; I.Kacc 6]);
    O.opPUSHACC7, [], (fun args -> [I.Kpush; I.Kacc 7]);
    O.opPUSHACC, [C], (fun args -> [I.Kpush; I.Kacc args.(0)]);
    O.opPOP, [C], (fun args -> [I.Kpop args.(0)]);
    O.opASSIGN, [C], (fun args -> [I.Kassign args.(0)]);
    O.opENVACC1, [], (fun args -> [I.Kenvacc 1]);
    O.opENVACC2, [], (fun args -> [I.Kenvacc 2]);
    O.opENVACC3, [], (fun args -> [I.Kenvacc 3]);
    O.opENVACC4, [], (fun args -> [I.Kenvacc 4]);
    O.opENVACC, [C], (fun args -> [I.Kenvacc args.(0)]);
    O.opPUSHENVACC1, [], (fun args -> [I.Kpush; I.Kenvacc 1]);
    O.opPUSHENVACC2, [], (fun args -> [I.Kpush; I.Kenvacc 2]);
    O.opPUSHENVACC3, [], (fun args -> [I.Kpush; I.Kenvacc 3]);
    O.opPUSHENVACC4, [], (fun args -> [I.Kpush; I.Kenvacc 4]);
    O.opPUSHENVACC, [C], (fun args -> [I.Kpush; I.Kenvacc args.(0)]);
    O.opPUSH_RETADDR, [L], (fun args -> [I.Kpush_retaddr args.(0)]);
    O.opAPPLY, [C], (fun args -> [I.Kapply args.(0)]);
    O.opAPPLY1, [], (fun args -> [I.Kapply 1]);
    O.opAPPLY2, [], (fun args -> [I.Kapply 2]);
    O.opAPPLY3, [], (fun args -> [I.Kapply 3]);
    O.opAPPTERM, [C;C], (fun args -> [I.Kappterm(args.(0), args.(1))]);
    O.opAPPTERM1, [C], (fun args -> [I.Kappterm(1, args.(0))]);
    O.opAPPTERM2, [C], (fun args -> [I.Kappterm(2, args.(0))]);
    O.opAPPTERM3, [C], (fun args -> [I.Kappterm(3, args.(0))]);
    O.opRETURN, [C], (fun args -> [I.Kreturn args.(0)]);
    O.opRESTART, [], (fun args -> [I.Krestart]);
    O.opGRAB, [C], (fun args -> [I.Kgrab args.(0)]);
    O.opCLOSURE, [C;L], (fun args -> [I.Kclosure(args.(1), args.(0))]);
    O.opCLOSUREREC, [N;C;LO;LL],
      (fun args ->
        let n = args.(0) in
        [I.Kclosurerec(Array.sub args 2 n |> Array.to_list, args.(1))]
      );
    O.opOFFSETCLOSUREM3, [], (fun args -> [I.Koffsetclosure 3]);
    O.opOFFSETCLOSURE0, [], (fun args -> [I.Koffsetclosure 0]);
    O.opOFFSETCLOSURE3, [], (fun args -> [I.Koffsetclosure (-3)]);
    O.opOFFSETCLOSURE, [C], (fun args -> [I.Koffsetclosure args.(0)]);
    O.opPUSHOFFSETCLOSUREM3, [], (fun args -> [I.Kpush; I.Koffsetclosure 3]);
    O.opPUSHOFFSETCLOSURE0, [], (fun args -> [I.Kpush; I.Koffsetclosure 0]);
    O.opPUSHOFFSETCLOSURE3, [], (fun args -> [I.Kpush; I.Koffsetclosure (-3)]);
    O.opPUSHOFFSETCLOSURE, [C], (fun args -> [I.Kpush; I.Koffsetclosure args.(0)]);
    O.opGETGLOBAL, [C], (fun args ->
      let ident = Ident.create_persistent (sprintf "global_%d" args.(0)) in
      [I.Kgetglobal ident]
    );
    O.opPUSHGETGLOBAL, [C], (fun args ->
      let ident = Ident.create_persistent (sprintf "global_%d" args.(0)) in
      [I.Kpush; I.Kgetglobal ident]
    );
    O.opGETGLOBALFIELD, [C;C], (fun args ->
      let ident = Ident.create_persistent (sprintf "global_%d" args.(0)) in
      [I.Kgetglobal ident; I.Kgetfield args.(1)]
    );
    O.opPUSHGETGLOBALFIELD, [C;C], (fun args ->
      let ident = Ident.create_persistent (sprintf "global_%d" args.(0)) in
      [I.Kpush; I.Kgetglobal ident; I.Kgetfield args.(1)]
    );
    O.opSETGLOBAL, [C], (fun args ->
      let ident = Ident.create_persistent (sprintf "global_%d" args.(0)) in
      [I.Ksetglobal ident]
    );
    O.opATOM0, [], (fun args -> [I.Kmakeblock(0, 0)]);
    O.opATOM, [C], (fun args -> [I.Kmakeblock(0, args.(0))]);
    O.opPUSHATOM0, [], (fun args ->
      [I.Kpush; I.Kmakeblock(0, 0)]);
    O.opPUSHATOM, [C], (fun args ->
      [I.Kpush; I.Kmakeblock(0, args.(0))]);
    O.opMAKEBLOCK, [C;C], (fun args -> [I.Kmakeblock(args.(0), args.(1))]);
    O.opMAKEBLOCK1, [C], (fun args -> [I.Kmakeblock(0, args.(0))]);
    O.opMAKEBLOCK2, [C], (fun args -> [I.Kmakeblock(1, args.(0))]);
    O.opMAKEBLOCK3, [C], (fun args -> [I.Kmakeblock(2, args.(0))]);
    O.opMAKEFLOATBLOCK, [C], (fun args -> [I.Kmakefloatblock args.(0)]);
    O.opGETFIELD0, [], (fun args -> [I.Kgetfield 0]);
    O.opGETFIELD1, [], (fun args -> [I.Kgetfield 1]);
    O.opGETFIELD2, [], (fun args -> [I.Kgetfield 2]);
    O.opGETFIELD3, [], (fun args -> [I.Kgetfield 3]);
    O.opGETFIELD, [C], (fun args -> [I.Kgetfield args.(0)]);
    O.opGETFLOATFIELD, [C], (fun args -> [I.Kgetfloatfield args.(0)]);
    O.opSETFIELD0, [], (fun args -> [I.Ksetfield 0]);
    O.opSETFIELD1, [], (fun args -> [I.Ksetfield 1]);
    O.opSETFIELD2, [], (fun args -> [I.Ksetfield 2]);
    O.opSETFIELD3, [], (fun args -> [I.Ksetfield 3]);
    O.opSETFIELD, [C], (fun args -> [I.Ksetfield args.(0)]);
    O.opSETFLOATFIELD, [C], (fun args -> [I.Ksetfloatfield args.(0)]);
    O.opVECTLENGTH, [], (fun args -> [I.Kvectlength]);
    O.opGETVECTITEM, [], (fun args -> [I.Kgetvectitem]);
    O.opSETVECTITEM, [], (fun args -> [I.Ksetvectitem]);
    O.opGETBYTESCHAR, [], (fun args -> [I.Kgetbyteschar]);
    O.opSETBYTESCHAR, [], (fun args -> [I.Ksetbyteschar]);
    O.opBRANCH, [L], (fun args -> [I.Kbranch args.(0)]);
    O.opBRANCHIF, [L], (fun args -> [I.Kbranchif args.(0)]);
    O.opBRANCHIFNOT, [L], (fun args -> [I.Kbranchifnot args.(0)]);
    O.opSWITCH, [NN; LO; LL; LL], (fun args ->
      let n_const = args.(0) land 0xffff in
      let n_block = args.(0) lsr 16 in
      let tbl_const = Array.sub args 1 n_const in
      let tbl_block = Array.sub args (1+n_const) n_block in
      [I.Kswitch(tbl_const, tbl_block)]
    );
    O.opBOOLNOT, [], (fun args -> [I.Kboolnot]);
    O.opPUSHTRAP, [L], (fun args -> [I.Kpushtrap args.(0)]);
    O.opPOPTRAP, [], (fun args -> [I.Kpoptrap]);
    O.opRAISE, [], (fun args -> [I.Kraise Lambda.Raise_regular]);
    O.opCHECK_SIGNALS, [], (fun args -> [I.Kcheck_signals]);
    O.opC_CALL1, [C], (fun args -> [I.Kccall (exec.primitives.(args.(0)), 1)]);
    O.opC_CALL2, [C], (fun args -> [I.Kccall (exec.primitives.(args.(0)), 2)]);
    O.opC_CALL3, [C], (fun args -> [I.Kccall (exec.primitives.(args.(0)), 3)]);
    O.opC_CALL4, [C], (fun args -> [I.Kccall (exec.primitives.(args.(0)), 4)]);
    O.opC_CALL5, [C], (fun args -> [I.Kccall (exec.primitives.(args.(0)), 5)]);
    O.opC_CALLN, [C;C], (fun args ->
      [I.Kccall (exec.primitives.(args.(1)), args.(0))]);
    O.opCONST0, [], (fun args ->
      [ I.Kconst (Lambda.Const_base (Asttypes.Const_int 0)) ]);
    O.opCONST1, [], (fun args ->
      [ I.Kconst (Lambda.Const_base (Asttypes.Const_int 1)) ]);
    O.opCONST2, [], (fun args ->
      [ I.Kconst (Lambda.Const_base (Asttypes.Const_int 2)) ]);
    O.opCONST3, [], (fun args ->
      [ I.Kconst (Lambda.Const_base (Asttypes.Const_int 3)) ]);
    O.opCONSTINT, [C], (fun args ->
      (* NB. cannot recognize Const_char *)
      [ I.Kconst (Lambda.Const_base (Asttypes.Const_int args.(0))) ]);
    O.opPUSHCONST0, [], (fun args ->
      [ I.Kpush; I.Kconst (Lambda.Const_base (Asttypes.Const_int 0)) ]);
    O.opPUSHCONST1, [], (fun args ->
      [ I.Kpush; I.Kconst (Lambda.Const_base (Asttypes.Const_int 1)) ]);
    O.opPUSHCONST2, [], (fun args ->
      [ I.Kpush; I.Kconst (Lambda.Const_base (Asttypes.Const_int 2)) ]);
    O.opPUSHCONST3, [], (fun args ->
      [ I.Kpush; I.Kconst (Lambda.Const_base (Asttypes.Const_int 3)) ]);
    O.opPUSHCONSTINT, [C], (fun args ->
      (* NB. cannot recognize Const_char *)
      [ I.Kpush; I.Kconst (Lambda.Const_base (Asttypes.Const_int args.(0))) ]);
    O.opNEGINT, [], (fun args -> [I.Knegint]);
    O.opADDINT, [], (fun args -> [I.Kaddint]);
    O.opSUBINT, [], (fun args -> [I.Ksubint]);
    O.opMULINT, [], (fun args -> [I.Kmulint]);
    O.opDIVINT, [], (fun args -> [I.Kdivint]);
    O.opMODINT, [], (fun args -> [I.Kmodint]);
    O.opANDINT, [], (fun args -> [I.Kandint]);
    O.opORINT, [], (fun args -> [I.Korint]);
    O.opXORINT, [], (fun args -> [I.Kxorint]);
    O.opLSLINT, [], (fun args -> [I.Klslint]);
    O.opLSRINT, [], (fun args -> [I.Klsrint]);
    O.opASRINT, [], (fun args -> [I.Kasrint]);
    O.opEQ, [], (fun args -> [I.Kintcomp Ceq]);
    O.opNEQ, [], (fun args -> [I.Kintcomp Cne]);
    O.opLTINT, [], (fun args -> [I.Kintcomp Clt]);
    O.opLEINT, [], (fun args -> [I.Kintcomp Cle]);
    O.opGTINT, [], (fun args -> [I.Kintcomp Cgt]);
    O.opGEINT, [], (fun args -> [I.Kintcomp Cge]);
    O.opOFFSETINT, [C], (fun args -> [I.Koffsetint args.(0)]);
    O.opOFFSETREF, [C], (fun args -> [I.Koffsetref args.(0)]);
    O.opISINT, [], (fun args -> [I.Kisint]);
    O.opGETMETHOD, [], (fun args -> [I.Kgetmethod]);
    O.opBEQ, [C;L], (fun args ->
      let const = Lambda.Const_base(Const_int args.(0)) in
      [ Kpush; Kconst const; Kintcomp Ceq; Kbranchif args.(1) ]);
    O.opBNEQ, [C;L], (fun args ->
      let const = Lambda.Const_base(Const_int args.(0)) in
      [ Kpush; Kconst const; Kintcomp Cne; Kbranchif args.(1) ]);
    O.opBLTINT, [C;L], (fun args ->
      let const = Lambda.Const_base(Const_int args.(0)) in
      [ Kpush; Kconst const; Kintcomp Clt; Kbranchif args.(1) ]);
    O.opBLEINT, [C;L], (fun args ->
      let const = Lambda.Const_base(Const_int args.(0)) in
      [ Kpush; Kconst const; Kintcomp Cle; Kbranchif args.(1) ]);
    O.opBGTINT, [C;L], (fun args ->
      let const = Lambda.Const_base(Const_int args.(0)) in
      [ Kpush; Kconst const; Kintcomp Cgt; Kbranchif args.(1) ]);
    O.opBGEINT, [C;L], (fun args ->
      let const = Lambda.Const_base(Const_int args.(0)) in
      [ Kpush; Kconst const; Kintcomp Cge; Kbranchif args.(1) ]);
    O.opULTINT, [], (fun args -> [I.Kisout]);
    O.opUGEINT, [], (fun args -> assert false);
    O.opBULTINT, [C;L], (fun args ->
      let const = Lambda.Const_base(Const_int args.(0)) in
      [ Kpush; Kconst const; Kisout; Kbranchif args.(1) ]);
    O.opBUGEINT, [C;L], (fun args ->
      let const = Lambda.Const_base(Const_int args.(0)) in
      [ Kpush; Kconst const; Kisout; Kbranchifnot args.(1) ]);
    O.opGETPUBMET, [C;C], (fun args -> [I.Kgetpubmet args.(0)]);
    O.opGETDYNMET, [], (fun args -> [I.Kgetdynmet]);
    O.opSTOP, [], (fun args -> [I.Kstop]);
    O.opEVENT, [], (fun args -> []);  (* FIXME *)
    O.opBREAK, [], (fun args -> failwith "bad instruction: BREAK");
    O.opRERAISE, [], (fun args -> [I.Kraise Lambda.Raise_reraise]);
    O.opRAISE_NOTRACE, [], (fun args -> [I.Kraise Lambda.Raise_notrace]);
    O.opGETSTRINGCHAR, [], (fun args -> [I.Kgetstringchar]);
  ]

let map_label_in_instr f instr =
  match instr with
    | I.Klabel _ -> assert false
    | I.Kpush_retaddr lab -> I.Kpush_retaddr (f lab)
    | I.Kclosure (lab, k) -> I.Kclosure (f lab, k)
    | I.Kclosurerec (labl, k) -> I.Kclosurerec (List.map f labl, k)
    | I.Kbranch lab -> I.Kbranch (f lab)
    | I.Kbranchif lab -> I.Kbranchif (f lab)
    | I.Kbranchifnot lab -> I.Kbranchifnot (f lab)
    | I.Kstrictbranchif _ -> assert false
    | I.Kstrictbranchifnot _ -> assert false
    | I.Kswitch (laba1, laba2) ->
        I.Kswitch (Array.map f laba1, Array.map f laba2)
    | I.Kpushtrap lab -> I.Kpushtrap (f lab)
    | _ -> instr

let get_labels_in_instr instr =
  match instr with
    | I.Klabel _ -> assert false
    | I.Kpush_retaddr lab ->[lab]
    | I.Kclosure (lab, k) -> [lab]
    | I.Kclosurerec (labl, k) -> labl
    | I.Kbranch lab -> [lab]
    | I.Kbranchif lab -> [lab]
    | I.Kbranchifnot lab -> [lab]
    | I.Kstrictbranchif _ -> assert false
    | I.Kstrictbranchifnot _ -> assert false
    | I.Kswitch (laba1, laba2) -> Array.append laba1 laba2 |> Array.to_list
    | I.Kpushtrap lab -> [lab]
    | _ -> []

let decode exec =
  let instrs_l = instructions exec in
  let num = List.fold_left (fun acc (n,_,_) -> max acc n) (-1) instrs_l + 1 in
  let instrs = Array.make num ([], fun args -> failwith "bad instruction") in
  List.iter
    (fun (opcode, signature, decoder) ->
      instrs.(opcode) <- (signature, decoder)
    )
    instrs_l;
  let code = exec.code in
  let decoded = ref [] in
  let p = ref 0 in
  let read_int() =
    let k =
      (Char.code code.[!p])
      lor (Char.code code.[!p+1] lsl 8)
      lor (Char.code code.[!p+2] lsl 16)
      lor (Char.code code.[!p+3] lsl 24) in
    p := !p + 4;
    let bit30 = (k land 0x4000_0000) <> 0 in
    let neg_fixup = if bit30 then (-1) lxor 0x3fff_ffff else 0 in
    k lor neg_fixup in
  while !p < String.length code do
    let p0 = !p in
    let label_origin = ref (-1) in
    let op = read_int() in
    if op < 0 || op >= num then failwith "bad instruction";
    let (signature, decoder) = instrs.(op) in
    let len0 = ref 0 in
    let len1 = ref 0 in
    let q = Queue.create() in
    List.iter
      (function
       | C ->
           Queue.add (read_int()) q
       | L ->
           let b = !p in
           let rel_label = read_int() in
           let abs_label = b / 4 + rel_label in
           Queue.add abs_label q
       | N ->
           len0 := read_int();
           len1 := 0;
           Queue.add !len0 q
       | NN ->
           let k = read_int() in
           len0 := k land 0xffff;
           len1 := k lsr 16;
           Queue.add k q
       | LO ->
           label_origin := !p
       | LL ->
           let n = !len0 in
           len0 := !len1;
           len1 := 0;
           if !label_origin < 0 then failwith "no label origin";
           for k = 1 to n do
             let rel_label = read_int() in
             let abs_label = !label_origin / 4 + rel_label in
             Queue.add abs_label q
           done
      )
      signature;
    let args = Array.make (Queue.length q) 0 in
    let k = ref 0 in
    Queue.iter
      (fun arg ->
        args.(!k) <- arg;
        incr k
      )
      q;
    let instr_l = decoder args |> List.map (fun instr -> (p0, instr)) in
    decoded := List.rev_append instr_l !decoded;
  done;
  let decoded_a =
    List.rev !decoded
    |> Array.of_list in
  let labels = Array.make (!p / 4) (-1) in
  Array.iteri
    (fun i (bytepos,_) ->
      if labels.(bytepos / 4) = (-1) then
        labels.(bytepos / 4) <- i
    )
    decoded_a;
  let all_labels = ref ISet.empty in
  let map_label lab =
    if lab < 0 || lab >= Array.length labels then failwith "bad label";
    let lab' = labels.(lab) in
    if lab' < 0 then failwith "bad label";
    all_labels := ISet.add lab' !all_labels;
    lab' in
  let all_instrs =
    decoded_a
    |> Array.mapi
         (fun i (_, instr) ->
           match instr with
             | I.Klabel _ -> assert false
             | I.Kpush_retaddr lab -> I.Kpush_retaddr (map_label lab)
             | I.Kclosure (lab, k) -> I.Kclosure (map_label lab, k)
             | I.Kclosurerec (labl, k) -> I.Kclosurerec (List.map map_label labl, k)
             | I.Kbranch lab -> I.Kbranch (map_label lab)
             | I.Kbranchif lab -> I.Kbranchif (map_label lab)
             | I.Kbranchifnot lab -> I.Kbranchifnot (map_label lab)
             | I.Kstrictbranchif _ -> assert false
             | I.Kstrictbranchifnot _ -> assert false
             | I.Kswitch (laba1, laba2) ->
                 I.Kswitch (Array.map map_label laba1, Array.map map_label laba2)
             | I.Kpushtrap lab -> I.Kpushtrap (map_label lab)
             | _ -> instr
         ) in
  (all_instrs, !all_labels)



