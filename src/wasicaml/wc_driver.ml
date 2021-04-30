open Wc_types
open Wc_reader
open Wc_control
open Wc_sexp
open Wc_emit
open Wc_sexp
open Printf

let prefix =
  Sys.getenv "HOME" ^ "/.wasicaml"

let main() =
  let out = ref "a.out" in   (* let's be traditional *)
  let inp = ref None in
  Arg.parse
    [ "-o", Arg.Set_string out,
      "<file>   Set the output file";
    ]
    (fun arg ->
      if !inp <> None then
        raise(Arg.Bad "only one input file is permitted");
      inp := Some arg
    )
    "usage: wasicaml -o output[.wasm] bytecode";
  let inp =
    match !inp with
      | None -> failwith "no input file"
      | Some inp -> inp in

  eprintf "* parsing...\n%!";
  let exec =
    try read_executable inp
    with Bytesections.Bad_magic_number ->
      failwith ("not a bytecode executable: " ^ inp) in
  let code, labels = decode exec in
  eprintf "  number instructions: %d\n" (Array.length code);
  eprintf "  number labels: %d\n%!" (ISet.cardinal labels);

  eprintf "* creating CFG...\n%!";
  let cfg = create_cfg code labels in
  eprintf "  number nodes: %d\n%!" (IMap.cardinal cfg.nodes);

  eprintf "* decompile...\n%!";
  let s = recover_structure cfg in
  eprintf "  number functions: %d\n%!" (IMap.cardinal s.functions);
  eprintf "* validating...\n%!";
  validate s;

  eprintf "* translating to WASM...\n%!";
  let sexpl = generate s exec in

  eprintf "* print as .wat...\n%!";
  let full = K "module" :: sexpl in
  let f = open_out (inp ^ ".wat") in
  print_indented f 0 80 (L full);
  close_out f;

  eprintf "* print as .s (llvm integrated assembler syntax)...\n%!";
  let f = open_out (inp ^ ".s") in
  Wc_sexp2s.write_file f (inp ^ ".s") sexpl;
  close_out f;

  eprintf "* assemble...\n%!";
  let cmd =
    sprintf "%s/bin/wasi_cc -c %s" prefix (Filename.quote (inp ^ ".s")) in
  eprintf "+ %s\n%!" cmd;
  let code = Sys.command cmd in
  if code <> 0 then
    failwith "command failed";

  eprintf "* link...\n%!";
  let cmd =
    sprintf "%s/bin/wasi_cc -o %s %s/lib/initruntime.o %s -L %s/lib/ocaml -lcamlrun"
            prefix !out prefix (inp ^ ".o") prefix in
  eprintf "+ %s\n%!" cmd;
  let code = Sys.command cmd in
  if code <> 0 then
    failwith "command failed";
  ()


let () =
  try
    main()
  with
    | Failure msg ->
        eprintf "%s\n%!" msg;
        exit 2
    | Arg.Bad msg ->
        eprintf "%s\n%!" msg;
        exit 2

