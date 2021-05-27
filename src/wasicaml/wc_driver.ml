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
  let cclib = ref [] in
  let cstack = ref (1024 * 1024) in
  let quiet = ref false in
  Arg.parse
    [ "-o", Arg.Set_string out,
      "<file>   Set the output file";

      "-cclib", Arg.String (fun s -> cclib := !cclib @ [s]),
      "<option>   pass this option to the linker";

      "-cstack", Arg.Set_int cstack,
      (sprintf "<numbytes>   set the size of the C shadow stack (default %d)" !cstack);

      "-q", Arg.Set quiet,
      "   quiet: reduce verbosity";

      "-enable-multivalue", Arg.Set Wc_emit.enable_multireturn,
      "   enable Wasm feature: multi-value returns (EXPERIMENTAL)";

      "-enable-deadbeef-check", Arg.Set Wc_emit.enable_deadbeef_check,
      "   enable stack initialization check (debug)";
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

  if not !quiet then
    eprintf "* parsing...\n%!";
  let exec =
    try read_executable inp
    with Bytesections.Bad_magic_number ->
      failwith ("not a bytecode executable: " ^ inp) in
  let code, labels, maplab = decode exec in
  if not !quiet then (
    eprintf "  number instructions: %d\n" (Array.length code);
    eprintf "  number labels: %d\n%!" (ISet.cardinal labels);
  );

  let get_defname = defname_of_label exec maplab in

  if not !quiet then
    eprintf "* creating CFG...\n%!";
  let cfg = create_cfg code labels in
  if not !quiet then
    eprintf "  number nodes: %d\n%!" (IMap.cardinal cfg.nodes);

  if not !quiet then
    eprintf "* linearize...\n%!";
  let s = recover_structure cfg in
  if not !quiet then (
    eprintf "  number functions: %d\n%!" (IMap.cardinal s.functions);
    eprintf "* validating...\n%!";
  );
  validate s;

  if not !quiet then
    eprintf "* translating to WASM...\n%!";
  let sexpl = generate s exec get_defname in

  if not !quiet then
    eprintf "* print as .wat...\n%!";
  let full = K "module" :: sexpl in
  let f = open_out (inp ^ ".wat") in
  print_indented f 0 80 (L full);
  close_out f;

  if not !quiet then
    eprintf "* print as .s (llvm integrated assembler syntax)...\n%!";
  let f = open_out (inp ^ ".s") in
  Wc_sexp2s.write_file f (inp ^ ".s") sexpl;
  close_out f;

  if not !quiet then
    eprintf "* assemble...\n%!";
  let cmd =
    sprintf "%s/bin/wasi_cc -c %s" prefix (Filename.quote (inp ^ ".s")) in
  if not !quiet then
    eprintf "+ %s\n%!" cmd;
  let code = Sys.command cmd in
  if code <> 0 then
    failwith "command failed";

  if not !quiet then
    eprintf "* link...\n%!";
  let cmd =
    sprintf "%s/bin/wasi_cc -Wl,-z,stack-size=%d -o %s %s/lib/initruntime.o %s -L %s/lib/ocaml %s -lcamlrun"
            prefix !cstack !out prefix (inp ^ ".o") prefix
            (List.map Filename.quote !cclib |> String.concat " ") in
  if not !quiet then
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

