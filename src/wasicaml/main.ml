let _ =
  Printexc.record_backtrace true;;

let _ =
  try
    let exec = Wc_reader.read_executable "t" in
    let code, labels = Wc_reader.decode exec in
    let ctx = Wc_control.create_context code labels in
    Wc_control.detect_loops ctx;
    let _ = Wc_control.recover_structure ctx in
    print_endline "Done"
  with
    | error ->
        let bt = Printexc.get_backtrace() in
        prerr_endline (Printexc.to_string error);
        prerr_endline bt;
        exit 2
