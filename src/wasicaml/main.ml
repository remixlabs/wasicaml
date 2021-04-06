open Wc_reader

let _ =
  Printexc.record_backtrace true;;

let _ =
  try
    let exec = read_executable "t" in
    let c = decode exec in
    print_endline "Done"
  with
    | error ->
        let bt = Printexc.get_backtrace() in
        prerr_endline (Printexc.to_string error);
        prerr_endline bt;
        exit 2
