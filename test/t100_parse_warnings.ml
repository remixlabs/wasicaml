open Printf

type modifier =
  | Set (** +a *)
  | Clear (** -a *)
  | Set_all (** @a *)

type token =
  | Letter of char * modifier option
  | Num of int * int * modifier

let parse_warnings s =
  let error () = raise (Arg.Bad "Ill-formed list of warnings") in
  let rec get_num n i =
    if i >= String.length s then i, n
    else match s.[i] with
    | '0'..'9' -> get_num (10 * n + Char.code s.[i] - Char.code '0') (i + 1)
    | _ -> i, n
  in
  let get_range i =
    let i, n1 = get_num 0 i in
    if i + 2 < String.length s && s.[i] = '.' && s.[i + 1] = '.' then
      let i, n2 = get_num 0 (i + 2) in
      if n2 < n1 then error ();
      i, n1, n2
    else
      i, n1, n1
  in
  let rec loop tokens i =
    if i >= String.length s then List.rev tokens else
    match s.[i] with
    | 'A' .. 'Z' | 'a' .. 'z' ->
        loop (Letter(s.[i],None)::tokens) (i+1)
    | '+' -> loop_letter_num tokens Set (i+1)
    | '-' -> loop_letter_num tokens Clear (i+1)
    | '@' -> loop_letter_num tokens Set_all (i+1)
    | _ -> error ()
  and loop_letter_num tokens modifier i =
    if i >= String.length s then error () else
    match s.[i] with
    | '0' .. '9' ->
        let i, n1, n2 = get_range i in
        loop (Num(n1,n2,modifier)::tokens) i
    | 'A' .. 'Z' | 'a' .. 'z' ->
       loop (Letter(s.[i],Some modifier)::tokens) (i+1)
    | _ -> error ()
  in
  loop [] 0

let string_of_mod =
  function
  | Set -> "+"
  | Clear -> "-"
  | Set_all -> "@"

let print_warnings toks =
  List.iter
    (function
     | Letter(c, None) ->
         Testprint.string "letter" (String.make 1 c)
     | Letter(c, Some m) ->
         Testprint.string "letter" (string_of_mod m ^ String.make 1 c)
     | Num(n1,n2,m) ->
         Testprint.string "num" (sprintf "%s%d..%d" (string_of_mod m) n1 n2)
    )
    toks


let f() =
  parse_warnings "+a-4-32..42+66..69-x"
  |> print_warnings

let () = f()
