(* taken from ocaml/testsuite/misc *)

(* Eratosthene's sieve *)

(* interval min max = [min; min+1; ...; max-1; max] *)

let rec interval min max =
  if min > max then [] else min :: interval (min + 1) max


(* filter p L returns the list of the elements in list L
   that satisfy predicate p *)

let rec filter p = function
     []  -> []
  | a::r -> if p a then a :: filter p r else filter p r


(* Application: removing all numbers multiple of n from a list of integers *)

let remove_multiples_of n =
  filter (fun m -> m mod n <> 0)


(* The sieve itself *)

let sieve max =
  let rec filter_again = function
     [] -> []
  | n::r as l ->
      if n*n > max then l else n :: filter_again (remove_multiples_of n r)
  in
    filter_again (interval 2 max)


let rec do_list f = function
    [] -> ()
  | a::l -> f a; do_list f l


let _ =
  let t0 = Testprint.clock64() in
  for k = 1 to 10 do
    do_list (fun n -> print_string " "; print_int n) (sieve 100000);
    print_newline();
  done;
  let t1 = Testprint.clock64() in
  Printf.eprintf "time=%Ld ms\n%!" (Int64.div (Int64.sub t1 t0) 1_000_000L);
  exit 0
