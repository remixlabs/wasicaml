#use "topfind";;
#require "compiler-libs.common, compiler-libs.bytecomp";;
#load "wc.cma";;
open Wc_types;;
open Wc_reader;;
open Wc_control;;
open Wc_sexp;;
open Wc_emit;;
open Wc_sexp;;
open Printf;;
#print_length 20000;;
let exec = read_executable "t";;
let code, labels = decode exec;;
eprintf "Number instructions: %d\n" (Array.length code);;
eprintf "Number labels: %d\n%!" (ISet.cardinal labels);;
let cfg = create_cfg code labels;;
eprintf "Number nodes: %d\n%!" (IMap.cardinal cfg.nodes);;
let s = recover_structure cfg;;
eprintf "Number functions: %d\n%!" (IMap.cardinal s.functions);;
validate s;;
let sexpl = generate s exec;;
let print() =
  let full = K "module" :: sexpl in
  let f = open_out "t.wat" in
  print_indented f 0 80 (L full);
  close_out f;
  let f = open_out "t.s" in
  Wc_sexp2s.write_file f "t.s" sexpl;
  close_out f;
