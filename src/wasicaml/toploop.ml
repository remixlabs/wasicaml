#use "topfind";;
#require "compiler-libs.common, compiler-libs.bytecomp";;
#load "wc.cma";;
open Wc_types;;
open Wc_reader;;
open Wc_control;;
open Wc_sexp;;
open Printf;;
(*
#print_length 20000;;
let exec = read_executable "/opt/opam/4.12.0/bin/ocamllex.byte";;
let code, labels = decode exec;;
eprintf "Number instructions: %d\n" (Array.length code);;
eprintf "Number labels: %d\n%!" (ISet.cardinal labels);;
let cfg = create_cfg code labels;;
eprintf "Number nodes: %d\n%!" (IMap.cardinal ctx.nodes);;
let s = recover_structure cfg;;
eprintf "Number functions: %d\n%!" (IMap.cardinal s.functions);;
validate s;;
 *)
