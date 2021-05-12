open Printf
open Wc_types
open Wc_number
open Wc_sexp
open Wc_instruct

(* TODO:
   - create global wasicaml_global_data and initialize it with &caml_global_data
     (needs a helper function in C to get the address, and generated init
     code to set the global)
   - wasicaml_domain_state = caml_domain_state
   - wasicaml_atom_table = caml_atom_table
   - wasicaml_functions

   - machine initialization
     DATA -> globals
     Marshal.to_string exe.data
   - init code
   - debug print per block
   - generate wrapper for caml_alloc_small_dispatch:
     pass args: accu, env, fp, framesize, blocksize
     return: new ptr
     pass back via special global: accu, env
   - do a stack check at the beginning of a function (but no realloc)
   - use helper functions for Kapply?
   - check that subfunc fits into codeptr (10 bits)
   - allow swapped stack positions (could avoid a lot of code)
   - implement 31 bit arithmetic correctly
   - do not emit "$letrec0" in addition to "$mainfunc"
   - helper functions for "makeblock"
   - rename "decompiling" -> "linearizing"
 *)

(* OCaml functions are translated to Wasm functions with parameters:
   param 1: env
   param 2: extra_args
   param 3: code pointer (overriding the one in the closure)
   param 4: fp
 *)


type wasm_value_type =
  | TI32 | TI64 | TF64

let string_of_vtype =
  function
  | TI32 -> "i32"
  | TI64 -> "i64"
  | TF64 -> "f64"

let zero_expr_of_vtype =
  function
  | TI32 -> [ L [ K "i32.const"; N (I32 0l) ] ]
  | TI64 -> [ L [ K "i64.const"; N (I64 0L) ] ]
  | TF64 -> [ L [ K "f64.const"; N (F64 0.0) ] ]

type gpad =  (* global pad *)
  { letrec_name : (int, string) Hashtbl.t;
    (* maps letrec label to name *)
    primitives : (string, sexp list) Hashtbl.t;
    (* maps name to type *)
    funcmapping : (int, int * int) Hashtbl.t;
    (* maps function label to (letrec_label, subfunction_id) *)
    subfunctions : (int, int list) Hashtbl.t;
    (* maps letrec_label to list of subfunction labels *)
    wasmindex : (int, int) Hashtbl.t;
    (* maps letrec label to the (relative) index in the table of functions *)
  }

type fpad =  (* function pad *)
  { lpad : Wc_unstack.lpad;
    mutable maxdepth : int;
    mutable need_appterm_common : bool;
    mutable need_return : bool;
    mutable need_panic : bool;
  }

(* Stack layout of a function with N arguments:

   fp is the stack pointer at the time the function starts executing.

     POSITION                USED FOR
   --------------------------------------------------------
   - fp+N-1:                 argN
   - fp+1 to fp+N-2:         ...
   - fp:                     arg1
   - fp-1:                   bottom of local stack
   - fp-camldepth+1 to fp-2: ...
   - fp-camldepth:           top of local stack
   - fp-camldepth-1 etc:     free
 *)


let enable_multireturn = ref false
(* whether Wasm code can use multivalue returns *)

let code_pointer_shift = 12
  (* OCaml code pointers:
      - Bit 0: whether to run RESTART
      - Bit 1 - code_pointer_shift-1: subfunction of the letrec
      - Bit code_pointer_shift-31: the Wasm function index
   *)
let code_pointer_subfunc_mask = 0xffel
let code_pointer_letrec_mask = 0xffff_f000l

(* TODO: grab the following values from C: *)
(* Note that the domain fields are 8-aligned, even on 32 bit systems *)
let max_young_wosize = 256
let domain_field_young_ptr = 0
let domain_field_young_limit = 1
let domain_field_stack_low = 17
let domain_field_stack_high = 18
let domain_field_stack_threshold = 19
let domain_field_extern_sp = 20
let domain_field_trapsp = 21
let domain_field_trap_barrier = 22
let domain_field_external_raise = 23
let domain_field_exn_bucket = 24

let double_size = 2
let double_tag = 253
let double_array_tag = 254

let closure_tag = 247
let infix_tag = 249

let caml_from_c = 0

let make_header size tag =
  (* color=white *)
  (size lsl 10) lor tag

let vtype repr =
  match repr with
    | RValue | RInt | RIntVal | RNatInt | RInt32 ->
        TI32
    | RInt64 ->
        TI64
    | RFloat ->
        TF64

let empty_fpad() =
  { lpad = Wc_unstack.empty_lpad();
    maxdepth = 0;
    need_appterm_common = false;
    need_return = false;
    need_panic = false;
  }

let new_local fpad repr =
  Wc_unstack.new_local fpad.lpad repr

let push_const n =
  [ L [ K "i32.const";
        N (I32 n)
      ]
  ]

let push_local var =
  [ L [ K "local.get";
        ID var
      ]
  ]

let pop_to_local var =
  [ L [ K "local.set";
        ID var
      ]
  ]

let load_offset offset =
  if offset >= 0 then
    [ L [ K "i32.load";
          K (sprintf "offset=0x%lx" (Int32.of_int offset));
          K "align=2";
        ]
    ]
  else
    [ L [ K "i32.const"; N (I32 (Int32.of_int (-offset))) ];
      L [ K "i32.sub" ];
      L [ K "i32.load"; K "align=2" ]
    ]


let push_field var_base field =
  [ L [ K "local.get";
        ID var_base;
      ]
  ] @ load_offset (4 * field)

let push_global_field var_base field =
  [ L [ K "global.get";
        ID var_base;
      ]
  ] @ load_offset (4 * field)

let push_field_addr var_base field =
  [ L [ K "local.get"; ID var_base ] ]
  @ if field <> 0 then
      [ L [ K "i32.const";
            N (I32 (Int32.of_int (4 * field)));
          ];
        L [ K "i32.add" ]
      ]
    else
      []

let push_global_field_addr var_base field =
  [ L [ K "global.get";
        ID var_base;
      ];
    L [ K "i32.const";
        N (I32 (Int32.of_int (4 * field)));
      ];
    L [ K "i32.add" ]
  ]

let push_stack fpad pos =
  if pos >= 0 then
    push_field "fp" pos
  else
    push_field "bp" (pos + fpad.maxdepth)

let push_domain_field field =
  [ L [ K "global.get"; ID "wasicaml_domain_state" ];
    L [ K "i32.load";
        K (sprintf "offset=0x%lx" (Int32.of_int (8 * field)));
        K "align=2";
      ];
  ]

let store_offset addr value offset =
  if offset >= 0 then
    [ L [ K "local.get"; ID addr ];
      L [ K "local.get"; ID value ];
      L [ K "i32.store";
          K (sprintf "offset=0x%lx" (Int32.of_int offset));
          K "align=2";
        ]
    ]
  else
    [ L [ K "local.get"; ID addr ];
      L [ K "i32.const"; N (I32 (Int32.of_int (-offset))) ];
      L [ K "i32.sub" ];
      L [ K "local.get"; ID value ];
      L [ K "i32.store"; K "align=2" ]
    ]

let pop_to_field var_base field =
  [ L [ K "local.set"; ID "h.i32" ];
  ] @ store_offset var_base "h.i32" (4*field)

let pop_to_double_field var_base field =
  [ L [ K "local.set"; ID "h.f64" ];
    L [ K "local.get"; ID var_base ];
    L [ K "i32.const";
        N (I32 (Int32.of_int (4 * double_size * field)));
      ];
    L [ K "i32.add" ];
    L [ K "local.get"; ID "h.f64" ];
    L [ K "f64.store"; K "align=2" ];
  ]

let pop_to_domain_field field =
  [ L [ K "local.set"; ID "h.i32" ];
    L [ K "global.get"; ID "wasicaml_domain_state" ];
    L [ K "local.get"; ID "h.i32" ];
    L [ K "i32.store";
        K (sprintf "offset=0x%lx" (Int32.of_int (8 * field)));
        K "align=2";
      ];
  ]

let pop_to_stack fpad pos =
  if pos >= 0 then
    pop_to_field "fp" pos
  else
    pop_to_field "bp" (pos + fpad.maxdepth)

let load_double =
  [ L [ K "f64.load";
        K "align=2"
      ]
  ]

let debug2 x0 x1 =
  [ L [ K "i32.const"; N (I32 (Int32.of_int x0)) ];
    L [ K "i32.const"; N (I32 (Int32.of_int x1)) ];
    L [ K "call"; ID "debug2" ]
  ]

let debug2_var x0 var =
  [ L [ K "i32.const"; N (I32 (Int32.of_int x0)) ];
    L [ K "local.get"; ID var ];
    L [ K "call"; ID "debug2" ]
  ]

let stack_init fpad descr =
  (* put zeros into the uninitialized stack positions *)
  let rec gen_init k =
    if k >= 1 then
      let pos = -k in
      let is_used = ISet.mem pos descr.stack_init in
      ( if not is_used then
          push_const 1l @ pop_to_stack fpad pos
        else
          []
      ) @ gen_init (k-1)
    else
      [] in
  gen_init descr.stack_depth

let setup_for_gc fpad descr =
  let sp_decr =
    if descr.stack_save_accu then 2 else 1 in
  let sexpl_stack =
    stack_init fpad descr in
  let sexpl_accu =
    if descr.stack_save_accu then
      push_local "accu" @ pop_to_stack fpad (-descr.stack_depth-1)
    else
      [] in
  let sexpl_env =
    push_local "env" @ pop_to_stack fpad (-descr.stack_depth-sp_decr) in
  let sexpl_extern_sp =
    [ L [ K "local.get";
          ID "fp";
        ];
      L [ K "i32.const";
          N (I32 (Int32.of_int ( 4 * (descr.stack_depth + sp_decr))));
        ];
      L [ K "i32.sub" ];
    ]
    @ pop_to_domain_field domain_field_extern_sp in
  sexpl_stack @ sexpl_accu @ sexpl_env @ sexpl_extern_sp

let restore_after_gc fpad descr =
  let sp_decr =
    if descr.stack_save_accu then 2 else 1 in
  let sexpl_accu =
    if descr.stack_save_accu then
      push_stack fpad (-descr.stack_depth-1) @ pop_to_local "accu"
    else
      [] in
  let sexpl_env =
    push_stack fpad (-descr.stack_depth-sp_decr) @ pop_to_local "env" in
  sexpl_accu @ sexpl_env

let alloc_atom fpad tag =
  [ L [ K "global.get";
        ID "wasicaml_atom_table";
      ];
    L [ K "i32.const";
        N (I32 (Int32.of_int (4 * tag)));
      ];
    L [ K "i32.add" ];
  ]

let alloc_decr =
  [ L [ K "func";
        ID "alloc_decr";
        L [ K "param"; ID "size"; K "i32" ];
        BR;
        L [ K "result"; C "ptr"; K "i32" ];
        L [ K "local"; ID "x"; K "i32" ];

        (* Caml_state_field(young_ptr) = ... *)
        L [ K "global.get"; ID "wasicaml_domain_state" ];

        (* Caml_state_field(young_ptr) - Whsize_wosize (wosize) *)
        L [ K "global.get"; ID "wasicaml_domain_state" ];
        L [ K "i32.load";
            K (sprintf "offset=0x%lx" (Int32.of_int (8 * domain_field_young_ptr)));
            K "align=2"
          ];
        L [ K "local.get"; ID "size" ];
        L [ K "i32.sub" ];
        L [ K "local.tee"; ID "x" ];

        L [ K "i32.store";
            K (sprintf "offset=0x%lx" (Int32.of_int (8 * domain_field_young_ptr)));
            K "align=2"
          ];

        L [ K "local.get"; ID "x" ];
        L [ K "return" ]
      ]
  ]

let alloc_slow =
  [ L ( [ [ K "func";
            ID "alloc_slow";
            L [ K "param"; ID "size"; K "i32" ];
            L [ K "param"; ID "fp"; K "i32" ];
            L [ K "param"; ID "stackdepth"; K "i32" ];
            L [ K "param"; ID "accu"; K "i32" ];
            L [ K "param"; ID "env"; K "i32" ];
            BR;
            L [ K "result"; C "ptr"; K "i32" ];
            L [ K "local"; ID "h.i32"; K "i32" ];
          ];
          if !enable_multireturn then [
              L [ K "result"; C "out_accu"; K "i32" ];
              L [ K "result"; C "out_env"; K "i32" ];
            ] else [];
          [ L [ K "local.get"; ID "fp" ];
            L [ K "local.get"; ID "stackdepth" ];
            L [ K "i32.sub" ];
            L [ K "i32.const"; N (I32 8l) ];
            L [ K "i32.sub" ];

            (* L [ K "local.tee"; ID "h.i32" ];
               L ( K "block" :: debug2_var 20 "h.i32");
             *)
          ]
          @ pop_to_domain_field domain_field_extern_sp
          @ push_local "accu"
          @ pop_to_field "fp" 4
          @ push_local "env"
          @ pop_to_field "fp" 0;

          (* caml_alloc_small_dispatch(size, CAML_FROM_C, 1, NULL); *)
          [ L [ K "local.get"; ID "size" ];
            L [ K "i32.const"; N (I32 (Int32.of_int caml_from_c)) ];
            L [ K "i32.const"; N (I32 1l) ];
            L [ K "i32.const"; N (I32 0l) ];
            L [ K "call"; ID "caml_alloc_small_dispatch" ];
          ];

          (* return Caml_state_field(young_ptr) *)
          push_domain_field domain_field_young_ptr;

          (* return accu, env *)
          push_local "accu";
          push_local "env";

          if !enable_multireturn then [] else
            [ L [ K "global.set"; ID "retval3" ];
              L [ K "global.set"; ID "retval2" ];
            ];

          [ L [ K "return" ]]

        ] |> List.flatten
      )
  ]

let call_alloc_slow =
  [ L [ K "call"; ID "alloc_slow" ]]
  @ if !enable_multireturn then [] else
      [ L [ K "global.get"; ID "retval2" ];
        L [ K "global.get"; ID "retval3" ];
      ]


let alloc_non_atom fpad descr size tag =
  (* TODO: use new push/pop functions *)
  let ptr = new_local fpad RValue in
  let young = size <= max_young_wosize in
  let code =
    if young then
      [ [ L [ K "i32.const";
              N (I32 (Int32.of_int (4 * (size+1))));
            ];
          L [ K "call"; ID "alloc_decr" ];
          L [ K "local.tee"; ID ptr ];
        ];

        (* if (ptr < Caml_state_field(young_limit)) *)
        push_domain_field domain_field_young_limit;
        [ L [ K "i32.lt_u" ] ];
        [ L [ K "if";
              L ( [ [
                      K "then";
                    ];
                    stack_init fpad descr;
                    push_const (Int32.of_int size);
                    push_local "fp";
                    push_const (Int32.of_int (4 * descr.stack_depth));
                    push_local "accu";
                    push_local "env";
                    call_alloc_slow;
                    pop_to_local "env";
                    pop_to_local "accu";
                    pop_to_local ptr;
                ] |> List.flatten
              );
          ];

        L [ K "local.get"; ID ptr ];
        L [ K "i32.const";
            N (I32 (Int32.of_int (make_header size tag)))
          ];
        L [ K "i32.store"; K "align=2" ];
        L [ K "local.get"; ID ptr ];
        L [ K "i32.const"; N (I32 4l) ];
        L [ K "i32.add" ];
      ] ] |> List.flatten
    else
      [ L [ K "i32.const"; N (I32 (Int32.of_int size)) ];
        L [ K "i32.const"; N (I32 (Int32.of_int tag)) ];
        L [ K "call"; ID "caml_alloc_shr" ];
      ] in
  (code, ptr, young)

let alloc fpad descr size tag =
  if size = 0 then
    let ptr = new_local fpad RValue in
    let young = false in
    (alloc_atom fpad tag, ptr, young)
  else
    alloc_non_atom fpad descr size tag

let alloc_set fpad descr size tag =
  let (code, ptr, young) = alloc fpad descr size tag in
  let code = code @ [ L [ K "local.set"; ID ptr ]] in
  (code, ptr, young)

let grab_helper gpad =
  (* generates a helper function:
     $grab_helper(env, extra_args, codeptr, fp)
   *)
  let fpad = empty_fpad() in
  let descr = empty_descr in
  assert(descr.stack_save_accu = false);

  [ L ( [ [ K "func";
            ID "grab_helper";
            L [ K "param"; ID "env"; K "i32" ];
            L [ K "param"; ID "extra_args"; K "i32" ];
            L [ K "param"; ID "codeptr"; K "i32" ];
            L [ K "param"; ID "fp"; K "i32" ];
            BR;
            L [ K "result"; K "i32" ];
            L [ K "local"; ID "accu"; K "i32" ];
            L [ K "local"; ID "i"; K "i32" ];
            L [ K "local"; ID "bp"; K "i32" ];
            L [ K "local"; ID "h.i32"; K "i32" ];
          ];

          [ L [ K "local.get"; ID "fp" ];
            L [ K "local.set"; ID "bp" ]  (* used in setup_for_gc *)
          ];

          setup_for_gc fpad descr;
          [ L [ K "local.get"; ID "extra_args" ];
            L [ K "i32.const"; N (I32 4l) ];
            L [ K "i32.add" ];
            L [ K "i32.const"; N (I32 (Int32.of_int closure_tag))];
            L [ K "call"; ID "caml_alloc_small" ];
            L [ K "local.set"; ID "accu" ];
          ];
          restore_after_gc fpad descr;  (* won't overwrite accu *)

          push_local "env";
          pop_to_field "accu" 2;

          push_const 0l;
          pop_to_local "i";

          [ L [ K "loop"; ID "fields"; BR;
                (* Field(accu, i+3) = ... *)
                L [ K "local.get"; ID "accu" ];
                L [ K "local.get"; ID "i" ];
                L [ K "i32.const"; N (I32 3l) ];
                L [ K "i32.add" ];
                L [ K "i32.const"; N (I32 2l) ];
                L [ K "i32.shl" ];
                L [ K "i32.add" ];

                (* fp[i] *)
                L [ K "local.get"; ID "fp" ];
                L [ K "local.get"; ID "i" ];
                L [ K "i32.const"; N (I32 2l) ];
                L [ K "i32.shl" ];
                L [ K "i32.add" ];
                L [ K "i32.load"; K "align=2" ];

                (* assign *)
                L [ K "i32.store"; K "align=2" ];

                (* i++, and jump back if i <= extra_args *)
                L [ K "local.get"; ID "i" ];
                L [ K "i32.const"; N (I32 1l) ];
                L [ K "i32.add" ];
                L [ K "local.tee"; ID "i" ];
                L [ K "local.get"; ID "extra_args" ];
                L [ K "i32.le_u" ];
                L [ K "br_if"; ID "fields" ];
              ]
          ];

          push_local "codeptr";
          pop_to_field "accu" 0;

          push_const 5l;
          pop_to_field "accu" 1;

          push_local "accu";
          [  L [ K "return" ] ]

        ] |> List.flatten
      )
  ]

let restart_helper gpad =
  [ L ( [ [ K "func";
            ID "restart_helper";
            L [ K "param"; ID "env"; K "i32" ];
            L [ K "param"; ID "extra_args"; K "i32" ];
            L [ K "param"; ID "fp"; K "i32" ];
            BR;
            L [ K "result"; C "out_env"; K "i32" ];
          ];
          if !enable_multireturn then [
            L [ K "result"; C "out_extra_args"; K "i32" ];
            L [ K "result"; C "out_fp"; K "i32" ];
          ] else [];

          [ L [ K "local"; ID "i"; K "i32" ];
            L [ K "local"; ID "num_args"; K "i32" ];
          ];

          [ (* num_args = Wosize_val(env) - 3 *)
            L [ K "local.get"; ID "env" ];
            L [ K "i32.const"; N (I32 4l) ];
            L [ K "i32.sub" ];
            L [ K "i32.load"; K "align=2" ];
            L [ K "i32.const"; N (I32 10l) ];
            L [ K "i32.shr_u" ];
            L [ K "i32.const"; N (I32 3l) ];
            L [ K "i32.sub" ];
            L [ K "local.set"; ID "num_args" ];
          ];

          [ (* fp -= num_args *)
            L [ K "local.get"; ID "fp" ];
            L [ K "local.get"; ID "num_args" ];
            L [ K "i32.const"; N (I32 2l)];
            L [ K "i32.shl" ];
            L [ K "i32.sub" ];
            L [ K "local.set"; ID "fp" ];
          ];

          [ L [ K "i32.const"; N (I32 0l)];
            L [ K "local.set"; ID "i" ];
            L [ K "loop"; ID "args"; BR;
                (* fp[i[ = ... *)
                L [ K "local.get"; ID "fp" ];
                L [ K "local.get"; ID "i" ];
                L [ K "i32.const"; N (I32 2l) ];
                L [ K "i32.shl" ];
                L [ K "i32.add" ];

                (* Field(env, i+3) *)
                L [ K "local.get"; ID "env" ];
                L [ K "local.get"; ID "i" ];
                L [ K "i32.const"; N (I32 3l) ];
                L [ K "i32.add" ];
                L [ K "i32.const"; N (I32 2l) ];
                L [ K "i32.shl" ];
                L [ K "i32.add" ];
                L [ K "i32.load"; K "align=2" ];

                (* assign *)
                L [ K "i32.store"; K "align=2" ];

                (* i++, and jump back if i < num_args *)
                L [ K "local.get"; ID "i" ];
                L [ K "i32.const"; N (I32 1l) ];
                L [ K "i32.add" ];
                L [ K "local.tee"; ID "i" ];
                L [ K "local.get"; ID "num_args" ];
                L [ K "i32.lt_u" ];
                L [ K "br_if"; ID "args" ];
              ]
          ];

          (* env = Field(env, 2) *)
          push_field "env" 2;
          pop_to_local "env";

          (* extra_args += num_args *)
          [ L [ K "local.get"; ID "extra_args" ];
            L [ K "local.get"; ID "num_args" ];
            L [ K "i32.add" ];
            L [ K "local.set"; ID "extra_args" ];
          ];

          (* return env *)
          push_local "env";
          push_local "extra_args";
          push_local "fp";

          if !enable_multireturn then [] else
            [ L [ K "global.set"; ID "retval3" ];
              L [ K "global.set"; ID "retval2" ];
            ];

          [ L [ K "return" ] ];
        ] |> List.flatten
      )
  ]

let call_restart_helper =
  [ L [ K "call"; ID "restart_helper" ]]
  @ if !enable_multireturn then [] else
      [ L [ K "global.get"; ID "retval2" ];
        L [ K "global.get"; ID "retval3" ];
      ]

let reset_frame =
  [ L [ K "func";
        ID "reset_frame";
        L [ K "param"; ID "fp"; K "i32" ];
        L [ K "param"; ID "depth"; K "i32" ];
        L [ K "param"; ID "old_num_args"; K "i32" ];  (* >= 1 *)
        L [ K "param"; ID "new_num_args"; K "i32" ];  (* >= 1 *)
        BR;
        L [ K "result"; C "out_fp"; K "i32" ];
        L [ K "local"; ID "i"; K "i32" ];
        L [ K "local"; ID "new_fp"; K "i32" ];

        (* new_fp = fp + old_num_args - new_num_args *)
        L [ K "local.get"; ID "fp" ];
        L [ K "local.get"; ID "old_num_args" ];
        L [ K "local.get"; ID "new_num_args" ];
        L [ K "i32.sub" ];
        L [ K "i32.const"; N (I32 2l) ];
        L [ K "i32.shl" ];
        L [ K "i32.add" ];
        L [ K "local.set"; ID "new_fp" ];

        L [ K "local.get"; ID "new_num_args" ];
        L [ K "local.set"; ID "i" ];
        L [ K "loop"; ID "args"; BR;
            (* i-- *)
            L [ K "local.get"; ID "i" ];
            L [ K "i32.const"; N (I32 1l) ];
            L [ K "i32.sub" ];
            L [ K "local.set"; ID "i" ];

            (* new_fp[i] = ... *)
            L [ K "local.get"; ID "new_fp" ];
            L [ K "local.get"; ID "i" ];
            L [ K "i32.const"; N (I32 2l) ];
            L [ K "i32.shl" ];
            L [ K "i32.add" ];

            (* fp[-(depth-i)] *)
            L [ K "local.get"; ID "fp" ];
            L [ K "local.get"; ID "depth" ];
            L [ K "local.get"; ID "i" ];
            L [ K "i32.sub" ];
            L [ K "i32.const"; N (I32 2l) ];
            L [ K "i32.shl" ];
            L [ K "i32.sub" ];
            L [ K "i32.load"; K "align=2" ];

            (* assign *)
            L [ K "i32.store"; K "align=2" ];

            (* loop if i > 0 *)
            L [ K "local.get"; ID "i" ];
            L [ K "br_if"; ID "args" ];
          ];
        L [ K "local.get"; ID "new_fp" ];
        L [ K "return" ]
      ];
  ]

let wasicaml_get_data =
  [ L [ K "func";
        ID "wasicaml_get_data";
        (*  L [ K "export"; S "wasicaml_get_data" ]; *)
        L [ K "result"; K "i32" ];
        BR;
        (* PIC: L [ K "global.get"; ID "wc__memory_base" ]; *)
        (* Extension: *)
        L [ K "i32.const"; ID "data" ];
        L [ K "return" ]
      ]
  ]

let wasicaml_get_data_size size =
  [ L [ K "func";
        ID "wasicaml_get_data_size";
        (* L [ K "export"; S "wasicaml_get_data_size" ]; *)
        L [ K "result"; K "i32" ];
        BR;
        L [ K "i32.const"; N (I32 (Int32.of_int size)) ];
        L [ K "return" ]
      ]
  ]

let wasicaml_init =
  [ L [ K "func";
        ID "wasicaml_init";
        (* L [ K "export"; S "wasicaml_init" ]; *)
        BR;
        L [ K "call"; ID "wasicaml_get_global_data" ];
        L [ K "global.set"; ID "wasicaml_global_data" ];
        L [ K "call"; ID "wasicaml_get_domain_state" ];
        L [ K "global.set"; ID "wasicaml_domain_state" ];
        L [ K "call"; ID "wasicaml_get_atom_table" ];
        L [ K "global.set"; ID "wasicaml_atom_table" ];
        L [ K "return" ]
      ]
  ]


let tovalue_alloc fpad repr descr_opt =
  (* transform the value of the Wasm stack to a proper OCaml value,
     and put that back on the Wasm stack *)
  match repr with
    | RValue | RIntVal ->
        []
    | RInt ->
        [ L [ K "i32.const";
              N (I32 1l);
            ];
          L [ K "i32.shl" ];
          L [ K "i32.const";
              N (I32 1l);
            ];
          L [ K "i32.or" ];
        ]
    | RFloat ->
        ( match descr_opt with
            | None -> failwith "cannot convert to double w/o stack descr"
            | Some descr ->
                let (instrs_alloc, _, _) =
                  alloc fpad descr double_size double_tag in
                let local = new_local fpad RFloat in
                let instrs =
                  [ L [ K "local.set"; ID local ] ]
                  @ instrs_alloc
                  @ [ L [ K "local.get"; ID local ];
                      L [ K "f64.store"; K "align=2" ];
                    ] in
                instrs
        )
    | _ ->
        (* TODO: need to allocate the block *)
        (* Careful: when allocating a block and initializing it,
           we cannot allocate in the middle (e.g. makeblock). If allocations
           are generated by tovalue, we need to insert more code to
           set the block early to 0. Probably the way out is a function
           straighten_if_alloc_needed that only straightens the stack
           positions that are neither RValue nor RInt.
         *)
        assert false

let tovalue fpad repr =
  tovalue_alloc fpad repr None

let toint repr =
  match repr with
    | RInt ->
        []
    | RValue | RIntVal ->
        [ L [ K "i32.const";
              N (I32 1l);
            ];
          L [ K "i32.shr_s" ];
        ]
    | _ ->
        assert false

let tofloat repr =
  match repr with
    | RValue ->
        load_double
    | _ ->
        assert false

let convert fpad repr_from repr_to descr_opt =
  (* convert the value on the wasm stack, from repr_from to repr_to *)
  match repr_to with
    | RValue | RIntVal -> tovalue_alloc fpad repr_from descr_opt
    | RInt -> toint repr_from
    | RFloat -> tofloat repr_from
    | _ ->
        assert false (* TODO *)

let push fpad store =
  (* put the value in store onto the wasm stack *)
  match store with
    | RealAccu ->
        push_local "accu"
    | Local(repr, name) ->
        push_local name
    | Const x ->
        push_const (Int32.of_int x)
    | RealStack pos ->
        push_stack fpad pos
    | Atom tag ->
        alloc_atom fpad tag

let push_alloc_as fpad store req_repr descr_opt =
  match store, req_repr with
    | Const x, (RValue | RIntVal) ->
        push_const (Int32.logor (Int32.shift_left (Int32.of_int x) 1) 1l)
    | _ ->
        let sexpl_push = push fpad store in
        let repr = repr_of_store store in
        sexpl_push @ convert fpad repr req_repr descr_opt

let push_as fpad store req_repr =
  push_alloc_as fpad store req_repr None

let pop_to fpad store repr descr_opt =
  match store with
    | RealAccu ->
        tovalue_alloc fpad repr descr_opt
        @ pop_to_local "accu"
    | Local(lrepr, name) ->
        convert fpad repr lrepr descr_opt
        @ pop_to_local name
    | RealStack pos ->
        tovalue_alloc fpad repr descr_opt
        @ pop_to_stack fpad pos
    | _ ->
        assert false

let copy fpad src dest descr_opt =
  match dest with
    | RealAccu ->
        push_as fpad src RValue
        @ pop_to_local "accu"
    | Local(repr, name) ->
        push_as fpad src repr
        @ pop_to_local name
    | RealStack pos ->
        push_as fpad src RValue
        @ pop_to_stack fpad pos
    | _ ->
        assert false

let rec drop n l =
  if n > 0 then
    match l with
      | _ :: l -> drop (n-1) l
      | [] -> []
  else
    l

let emit_unary fpad op src1 dest =
  match op with
    | Pnegint ->
        push_as fpad src1 RIntVal
        @  [ L [ K "i32.const";
                 N (I32 (0xffff_fffel));
               ];
             L [ K "i32.xor" ];
             L [ K "i32.const";
                 N (I32 2l);
               ];
             L [ K "i32.add" ];
           ]
        @ pop_to fpad dest RIntVal None
    | Pboolnot ->
        push_as fpad src1 RIntVal
        @ [ L [ K "i32.const";
                N (I32 2l);
              ];
            L [ K "i32.xor" ]
          ]
        @ pop_to fpad dest RIntVal None
    | Poffsetint offset ->
        push_as fpad src1 RIntVal
        @ [ L [ K "i32.const";
                N (I32 (Int32.shift_left (Int32.of_int offset) 1));
              ];
            L [ K "i32.add" ]
          ]
        @ pop_to fpad dest RIntVal None
    | Pisint ->
        push_as fpad src1 RValue
        @ [ L [ K "i32.const"; N (I32 1l) ];
            L [ K "i32.and" ];
          ]
        @ pop_to fpad dest RInt None
    | Pgetfield field ->
        assert(field >= 0);
        push_as fpad src1 RValue
        @ [ L [ K "i32.load";
                K (sprintf "offset=0x%lx" (Int32.of_int (4 * field)));
                K "align=2";
              ];
          ]
        @ pop_to fpad dest RValue None
    | Pgetfloatfield field ->
        (* dest is here a Local(RFloat,_), so no allocation needed *)
        ( match dest with
            | Local(RFloat, name) ->
                push_as fpad src1 RValue
                @ [ L [ K "f64.load";
                        K (sprintf "offset=0x%lx" (Int32.of_int (4 * double_size * field)));
                        K "align=2";
                      ];
                    L [ K "local.set"; ID name ]
                  ]
            | _ ->
                assert false
        )
    | Pvectlength ->
        (* CHECK: this is long enough for a helper function *)
        let local = new_local fpad RInt in
        push_as fpad src1 RValue
        @ [ L [ K "i32.const"; N (I32 4l) ];
            L [ K "i32.sub" ];
            L [ K "i32.load" ];
            L [ K "local.tee"; ID local ];
            L [ K "local.get"; ID local ];
            L [ K "i32.const"; N (I32 0xffl) ];
            L [ K "i32.and" ];
            L [ K "i32.const";
                N (I32 (Int32.of_int double_array_tag));
              ];
            L [ K "i32.eq" ];  (* 1 if double array, else 0 *)
            L [ K "i32.const";
                N (I32 9l)
              ];
            L [ K "i32.add" ];
            L [ K "i32.shr_u" ];
            (* shift by 10 for double array, else by 9 *)
            L [ K "i32.const";
                N (I32 1l);
              ];
            L [ K "i32.or" ];
          ]
        @ pop_to fpad dest RIntVal None
    | Pgetpubmet k ->
        assert false (* TODO *)

let rec emit_instr fpad instr =
  match instr with
    | Wcomment s ->
        [ C s ]
    | Wblock arg ->
        ( match arg.label with
            | Label lab ->
                [ L ( [ K "block";
                        ID (sprintf "label%d" lab);
                        BR
                      ]
                      @ emit_instrs fpad arg.body
                    )
                ]
            | Loop lab ->
                [ L ( [ K "loop";
                        ID (sprintf "loop%d" lab);
                        BR
                      ]
                      @ emit_instrs fpad arg.body
                    )
                ]
        )
    | Wcopy arg ->
        copy fpad arg.src arg.dest None
    | Walloc arg ->
        copy fpad arg.src arg.dest (Some arg.descr)
    | Wenv arg ->
        push_field "env" arg.field
        @ pop_to_local "accu"
    | Wcopyenv arg ->
        push_local "env"
        @ ( if arg.offset > 0 then
              [ L [ K "i32.const"; N (I32 (Int32.of_int arg.offset)) ];
                L [ K "i32.add" ]
              ]
            else
              []
          )
        @ pop_to_local "accu"
    | Wgetglobal arg ->
        let Global offset = arg.src in
        [ L [ K "global.get";
              ID "wasicaml_global_data";
            ];
          L [ K "i32.load" ];
          L [ K "i32.load";
              K (sprintf "offset=0x%lx" (Int32.of_int (4 * offset)));
              K "align=2";
            ];
        ]
        @ pop_to_local "accu"
    | Wunary arg ->
        emit_unary arg.op arg.src1 arg.dest
    | Wunaryeffect arg ->
        XXX
    | Wbinary arg ->
        XXX
    | Wbinaryeffect arg ->
        XXX
    | Wternaryeffect arg ->
        XXX
    | Wmakeblock arg ->
        XXX
    | Wmakefloatblock arg ->
        XXX
    | Wccall arg ->
        XXX
    | Wccall_vector arg ->
        XXX
    | Wbranch arg ->
        XXX
    | Wbranchif arg ->
        XXX
    | Wbranchifnot arg ->
        XXX
    | Wswitch arg ->
        XXX
    | Wapply arg ->
        XXX
    | Wappterm arg ->
        XXX
    | Wreturn arg ->
        XXX
    | Wgrab arg ->
        XXX
    | Wclosurerec arg ->
        XXX
    | Wraise arg ->
        XXX
    | Wtrap arg ->
        XXX
    | Wtryreturn arg ->
        XXX
    | Wstop ->
        XXX

and emit_instrs fpad instrs =
  List.fold_left
    (fun acc instr ->
      List.rev_append (emit_instr fpad instr) acc
    )
    []
    instrs
  |> List.rev

let nullary_operation gpad lpad state op_repr op_sexpl =
  match op_repr with
    | RValue ->
        let state, sexpl_flush = flush_accu gpad lpad state in
        let state = { state with accu = RealAccu } in
        let sexpl =
          sexpl_flush @ op_sexpl @ pop_to_local "accu" in
        (state, sexpl)
    | RInt ->
        let result = new_local lpad (vtype op_repr) in
        let state = { state with accu = Local(RIntVal, result) } in
        let intval_sexpl = tovalue RInt in
        let sexpl =
          op_sexpl @ op_sexpl @ intval_sexpl @ pop_to_local result in
        (state, sexpl)
    | _ ->
        let result = new_local lpad (vtype op_repr) in
        let state = { state with accu = Local(op_repr, result) } in
        let sexpl =
          op_sexpl @ op_sexpl @ pop_to_local result in
        (state, sexpl)

let unary_operation_norvalue gpad lpad state op_repr op_sexpl =
  assert(op_repr <> RValue);
  let sexpl_push = push_as gpad lpad state state.accu op_repr in
  let res_repr = if op_repr = RInt then RIntVal else op_repr in
  let result = new_local lpad (vtype res_repr) in
  let state = { state with accu = Local(res_repr, result) } in
  let sexpl_cvt = convert op_repr res_repr in
  let sexpl_pop = pop_to_local result in
  (state, sexpl_push @ op_sexpl @ sexpl_cvt @ sexpl_pop)

let unary_operation_rvalue gpad lpad state op_sexpl =
  let state, flush_sexpl = flush_accu gpad lpad state in
  let push_sexpl = push_as gpad lpad state state.accu RValue in
  let sexpl = flush_sexpl @ push_sexpl @ op_sexpl @ pop_to_local "accu" in
  let state = { state with accu = RealAccu; } in
  (state, sexpl)

let unary_operation_rvalue_unit gpad lpad state op_sexpl =
  (* takes an RValue arg, and returns Const 0 *)
  let state, flush_sexpl = flush_accu gpad lpad state in
  let push_sexpl = push_as gpad lpad state state.accu RValue in
  let sexpl = flush_sexpl @ push_sexpl @ op_sexpl in
  let state = { state with accu = Const 0 } in
  (state, sexpl)
  
let unary_operation gpad lpad state op_repr op_sexpl =
  if op_repr = RValue then
    unary_operation_rvalue gpad lpad state op_sexpl
  else
    unary_operation_norvalue gpad lpad state op_repr op_sexpl

let binary_operation_norvalue gpad lpad state op_repr op_sexpl =
  assert(op_repr <> RValue);
  let res_repr = if op_repr = RInt then RIntVal else op_repr in
  let sexpl_push1 = push_as gpad lpad state state.accu op_repr in
  let second = List.hd state.camlstack in
  let sexpl_push2 = push_as gpad lpad state second op_repr in
  let result = new_local lpad (vtype res_repr) in
  let state = pop_camlstack state in
  let state = { state with accu = Local(res_repr, result) } in
  let sexpl_cvt = convert op_repr res_repr in
  let sexpl_pop = pop_to_local result in
  (state, sexpl_push1 @ sexpl_push2 @ op_sexpl @ sexpl_cvt @ sexpl_pop)

let binary_operation_rvalue gpad lpad state op_sexpl =
  (* Logically, first operand is in accu, second operand on top of stack.
     For running it, put first operand first on wasm stack, then the
     second operand on top.
   *)
  let second = List.hd state.camlstack in
  let state, sexpl1 = flush_accu gpad lpad state in
  let sexpl3 = push_as gpad lpad state state.accu RValue in
  let sexpl4 = push_as gpad lpad state second RValue in
  let state = pop_camlstack state in
  let state = { state with accu = RealAccu } in
  let sexpl =
    sexpl1 @ sexpl3 @ sexpl4 @ op_sexpl @ pop_to_local "accu" in
  (state, sexpl)

let binary_operation gpad lpad state op_repr op_sexpl =
  if op_repr = RValue then
    binary_operation_rvalue gpad lpad state op_sexpl
  else
    binary_operation_norvalue gpad lpad state op_repr op_sexpl

let ternary_operation_rvalue gpad lpad state op_sexpl =
  (* Logically: 1. accu, 2. stack top, 3. stack top - 1.
     Wasm: put the args in this order on the wasm stack
   *)
  let second = List.hd state.camlstack in
  let third = List.hd (List.tl state.camlstack) in
  let state, sexpl1 = flush_accu gpad lpad state in
  let sexpl4 = push_as gpad lpad state state.accu RValue in
  let sexpl5 = push_as gpad lpad state second RValue in
  let sexpl6 = push_as gpad lpad state third RValue in
  let state = popn_camlstack state 2 in
  let state = { state with accu = RealAccu } in
  let sexpl =
    sexpl1 @ sexpl4 @ sexpl5 @ sexpl6 @ op_sexpl @ pop_to_local "accu" in
  (state, sexpl)

let makeblock gpad lpad state start vars size tag =
  (* alloc a block of [size] and with [tag]. The fields from [start] to
     [start+vars-1] are initialized with values from the accu and the
     top of the stack. Returns the block in accu.
   *)
  let sexpl_alloc, ptr, young = alloc_set gpad lpad state size tag in
  let fields = enum start vars in
  let camlstack = Array.of_list state.camlstack in
  let store_of_field field =
    if field=start then state.accu else camlstack.(field-start-1) in
  let sexpl_init =
    if young then
      List.map
        (fun field ->
          let store = store_of_field field in
          push_as gpad lpad state store RValue
          @ pop_to_field ptr field
        )
        fields
      |> List.flatten
    else
      List.map
        (fun field ->
          let store = store_of_field field in
          push_field_addr ptr field
          @ push_as gpad lpad state store RValue
          @ [ L [ K "call";
                  ID "caml_initialize"
                ]
            ]
        )
        fields
      |> List.flatten in
  let state =
    if vars > 0 then popn_camlstack state (vars-1) else state in
  let state, sexpl_flush = flush_accu gpad lpad state in
  let state = { state with accu = RealAccu } in
  let sexpl_set = push_local ptr @ pop_to_local "accu" in
  (state, sexpl_alloc @ sexpl_init @ sexpl_flush @ sexpl_set)

let makefloatblock gpad lpad state size =
  let wosize = size * double_size in
  let sexpl_alloc, ptr, _ = alloc_set gpad lpad state wosize double_array_tag in
  let fields = enum 0 size in
  let camlstack = Array.of_list state.camlstack in
  let store_of_field field =
    if field=0 then state.accu else camlstack.(field-1) in
  let sexpl_init =
    List.map
      (fun field ->
        let store = store_of_field field in
        push_as gpad lpad state store RValue
        @ load_double
        @ pop_to_double_field ptr field
      )
      fields
    |> List.flatten in
  let state = popn_camlstack state (size-1) in
  let state, sexpl_flush = flush_accu gpad lpad state in
  let state = { state with accu = RealAccu } in
  let sexpl_set =
    push_local ptr @ pop_to_local "accu" in
  (state, sexpl_alloc @ sexpl_init @ sexpl_flush @ sexpl_set)

let c_call gpad lpad state name num_args =
  let state, sexpl =
    if num_args <= 5 then
      let state, sexpl_flush = flush_accu gpad lpad state in
      let rdepth = realdepth state in
      let sexpl_setup = setup_for_c_call gpad lpad state 0 rdepth in
      let sexpl_restore = restore_after_c_call gpad lpad state 0 rdepth in
      let args = enum 0 num_args in
      let camlstack = Array.of_list state.camlstack in
      let store_of_arg k =
        if k = 0 then state.accu else camlstack.(k-1) in
      let sexpl_args =
        List.map
          (fun arg ->
            let store = store_of_arg arg in
            push_as gpad lpad state store RValue
          )
          args
        |> List.flatten in
      let p_i32 = L [ K "param"; K "i32" ] in
      let r_i32 = L [ K "result"; K "i32" ] in
      let ty = (args |> List.map (fun _ -> p_i32)) @ [ r_i32 ] in
      Hashtbl.replace gpad.primitives name ty;
      ( state,
        sexpl_flush
        @ sexpl_setup
        @ sexpl_args
        (* @ debug2 19 0 *)
        @ [ L [ K "call";
                ID name
              ]
          ]
        @ pop_to_local "accu"
        @ sexpl_restore
      )
    else
      let state, sexpl_flush = flush_accu gpad lpad state in
      let state, sexpl_stack_fixup =
        List.fold_left
          (fun (state, sexpl) k ->
            straighten_stack_at gpad lpad state (-state.camldepth+k)
          )
          (state, [])
          (enum 0 (num_args-1)) in
      let sexpl_setup = setup_for_c_call gpad lpad state 1 state.camldepth in
      let sexpl_restore = restore_after_c_call gpad lpad state 1 state.camldepth in
      let sexpl_accu =
        push_as gpad lpad state state.accu RValue
        @ pop_to_stack state (-state.camldepth-1) in
      let ty =
        [ L [ K "param"; K "i32" ];
          L [ K "param"; K "i32" ];
          L [ K "result"; K "i32" ] ] in
      Hashtbl.replace gpad.primitives name ty;
      ( state,
        sexpl_flush
        @ sexpl_stack_fixup
        @ sexpl_accu
        @ sexpl_setup
        @ push_field_addr "fp" (-state.camldepth-1)
        @ push_const (Int32.of_int num_args)
        (* @ debug2 19 0 *)
        @ [ L [ K "call";
                ID name
              ]
          ]
        @ pop_to_local "accu"
        @ sexpl_restore
      ) in
  let state = popn_camlstack state (num_args-1) in
  let state = { state with accu = RealAccu } in
  (state, sexpl)

let string_label lpad label =
  if ISet.mem label lpad.loops then
    sprintf "loop%d" label
  else
    sprintf "label%d" label

let switch gpad lpad state labls_ints labls_blocks =
  lpad.need_panic <- true;
  let value = new_local lpad TI32 in
  push_as gpad lpad state state.accu RValue
  @ pop_to_local value
  @ [ (* if (!Is_block(value)) *)
      L [ K "local.get";
          ID value;
        ];
      L [ K "i32.const";
          N (I32 1l);
        ];
      L [ K "i32.and" ];
      L [ K "if";
          L [ K "then";

              L [ K "local.get";
                  ID value;
                ];
              L [ K "i32.const";
                  N (I32 1l);
                ];
              L [ K "i32.shr_s" ];

              L ( [  K "br_table" ]
                  @ ( Array.map
                        (fun lab -> ID (string_label lpad lab))
                        labls_ints
                      |>  Array.to_list
                    )
                  @ [ ID "panic" ]
                );
            ];

          L [ K "else";

              L [ K "local.get"; ID value ];
              L [ K "i32.const"; N (I32 4l) ];
              L [ K "i32.sub" ];
              L [ K "i32.load"; K "align=2" ];
              L [ K "i32.const"; N (I32 0xffl) ];
              L [ K "i32.and" ];

              L ( [  K "br_table" ]
                  @ ( Array.map
                        (fun lab -> ID (string_label lpad lab))
                        labls_blocks
                      |>  Array.to_list
                    )
                  @ [ ID "panic" ]
                );
            ];
        ]
    ]

let grab gpad lpad state num =
  let sexpl =
    [ (* if (codeptr & 1) *)
      L [ K "local.get"; ID "codeptr" ];
      L [ K "i32.const"; N (I32 1l) ];
      L [ K "i32.and" ];
      L [ K "if";
          L ( [ K "then";
                (* RESTART *)
                (* (env, extra_args, fp) = restart_helper(env, extra_args, fp) *)
                L [ K "local.get"; ID "env" ];
                L [ K "local.get"; ID "extra_args" ];
                L [ K "local.get"; ID "fp" ];
              ]
              @ call_restart_helper
              @ [ L [ K "local.set"; ID "fp" ];
                  L [ K "local.set"; ID "extra_args" ];
                  L [ K "local.set"; ID "env" ];

                  (* codeptr &= ~1 *)
                  L [ K "local.get"; ID "codeptr" ];
                  L [ K "i32.const"; N (I32 0xffff_fffel) ];
                  L [ K "i32.and" ];
                  L [ K "local.set"; ID "codeptr" ];
                ]
            )
        ];

      (* regular GRAB *)
      L [ K "local.get"; ID "extra_args" ];
      L [ K "i32.const"; N (I32 (Int32.of_int num)) ];
      L [ K "i32.ge_u" ];
      L [ K "if";
          L [ K "then";
              L [ K "local.get"; ID "extra_args" ];
              L [ K "i32.const"; N (I32 (Int32.of_int num)) ];
              L [ K "i32.sub" ];
              L [ K "local.set"; ID "extra_args" ];
            ];
          L [ K "else";
              L [ K "local.get"; ID "env" ];
              L [ K "local.get"; ID "extra_args" ];
              L [ K "local.get"; ID "codeptr" ];
              L [ K "i32.const"; N (I32 1l) ];
              L [ K "i32.or" ];  (* codeptr of RESTART *)
              L [ K "local.get"; ID "fp" ];
              L [ K "call"; ID "grab_helper" ];
              (* L ( K "block" :: debug2 1 0); *)
              L [ K "return" ];
            ]
        ];
    ] in
  (state, sexpl)

let return =
  [ C "$return";
    L [ K "local.get"; ID "extra_args" ];
    L [ K "if";
        L ( [ K "then";
              L [ K "local.get"; ID "accu" ];
              L [ K "local.get"; ID "extra_args" ];
              L [ K "i32.const"; N (I32 1l) ];
              L [ K "i32.sub" ];
            ]
            @ push_field "accu" 0
            @ [ L [ K "local.tee"; ID "codeptr" ];

                (* L [ K "i32.const"; N (I32 4l) ];
                   L [K "local.get"; ID "codeptr" ];
                   L [K "call"; ID "debug2" ];
                 *)

                L [ K "local.get"; ID "fp" ];
                L [ K "local.get"; ID "codeptr" ];
                L [ K "i32.const"; N (I32 (Int32.of_int code_pointer_shift)) ];
                L [ K "i32.shr_u" ];
                L [ K "call_indirect";
                    N (I32 0l);    (* table *)
                    L [ K "param"; K "i32" ];
                    L [ K "param"; K "i32" ];
                    L [ K "param"; K "i32" ];
                    L [ K "param"; K "i32" ];
                    L [ K "result"; K "i32" ];
                  ];
                L [ K "local.set"; ID "accu" ];
              ]
          )
      ];
    L [ K "local.get"; ID "accu" ];
    (* L ( K "block" :: debug2 1 1); *)
    L [ K "return" ];
  ]

let apply_code lpad state extra_args sp_decr env_pos =
  let codeptr = new_local lpad TI32 in
  [ L [ K "local.get"; ID "accu" ];
    L [ K "i32.const"; N (I32 (Int32.of_int extra_args)) ];
  ]
  @ push_field "accu" 0
  @ [ L [ K "local.tee"; ID codeptr ];

      (*
      L [ K "i32.const"; N (I32 3l) ];
      L [ K "local.get"; ID codeptr ];
      L [ K "call"; ID "debug2" ];
       *)

      L [ K "local.get"; ID "fp" ];
      L [ K "i32.const"; N (I32 (Int32.of_int (4 * sp_decr))) ];
      L [ K "i32.sub" ];
      L [ K "local.get"; ID codeptr ];
      L [ K "i32.const"; N (I32 (Int32.of_int code_pointer_shift)) ];
      L [ K "i32.shr_u" ];

      (*
      L [ K "local.set"; ID "h.i32" ];
      L [ K "i32.const"; N (I32 2l) ];
      L [ K "local.get"; ID "h.i32" ];
      L [ K "call"; ID "debug2" ];
      L [ K "local.get"; ID "h.i32" ];
       *)

      L [ K "call_indirect";
          N (I32 0l);     (* table index *)
          L [ K "param"; K "i32" ];
          L [ K "param"; K "i32" ];
          L [ K "param"; K "i32" ];
          L [ K "param"; K "i32" ];
          L [ K "result"; K "i32" ];
        ];
      L [ K "local.set"; ID "accu" ];
    ]
  (* @ debug2_var 51 "accu" *)
  @ push_stack state env_pos   (* env might have been moved by GC *)
  @ pop_to_local "env"

let apply123 gpad lpad state num =
  let state, sexpl_str = straighten_all gpad lpad state in
  let sexpl_move =
    enum 0 num
    |> List.map
         (fun k ->
           push_stack state (-state.camldepth+k)
           @ pop_to_stack state (-state.camldepth+k-3)
         )
    |> List.flatten in
  let sexpl_frame =
    push_const 1l   (* instead of pc *)
    @ pop_to_stack state (-state.camldepth+num-3)
    @ push_local "env"
    @ pop_to_stack state (-state.camldepth+num-2)
    @ push_const 1l  (* instead of extra_args *)
    @ pop_to_stack state (-state.camldepth+num-1) in
  let sexpl_call =
    apply_code lpad state (num-1) (state.camldepth+3) (-state.camldepth+num-2) in
  let sexpl = sexpl_str @ sexpl_move @ sexpl_frame @ sexpl_call in
  let state = popn_camlstack state num in
  (state, sexpl)

let apply4plus gpad lpad state num =
  let state, sexpl_str = straighten_all gpad lpad state in
  let sexpl =
    apply_code lpad state (num-1) state.camldepth (-state.camldepth + num + 1) in
  let state = popn_camlstack state (num+3) in
  (state, sexpl_str @ sexpl)

let appterm_common =
  let sexpl_frame =
    (* TODO: for small num, expand reset_frame *)
    (* debug2_var 40 "accu" *)
    push_local "fp"
    @ push_local "appterm_depth"
    @ push_local "appterm_old_num_args"
    @ push_local "appterm_new_num_args"
    @ [ L [ K "call"; ID "reset_frame" ] ]
    @ pop_to_local "fp" in
  let sexpl_regs =
    push_local "extra_args"
    @ push_local "appterm_new_num_args"
    @ [ L [ K "i32.add" ]]
    @ pop_to_local "extra_args"
    @ push_local "accu"
    @ pop_to_local "env"
    @ push_field "accu" 0
    @ pop_to_local "appterm_codeptr" in
  let sexpl_call =
    [ L [ K "local.get"; ID "appterm_codeptr" ];
      L [ K "i32.const"; N (I32 code_pointer_letrec_mask) ];
      L [ K "i32.and" ];
      L [ K "local.get"; ID "codeptr" ];
      L [ K "i32.const"; N (I32 code_pointer_letrec_mask) ];
      L [ K "i32.and" ];
      L [ K "i32.eq" ];
      L [ K "if";
          L [ K "then" ;
              (* same letrec: we can jump! *)
              L [ K "local.get"; ID "extra_args" ];
              L [ K "i32.const"; N (I32 1l) ];
              L [ K "i32.sub" ];
              L [ K "local.set"; ID "extra_args" ];
              L [ K "local.get"; ID "appterm_codeptr" ];
              L [ K "local.set"; ID "codeptr" ];
              L [ K "br"; ID "startover" ]
            ];
          L [ K "else";
              (* different letrec: call + return. In the future,
                 this can be turned to a tailcall *)
              (* we can jump to $return if we adjust a bit... *)
              L [ K "local.get"; ID "env" ];
              L [ K "local.set"; ID "accu" ];
              L [ K "br"; ID "return" ];
            ]
        ]
    ] in
  [ C "$appterm_common" ]
  @ sexpl_frame @ sexpl_regs @ sexpl_call


let appterm gpad lpad state num slots =
  let state, sexpl_accu =
    straighten_accu gpad lpad state in
  let state, sexpl_stack =
    straighten_stack_multi gpad lpad state (enum (-state.camldepth) num) in
  let sexpl =
    push_const (Int32.of_int state.camldepth)
    @ pop_to_local "appterm_depth"
    @ push_const (Int32.of_int (slots - state.camldepth))
    @ pop_to_local "appterm_old_num_args"
    @ push_const (Int32.of_int num)
    @ pop_to_local "appterm_new_num_args"
    @ [ L [ K "br"; ID "appterm_common" ] ] in
  lpad.need_appterm_common <- true;
  lpad.need_return <- true;
  (state, sexpl)

let lookup_label gpad lab =
  let letrec_label, subfunc =
    try Hashtbl.find gpad.funcmapping lab
    with Not_found -> assert false in
  let wasmindex =
    try Hashtbl.find gpad.wasmindex letrec_label
    with Not_found -> assert false in
  (wasmindex, letrec_label, subfunc)

let push_wasmptr gpad lab =
  (* For PIC: *)
  (*
  let wasmindex, subfunc = lookup_label gpad lab in
  [ L [ K "global.get"; ID "__table_base" ];
    L [ K "i32.const"; N (I32 (Int32.of_int wasmindex)) ];
    L [ K "i32.add" ];
  ]
   *)
  (* For statically linked WASM. Note that this way of taking the
     address of a function is not officially supported in the wat file format.
   *)
  let wasmindex, letrec_label, subfunc = lookup_label gpad lab in
  [ L [ K "i32.const"; ID (Hashtbl.find gpad.letrec_name letrec_label) ]]

let push_codeptr gpad lab =
  let wasmindex, letrec_label, subfunc = lookup_label gpad lab in
  push_wasmptr gpad lab
  @ [ L [ K "i32.const"; N (I32 (Int32.of_int code_pointer_shift)) ];
      L [ K "i32.shl" ]
    ]
  @ (if subfunc <> 0 then
      [ L [ K "i32.const"; N (I32 (Int32.of_int (subfunc lsl 1))) ];
        L [ K "i32.or" ];
      ]
     else [])

let closure gpad lpad state lab num =
  let (state, sexpl_alloc) =
    makeblock gpad lpad state 2 num (num + 2) closure_tag in
  let sexpl_codeval =
    push_codeptr gpad lab
    @ pop_to_field "accu" 0
    (* @ [ L [ K "i32.const"; (N (I32 5l)) ];
       L [ K "local.get"; ID "accu" ];
       L [ K "i32.load" ];
       L [ K "call"; ID "debug2" ]
      ]
     *)
  in
  let sexpl_closinfo =
    push_const 5l
    @ pop_to_field "accu" 1 in
  (state, sexpl_alloc @ sexpl_codeval @ sexpl_closinfo)

let closurerec gpad lpad state func_labs nvars =
  let nfuncs = List.length func_labs in
  let envofs = nfuncs * 3 - 1 in
  let blksize = envofs + nvars in
  let (state, sexpl_alloc) =
    makeblock gpad lpad state envofs nvars blksize closure_tag in
  let (state, envofs, sexpl_init) =
    List.mapi (fun i func_lab -> (i, func_lab)) func_labs
    |> List.fold_left
         (fun (state, envofs, sexpl) (i, func_lab) ->
           let sexpl_cp =
             push_codeptr gpad func_lab
             @ pop_to_field "accu" (3*i) in
           let sexpl_closinfo =
             push_const (Int32.of_int ((envofs lsl 1) + 1))
             @ pop_to_field "accu" (3*i+1) in
           let sexpl_addr =
             push_field_addr "accu" (3*i)
             @ pop_to_stack state (-state.camldepth-1) in
           let sexpl_infix =
             push_const (Int32.of_int (make_header (i*3) infix_tag))
             @ pop_to_field "accu" (3*i-1) in
           let sexpl =
             sexpl @
               if i = 0 then
                 sexpl_cp @ sexpl_closinfo @ sexpl_addr
               else
                 sexpl_infix @ sexpl_cp @ sexpl_closinfo @ sexpl_addr in
           let state =
             push_camlstack (RealStack (-state.camldepth-1)) state in
           (state, envofs, sexpl)
         )
         (state, envofs, []) in
  let state = { state with accu = Invalid } in
  (state, sexpl_alloc @ sexpl_init)

let global_offset ident =
  assert(Ident.global ident);
  let name = Ident.name ident in
  int_of_string name

let emit_instr gpad lpad state instr =
  match instr with
    | I.Klabel _ -> assert false
    | Kconst (Lambda.Const_base (Asttypes.Const_int k)) ->
        ( { state with accu = Const k }, [])
    | Kconst _ ->
        assert false
    | Kacc sp ->
        let state =
          if sp < state.camldepth then
            { state with accu = List.nth state.camlstack sp }
          else
            { state with accu = RealStack (-state.camldepth + sp) } in
        (state, [])
    | Kpush ->
        let state, sexpl = straighten_accu_when_on_stack gpad lpad state in
        let state = push_camlstack state.accu state in
        (state, sexpl)
    | Kpop num ->
        let cd = state.camldepth in
        let state, sexpl =
          flush_real_stack gpad lpad state (-cd) (-cd+num-1) in
        let state = popn_camlstack state num in
        (state, sexpl)
    | Kassign sp ->
        let cd = state.camldepth in
        let state, sexpl =
          straighten_accu_when_on_stack gpad lpad state in
        let state, sexpl =
          flush_real_stack gpad lpad state (-cd+sp) (-cd+sp) in
        let camlstack =
          List.mapi
            (fun i store ->
              if i = sp then
                state.accu
              else
                store
            )
            state.camlstack in
        let realaccu =
          if state.accu = RealAccu then
            ISet.add (-cd+sp) state.realaccu
          else
            state.realaccu in
        let state = { state with camlstack; realaccu } in
        (state, sexpl)
    | Kenvacc field ->
        let sexpl =
          push_field "env" field
          (*
          @ [ L [ K "local.set"; ID "h.i32" ]]
          @ debug2_var 50 "h.i32"
          @ [ L [ K "local.get"; ID "h.i32" ]]
           *)
        in
        nullary_operation gpad lpad state RValue sexpl
    | Kgetglobal ident ->
        let offset = global_offset ident in
        assert(offset >= 0);
        let sexpl =
          [ L [ K "global.get";
                ID "wasicaml_global_data";
              ];
            L [ K "i32.load" ];
            L [ K "i32.load";
                K (sprintf "offset=0x%lx" (Int32.of_int (4 * offset)));
                K "align=2";
              ];
          ] in
        nullary_operation gpad lpad state RValue sexpl
    | Knegint ->
        let negate_sexpl =
          [ L [ K "i32.const";
                N (I32 (0xffff_fffel));
              ];
            L [ K "i32.xor" ];
            L [ K "i32.const";
                N (I32 2l);
              ];
            L [ K "i32.add" ];
          ] in
        unary_operation gpad lpad state RIntVal negate_sexpl
    | Kboolnot ->
        let not_sexpl =
          [ L [ K "i32.const";
                N (I32 2l);
              ];
            L [ K "i32.xor" ]
          ] in
        unary_operation gpad lpad state RIntVal not_sexpl
    | Koffsetint offset ->
        let offset_sexpl =
          [ L [ K "i32.const";
                N (I32 (Int32.shift_left (Int32.of_int offset) 1));
              ];
            L [ K "i32.add" ]
          ] in
        unary_operation gpad lpad state RIntVal offset_sexpl
    | Koffsetref offset ->
        (* Field(accu, 0) += offset *)
        let local = new_local lpad TI32 in
        let offset_sexpl =
          [ L [ K "local.tee"; ID local ];
            L [ K "local.get"; ID local ];
            L [ K "i32.load"; K "align=2" ];
            L [ K "i32.const";
                N (I32 (Int32.shift_left (Int32.of_int offset) 1));
              ];
            L [ K "i32.add" ];
            L [ K "i32.store";  K "align=2" ];
          ] in
        unary_operation_rvalue_unit gpad lpad state offset_sexpl
    | Kisint ->
        let isint_sexpl =
          [ L [ K "i32.const"; N (I32 1l) ];
            L [ K "i32.and" ];
            L [ K "i32.const"; N (I32 1l) ];
            L [ K "i32.shl" ];
            L [ K "i32.const"; N (I32 1l) ];
            L [ K "i32.or" ]
          ] in
        unary_operation gpad lpad state RValue isint_sexpl
    | Ksetglobal ident ->
        let offset = global_offset ident in
        let local = new_local lpad TI32 in
        let sexpl =
          (* debug2 6 offset @ *)
          [ L [ K "local.set";
                ID local
              ];
            L [ K "global.get";
                ID "wasicaml_global_data";
              ];
            L [ K "i32.load" ];
            L [ K "i32.const";
                N (I32 (Int32.of_int (4 * offset)));
              ];
            L [ K "i32.add" ];
            L [ K "local.get";
                ID local
              ];
            L [ K "call";
                ID "caml_modify";
              ];
          ] in
        unary_operation_rvalue_unit gpad lpad state sexpl
    | Kgetfield field ->
        assert(field >= 0);
        let sexpl =
          [ L [ K "i32.load";
                K (sprintf "offset=0x%lx" (Int32.of_int (4 * field)));
                K "align=2";
              ];
          ] in
        unary_operation gpad lpad state RValue sexpl
    | Kgetfloatfield field ->
        assert(field >= 0);
        let (sexpl_alloc, ptr, _) =
          alloc_set gpad lpad state double_size double_tag in
        let local = new_local lpad TI32 in
        let sexpl =
          sexpl_alloc
          @ [ L [ K "f64.load";
                  K (sprintf "offset=0x%lx" (Int32.of_int (4 * double_size * field)));
                  K "align=2";
                ];
              L [ K "local.set"; ID local ];
              L [ K "local.get"; ID ptr ];
              L [ K "local.get"; ID local ];
              L [ K "f64.store"; K "align=2" ];
            ] in
        unary_operation gpad lpad state RValue sexpl
    | Kvectlength ->
        let local = new_local lpad TI32 in
        let sexpl =
          [ L [ K "i32.const"; N (I32 4l) ];
            L [ K "i32.sub" ];
            L [ K "i32.load" ];
            L [ K "local.tee"; ID local ];
            L [ K "local.get"; ID local ];
            L [ K "i32.const"; N (I32 0xffl) ];
            L [ K "i32.and" ];
            L [ K "i32.const";
                N (I32 (Int32.of_int double_array_tag));
              ];
            L [ K "i32.eq" ];  (* 1 if double array, else 0 *)
            L [ K "i32.const";
                N (I32 9l)
              ];
            L [ K "i32.add" ];
            L [ K "i32.shr_u" ];
            (* shift by 10 for double array, else by 9 *)
            L [ K "i32.const";
                N (I32 1l);
              ];
            L [ K "i32.or" ];
          ] in
        unary_operation gpad lpad state RValue sexpl
    | Kaddint ->
        let add_sexpl =
          [ L [ K "i32.add" ];
            L [ K "i32.const"; N(I32 1l) ];
            L [ K "i32.sub" ]
          ] in
        binary_operation gpad lpad state RIntVal add_sexpl
    | Ksubint ->
        let sub_sexpl =
          [ L [ K "i32.sub" ];
            L [ K "i32.const"; N(I32 1l) ];
            L [ K "i32.add" ]
          ] in
        binary_operation gpad lpad state RIntVal sub_sexpl
    | Kmulint ->
        let mul_sexpl = [ L [ K "i32.mul" ] ] in
        binary_operation gpad lpad state RInt mul_sexpl
    | Kdivint ->
        (* TODO: division by zero check *)
        let div_sexpl = [ L [ K "i32.div_s" ] ] in
        binary_operation gpad lpad state RInt div_sexpl
    | Kmodint ->
        (* TODO: division by zero check *)
        (* CHECK: is this the right remainder function? Needs to be like % in C *)
        let mod_sexpl = [ L [ K "i32.mod_s" ] ] in
        binary_operation gpad lpad state RInt mod_sexpl
    | Kandint ->
        let and_sexpl = [ L [ K "i32.and" ] ] in
        binary_operation gpad lpad state RIntVal and_sexpl
    | Korint ->
        let or_sexpl = [ L [ K "i32.or" ] ] in
        binary_operation gpad lpad state RIntVal or_sexpl
    | Kxorint ->
        let xor_sexpl =
          [ L [ K "i32.xor" ];
            L [ K "i32.const"; N (I32 1l) ];
            L [ K "i32.or" ];
          ] in
        binary_operation gpad lpad state RIntVal xor_sexpl
    | Klslint ->
        let lsl_sexpl = [ L [ K "i32.shl" ] ] in
        binary_operation gpad lpad state RInt lsl_sexpl
    | Klsrint ->
        let lsr_sexpl = [ L [ K "i32.shr_u" ] ] in
        binary_operation gpad lpad state RInt lsr_sexpl
    | Kasrint ->
        let asr_sexpl = [ L [ K "i32.shr_s" ] ] in
        binary_operation gpad lpad state RInt asr_sexpl
    | Kintcomp op ->
        let wasm_op =
          match op with
            | Ceq -> "i32.eq"
            | Cne -> "i32.ne"
            | Clt -> "i32.lt_s"
            | Cle -> "i32.le_s"
            | Cgt -> "i32.gt_s"
            | Cge -> "i32.ge_s" in
        let cmp_sexpl =
          [ L [ K wasm_op ] ]
          @ convert RInt RIntVal in
        binary_operation gpad lpad state RIntVal cmp_sexpl
    | Kisout ->
        let cmp_sexpl = [ L [ K "i32.gt_u" ]] in
        binary_operation gpad lpad state RInt cmp_sexpl
    | Ksetfield field ->
        let local = new_local lpad TI32 in
        let sexpl =
          [ L [ K "local.set";
                ID local
              ];
            L [ K "i32.const";
                N (I32 (Int32.of_int (4 * field)));
              ];
            L [ K "i32.add" ];
            L [ K "local.get";
                ID local
              ];
            L [ K "call";
                ID "caml_modify"
              ];
            L [ K "i32.const";
                N (I32 1l)
              ]
          ] in
        binary_operation gpad lpad state RValue sexpl
    | Ksetfloatfield field ->
        assert(field >= 0);
        let sexpl =
          [ L [ K "f64.load"; K "align=2" ];
            L [ K "f64.store";
                K (sprintf "offset=0x%lx" (Int32.of_int (8 * field)));
                K "align=2";
              ];
            L [ K "i32.const";
                N (I32 1l);
              ]
          ] in
        binary_operation gpad lpad state RValue sexpl
    | Kgetvectitem ->
        (* wasm stack: 1. array, 2. index *)
        let sexpl =
          [ L [ K "i32.const";
                N (I32 1l);
              ];
            L [ K "i32.shl" ];
            L [ K "i32.const";
                N (I32 0xffff_fffcl);
              ];
            L [ K "i32.and" ];
            L [ K "i32.add" ];
            L [ K "i32.load";
                K "align=2"
              ];
          ] in
        binary_operation gpad lpad state RValue sexpl
    | Kgetstringchar | Kgetbyteschar ->
        let sexpl =
          [ L [ K "i32.const";
                N (I32 1l);
              ];
            L [ K "i32.shr_s" ];
            L [ K "i32.add" ];
            L [ K "i32.load8_u" ];
            L [ K "i32.const";
                N (I32 1l);
              ];
            L [ K "i32.shl" ];
            L [ K "i32.const";
                N (I32 1l);
              ];
            L [ K "i32.or" ]
          ] in
        binary_operation gpad lpad state RValue sexpl
    | Ksetvectitem ->
        (* wasm stack: 1. array, 2. index, 3. value *)
        let local = new_local lpad TI32 in
        let sexpl =
          [ L [ K "local.set";
                ID local;
              ];
            L [ K "i32.const";
                N (I32 1l);
              ];
            L [ K "i32.shl" ];
            L [ K "i32.const";
                N (I32 0xffff_fffcl);
              ];
            L [ K "i32.and" ];
            L [ K "i32.add" ];
            L [ K "local.get";
                ID local
              ];
            L [ K "caml_modify" ];
            L [ K "i32.const";
                N (I32 1l);
              ];
          ] in
        ternary_operation_rvalue gpad lpad state sexpl
    | Ksetbyteschar ->
        (* wasm stack: 1. string, 2. index, 3. value *)
        (* FIXME: it would be better if there was a variant of ternary_op
           that puts the args in the reverse order on the wasm stack *)
        let local = new_local lpad TI32 in
        let sexpl =
          [ L [ K "i32.const"; N (I32 1l) ];
            L [ K "i32.shr_s" ];
            L [ K "local.set"; ID local ];
            L [ K "i32.const"; N (I32 1l) ];
            L [ K "i32.shr_s" ];
            L [ K "i32.add" ];
            L [ K "local.get"; ID local ];
            L [ K "i32.store8" ];
            L [ K "i32.const"; N (I32 1l) ];
          ] in
        ternary_operation_rvalue gpad lpad state sexpl
    | Kmakeblock (size, tag) ->
        makeblock gpad lpad state 0 size size tag
    | Kmakefloatblock size ->
        makefloatblock gpad lpad state size
    | Kccall (name, num) ->
        c_call gpad lpad state name num
    | Kbranch lab ->
        (* TODO opt: if the branch target immediately loads the accu,
           we don't need to straighten it up. *)
        let state, sexpl_str = straighten_all gpad lpad state in
        ( state, sexpl_str @ [ L [ K "br"; ID (string_label lpad lab) ] ] )
    | Kbranchif lab ->
        let state, sexpl_str = straighten_all gpad lpad state in
        let sexpl =
          sexpl_str
          @ push_as gpad lpad state state.accu RIntVal
          @ [ L [ K "i32.const"; N (I32 1l) ];
              L [ K "i32.gt_u" ];
              L [ K "br_if"; ID (string_label lpad lab) ]
            ] in
        (state, sexpl)
    | Kbranchifnot lab ->
        let state, sexpl_str = straighten_all gpad lpad state in
        let sexpl =
          sexpl_str
          @ push_as gpad lpad state state.accu RIntVal
          @ [ L [ K "i32.const"; N (I32 1l) ];
              L [ K "i32.le_u" ];
              L [ K "br_if"; ID (string_label lpad lab) ] ] in
        (state, sexpl)
    | Kswitch (labls_ints, labls_blocks) ->
        let state, sexpl_str = straighten_all gpad lpad state in
        (state, sexpl_str @ switch gpad lpad state labls_ints labls_blocks)
    | Kpush_retaddr lab ->
        let d = state.camldepth in
        let sexpl =
          push_const 1l
          @ pop_to_stack state (-d-1)
          @ push_local "env"
          @ pop_to_stack state (-d-2)
          @ push_const 1l
          @ pop_to_stack state (-d-1) in
        let state =
          { state with
            camlstack = RealStack(-d-3) ::
                          RealStack(-d-2) ::
                            RealStack(-d-1) ::
                              state.camlstack;
            camldepth = state.camldepth + 3
          } in
        (state, sexpl)
    | Kapply num ->
        (* Careful: for num < 4 this includes PUSH_RETADDR and the pushes
           of the args, but for higher num these pushes are not done.
           RETURN will pop the args and the return address triple.
         *)
        if num < 4 then
          apply123 gpad lpad state num
        else
          apply4plus gpad lpad state num
    | Kappterm(num, slots) ->
        appterm gpad lpad state num slots
    | Kreturn slots ->
        (* TODO: if accu does not contain an RValue we can return directly *)
        lpad.need_return <- true;
        let state, sexpl_str = straighten_accu gpad lpad state in
        (state, sexpl_str @ [ L [ K "br"; ID "return" ] ])
    | Krestart -> assert false
    | Kgrab num ->
        grab gpad lpad state num
    | Kclosure(lab, num) ->
        closure gpad lpad state lab num
    | Kclosurerec(funcs, num_args) ->
        closurerec gpad lpad state funcs num_args
    | Koffsetclosure index ->
        let state, sexpl_flush = flush_accu gpad lpad state in
        let sexpl_offset =
          [ L [ K "local.get"; ID "env" ];
            L [ K "i32.const"; N (I32 (Int32.of_int (index * 4))) ];
            L [ K "i32.add" ];
            L [ K "local.set"; ID "accu" ]
          ] in
        let state = { state with accu = RealAccu } in
        (state, sexpl_flush @ sexpl_offset)
    | Kpushtrap lab ->
        assert false
    | Kpoptrap ->
        (* this is a nop - all work done by emit_trap below *)
        (state, [])
    | Kraise kind ->
        (* set Caml_state->exn_bucket = accu *)
        (* wasicaml_throw() *)
        let sexpl =
          push_as gpad lpad state state.accu RValue
          (* @ debug2 30 2 *)
          @ [ L [ K "global.get"; ID "wasicaml_domain_state" ];
              L [ K "i32.load";
                  K (sprintf "offset=0x%lx" (Int32.of_int (8 * domain_field_exn_bucket)));
                  K "align=2"
                ];
              L [ K "call"; ID "wasicaml_throw" ];
              L [ K "unreachable" ]
            ] in
        (state, sexpl)
    | Kcheck_signals ->
        (state, [])   (* TODO *)
    | Kgetmethod ->
        (state, [])   (* TODO *)
    | Kgetpubmet k ->
        let state = push_camlstack state.accu state in
        (state, [])   (* TODO *)
    | Kgetdynmet ->
        (state, [])   (* TODO *)
    | Kevent _ ->
        (state, [])
    | Kstop ->
        (* return NULL pointer *)
        let sexpl =
          [ L [ K "i32.const"; N (I32 0l) ];
            (* L ( K "block" :: debug2 1 2); *)
            L [ K "return" ]
          ] in
        (state, sexpl)
    | Kstrictbranchif _ -> assert false
    | Kstrictbranchifnot _ -> assert false

let emit_trap gpad lpad state trylabel catchlabel =
  (* push 4 values onto stack: 0, 0, env, extra_args *)
  (* get function pointer and codeptr *)
  (* caught = wasicaml_try4(f, env, extra_args, codeptr, fp - depth) *)
  (* if (caught): accu = Caml_state->exn_bucket; jump lab *)
  (* else: accu = global "exn_result" *)
  (* at end of try function: global "exn_result" = accu *)
  let local = new_local lpad TI32 in
  let state, sexpl_str = straighten_all gpad lpad state in
  let sexpl_stack =
    push_const 0l
    @ pop_to_stack state (-state.camldepth-1)
    @ push_const 0l
    @ pop_to_stack state (-state.camldepth-2)
    @ push_local "env"
    @ pop_to_stack state (-state.camldepth-3)
    @ push_local "extra_args"
    @ pop_to_stack state (-state.camldepth-4) in
  let sexpl_try =
    push_wasmptr gpad trylabel
    @ push_local "env"
    @ push_local "extra_args"
    @ push_codeptr gpad trylabel
    @ push_local "fp"
    @ [ L [ K "i32.const"; N (I32 (Int32.of_int (4 * (state.camldepth + 4))))];
        L [ K "i32.sub" ]
      ]
    (* @ debug2 30 0 *)
    @ [ L [ K "call"; ID "wasicaml_try4" ];
        L [ K "local.set"; ID local ];
      ]
    (* @ debug2 30 1 *)
    @ push_stack state (-state.camldepth-3)
    @ pop_to_local "env"
    @ [ L [ K "local.get"; ID local ];
        L [ K "if";
            L [ K "then";
                L [ K "global.get"; ID "wasicaml_domain_state" ];
                L [ K "i32.load";
                    K (sprintf "offset=0x%lx" (Int32.of_int (8 * domain_field_exn_bucket)));
                    K "align=2"
                  ];
                L [ K "local.set"; ID "accu" ];
                L [ K "br"; ID (sprintf "label%d" catchlabel) ]
              ]
          ];
        L [ K "global.get"; ID "exn_result" ];
        L [ K "local.set"; ID "accu" ]
      ] in
  let state = { state with accu = RealAccu } in
  (state, sexpl_str @ sexpl_stack @ sexpl_try)

let emit_try_return gpad lpad state =
  (* return from a "try" function *)
  let sexpl =
    push_as gpad lpad state state.accu RValue
    @ [ L [ K "global.set"; ID "exn_result" ];
        L [ K "i32.const"; N (I32 0l) ];
        (* L ( K "block" :: debug2 1 3); *)
        L [ K "return" ]
      ] in
  (state, sexpl)

let emit_fblock fpad fblock =
  let maxdepth = Wc_tracestack.max_stack_depth_of_fblock fblock in
  fpad.maxdepth <- maxdepth;
  let instrs =
    Wc_unstack.transl_fblock fpad.lpad fblock
    |> emit_instrs fpad in
  [ L [ K "local.get"; ID "fp" ];
    L [ K "i32.const"; N (I32 (Int32.of_int (4 * maxdepth))) ];
    L [ K "i32.sub" ];
    L [ K "local.set"; ID "bp" ];
  ]
  @ instrs

let get_funcmapping scode =
  let open Wc_control in
  let funcmapping = Hashtbl.create 7 in
  let subfunction_num = Hashtbl.create 7 in
  let subfunctions = Hashtbl.create 7 in
  IMap.iter
    (fun func_label fblock ->
      let letrec_label =
        match fblock.scope.cfg_letrec_label with
          | None -> 0
          | Some label -> label in
      let subfunc_num =
        try Hashtbl.find subfunction_num letrec_label
        with Not_found -> 0 in
      Hashtbl.replace subfunction_num letrec_label (subfunc_num+1);
      Hashtbl.add funcmapping func_label (letrec_label, subfunc_num);
      let subfunc_list =
        try Hashtbl.find subfunctions letrec_label
        with Not_found -> [] in
      Hashtbl.replace subfunctions letrec_label (func_label :: subfunc_list)
    )
    scode.functions;
  let subfunctions_rev = Hashtbl.create 7 in
  Hashtbl.iter
    (fun letrec_label subfunc_labels ->
      Hashtbl.add subfunctions_rev letrec_label (List.rev subfunc_labels)
    )
    subfunctions;
  ( funcmapping,
    subfunctions_rev
  )

let block_cascade start_sexpl label_sexpl_pairs =
  let rec shift prev_sexpl pairs =
    match pairs with
      | (label, lsexpl) :: pairs' ->
          (prev_sexpl, Some label) :: shift lsexpl pairs'
      | [] ->
          [ prev_sexpl, None ] in
  let rec arrange inner_sexpl shifted =
    match shifted with
      | (sexpl, label_opt) :: shifted' ->
          let body =
            inner_sexpl @ sexpl @ [ L [ K "unreachable" ]] in
          let inner_sexpl' =
            match label_opt with
              | None -> body
              | Some lab ->
                  [ L ( [ K "block";
                          ID lab;
                          BR;
                        ] @ body
                      )
                  ] in
          arrange inner_sexpl' shifted'
      | [] ->
          inner_sexpl in
  arrange [] (shift start_sexpl label_sexpl_pairs)

let cond_section cond label sexpl_section sexpl_users =
  if cond then
    [ L ( [ K "block"; ID label; BR ] @ sexpl_users ) ] @ sexpl_section
  else
    sexpl_users

let cond_loop cond label sexpl =
  if cond then
    [ L ( [ K "loop"; ID label; BR ] @ sexpl ) ]
  else
    sexpl


let generate_function scode gpad letrec_label func_name subfunc_labels export_flag =
  let lpad = empty_lpad() in
  Hashtbl.add lpad.locals "accu" TI32;
  Hashtbl.add lpad.locals "bp" TI32;
  Hashtbl.add lpad.locals "h.i32" TI32;
  Hashtbl.add lpad.locals "h.f64" TF64;

  let subfunc_pairs =
    List.map
      (fun func_label ->
        let fblock = IMap.find func_label Wc_control.(scode.functions) in
        let label = sprintf "func%d" func_label in
        let sexpl = emit_fblock gpad lpad fblock in
        (label, sexpl)
      )
      subfunc_labels in
  let subfunc_pairs_with_panic =
    subfunc_pairs @ [ "panic", [] ] in

  let subfunc_sexpl =
    match subfunc_pairs with
      | [] ->
          assert false
      | [ label, sexpl ] ->
          cond_section lpad.need_panic "panic" [ L [ K "unreachable" ]] sexpl
      | _ ->
          let labels =
            List.map (fun (label, _) -> ID label) subfunc_pairs_with_panic in
          let body =
            [ L [ K "local.get"; ID "codeptr" ];
              L [ K "i32.const"; N (I32 code_pointer_subfunc_mask) ];
              L [ K "i32.and" ];
              L [ K "i32.const"; N (I32 1l) ];
              L [ K "i32.shr_u" ];
              L ( [ K "br_table" ] @ labels )
            ] in
          block_cascade body subfunc_pairs_with_panic in
  let sexpl =
    [ L [ K "i32.const"; N (I32 1l) ];
      L [ K "local.set"; ID "accu" ]
    ]
    @ (subfunc_sexpl
       |> cond_section
            lpad.need_appterm_common "appterm_common" appterm_common
       |> cond_loop
            lpad.need_appterm_common "startover"
       |> cond_section
            lpad.need_return "return" return
      ) in

  if lpad.need_appterm_common then (
    Hashtbl.add lpad.locals "appterm_depth" TI32;
    Hashtbl.add lpad.locals "appterm_old_num_args" TI32;
    Hashtbl.add lpad.locals "appterm_new_num_args" TI32;
    Hashtbl.add lpad.locals "appterm_codeptr" TI32;
  );

  let locals =
    Hashtbl.fold (fun name vtype acc -> (name,vtype) :: acc) lpad.locals [] in

  let letrec =
    [ L ( [ K "func";
            ID func_name;
          ]
          @ (if export_flag then [ L [ K "export"; S func_name ]] else [])
          @ [ L [ K "param"; ID "env"; K "i32" ];
              L [ K "param"; ID "extra_args"; K "i32" ];
              L [ K "param"; ID "codeptr"; K "i32" ];
              L [ K "param"; ID "fp"; K "i32" ];
              BR;
              L [ K "result"; K "i32" ];
            ]
          @ (List.map
               (fun (name,vtype) ->
                 L [ K "local";
                     ID name;
                     K (string_of_vtype vtype)
                   ];
               )
               locals
            )
          (* @ debug2 0 letrec_label
             @ debug2_var 10 "fp"
             @ debug2_var 11 "env"
             @ debug2_var 12 "codeptr"
             @ debug2_var 13 "extra_args"
           *)
          @ sexpl
          @ [ L [ K "unreachable" ]]
        )
    ] in
  letrec

let generate_letrec scode gpad letrec_label =
  let subfunc_labels =
    try Hashtbl.find gpad.subfunctions letrec_label
    with Not_found -> assert false in
  assert(subfunc_labels <> []);
  let func_name = Hashtbl.find gpad.letrec_name letrec_label in
  generate_function scode gpad letrec_label func_name subfunc_labels false

let generate_main scode gpad =
  let letrec_label = 0 in
  let subfunc_labels =
    try Hashtbl.find gpad.subfunctions letrec_label
    with Not_found -> assert false in
  assert(subfunc_labels <> []);
  let func_name = "mainfunc" in
  generate_function scode gpad letrec_label func_name subfunc_labels true


let globals =
  [ "wasicaml_global_data", true, TI32;
    "wasicaml_domain_state", true, TI32;
    "wasicaml_atom_table", true, TI32;
    "exn_result", true, TI32;
  ]
  @ if !enable_multireturn then [] else
      [ "retval2", true, TI32;
        "retval3", true, TI32;
      ]

let imp_functions =
  [ "ocaml", "caml_alloc_small_dispatch",
    [ L [ K "param"; K "i32" ];
      L [ K "param"; K "i32" ];
      L [ K "param"; K "i32" ];
      L [ K "param"; K "i32" ];
    ];
    "ocaml", "caml_alloc_small",
    [ L [ K "param"; K "i32" ];
      L [ K "param"; K "i32" ];
      L [ K "result"; K "i32" ]
    ];
    "ocaml", "caml_alloc_shr",
    [ L [ K "param"; K "i32" ];
      L [ K "param"; K "i32" ];
      L [ K "result"; K "i32" ]
    ];
    "ocaml", "caml_initialize",
    [ L [ K "param"; K "i32" ];
      L [ K "param"; K "i32" ];
    ];
    "ocaml", "caml_modify",
    [ L [ K "param"; K "i32" ];
      L [ K "param"; K "i32" ];
    ];
    "wasicaml", "wasicaml_try4",
    [ L [ K "param"; K "i32" ];
      L [ K "param"; K "i32" ];
      L [ K "param"; K "i32" ];
      L [ K "param"; K "i32" ];
      L [ K "param"; K "i32" ];
      L [ K "result"; K "i32" ]
    ];
    "wasicaml", "wasicaml_throw",
    [];
    "wasicaml", "wasicaml_get_global_data",
    [ L [ K "result"; K "i32" ]];
    "wasicaml", "wasicaml_get_domain_state",
    [ L [ K "result"; K "i32" ]];
    "wasicaml", "wasicaml_get_atom_table",
    [ L [ K "result"; K "i32" ]];
    "wasicaml", "debug2",
    [ L [ K "param"; K "i32" ];
      L [ K "param"; K "i32" ];
    ]
  ]

let sanitize =
  String.map
    (fun c ->
      match c with
        | 'A' .. 'Z' | 'a' .. 'z' | '0' .. '9' | '_' | '.' -> c
        | _ -> '?'
    )

let generate scode exe get_defname =
  let (funcmapping, subfunctions) = get_funcmapping scode in

  let letrec_name = Hashtbl.create 7 in
  Hashtbl.iter
    (fun letrec_label _ ->
      let suffix =
        try
          let defname = get_defname letrec_label in
          "_" ^ sanitize defname
        with
          | Not_found -> "" in
      Hashtbl.add letrec_name letrec_label (sprintf "letrec%d%s" letrec_label suffix)
    )
    subfunctions;

  let wasmindex = Hashtbl.create 7 in
  (* Need 'elements' this only for PIC: *)
  let nextindex = ref 0 in
  let _elements =
    Hashtbl.fold
      (fun letrec_label _ acc ->
        Hashtbl.add wasmindex letrec_label !nextindex;
        incr nextindex;
        let name =
          if letrec_label = 0 then "mainfunc" else
            Hashtbl.find letrec_name letrec_label in
        (ID name) :: acc
      )
      subfunctions
      [] in
  let data = Marshal.to_string Wc_reader.(exe.data) [] in

  let gpad =
    { funcmapping;
      subfunctions;
      primitives = Hashtbl.create 7;
      wasmindex;
      letrec_name;
    } in

  let sexpl_code =
    Hashtbl.fold
      (fun letrec_label _ acc ->
        if letrec_label > 0 then
          let sexpl = generate_letrec scode gpad letrec_label in
          sexpl @ acc
        else
          acc
      )
      gpad.subfunctions
      []
  @ generate_main scode gpad in

  let sexpl_memory =
    [ L [ K "import";
          S "env";
          S "memory";
          L [ K "memory";
              ID "memory";
              N (I32 65536l);
            ]
        ]
    ] in

  let sexpl_table =
    [ L [ K "import";
          S "env";
          S "table";
          L [ K "table";
              (* ID "table"; *)
              N (I32 (Int32.of_int (Hashtbl.length subfunctions)));
              K "funcref"
            ]
        ]
    ] in
  let sexpl_functions =
    List.map
      (fun (modname, name, typeuse) ->
        L [ K "import";
            S modname;
            S name;
            L ( [ K "func";
                  ID name;
                ] @ typeuse
              )
          ]
      )
      imp_functions
    @ Hashtbl.fold
        (fun name typeuse acc ->
          ( L [ K "import";
                S "ocaml";
                S name;
                L ( [ K "func";
                      ID name;
                    ] @ typeuse
                  )
              ]
          ) :: acc
        )
        gpad.primitives
        []  in

  let sexpl_globals =
    List.map
      (fun (name, mut, vtype) ->
        L ( [ K "global";
              ID name;
              ( if mut then
                  L [ K "mut"; K (string_of_vtype vtype) ]
                else
                  K (string_of_vtype vtype)
              );
            ]
            @ zero_expr_of_vtype vtype
          )
      )
      globals in

  sexpl_memory
  @ sexpl_functions
  @ sexpl_table
  @ sexpl_globals
  @ wasicaml_init
  @ wasicaml_get_data
  @ wasicaml_get_data_size (String.length data)
  @ alloc_decr
  @ alloc_slow
  @ grab_helper gpad
  @ restart_helper gpad
  @ reset_frame
  @ sexpl_code
(* Only PIC:
  @ [ L ( [ K "elem";
            L [ K "global.get"; ID "__table_base" ];
            K "func";
          ]
          @ elements
        )
    ]
 *)
  @ [ L [ K "data";
          L [ K "memory"; N (I32 0l) ];
          (* PIC: L [ K "offset"; K "global.get"; ID "__memory_base" ]; *)
          (* WAT extension: *)
          ID "data";
          S data
        ]
    ]

  (*
Notes:
~/.wasicaml/bin/wasi_ld --relocatable -o caml.wasm lib/initruntime.o lib/prims.o ~/.wasicaml/lib/ocaml/libcamlrun.a
~/.wasicaml/bin/wasi_ld -o final.wasm caml.wasm src/wasicaml/t.wasm

wat syntax: https://webassembly.github.io/spec/core/text/index.html

deficiencies of wat2wasm:
https://github.com/WebAssembly/wabt/issues/1199
https://github.com/WebAssembly/wabt/issues/1658

Static linking: https://github.com/WebAssembly/tool-conventions/blob/master/Linking.md
some llvm toolchain tricks: https://surma.dev/things/c-to-webassembly/
wasm-ld: https://lld.llvm.org/WebAssembly.html

LLVM MC: https://blog.llvm.org/2010/04/intro-to-llvm-mc-project.html
LLVM wasm assembly parser: https://llvm.org/doxygen/WebAssemblyAsmParser_8cpp_source.html
LLVM wasm assembly tests: https://github.com/llvm/llvm-project/tree/main/llvm/test/MC/WebAssembly

   *)
