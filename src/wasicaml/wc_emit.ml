open Printf
open Wc_types
open Wc_number
open Wc_sexp
open Wc_instruct

(* TODO:
   - do a stack check at the beginning of a function (but no realloc)
   - use helper functions for Kapply?
   - check that subfunc fits into codeptr (10 bits)
   - helper functions for "makeblock"
   - env: avoid to pass it around. Instead of env we can also can use
     envptr (a pointer to a value on the stack). This way it is automatically
     protected at GC time. When calling a new OCaml function, put the NEW
     env on the stack instead of the old, and let envptr point to it.
     appterm and restart might be a bit tricky.
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

let store_offset addr offset code_value =
  if offset >= 0 then
    [ L [ K "local.get"; ID addr ] ]
    @ code_value
    @ [ L [ K "i32.store";
            K (sprintf "offset=0x%lx" (Int32.of_int offset));
            K "align=2";
          ]
      ]
  else
    [ L [ K "local.get"; ID addr ];
      L [ K "i32.const"; N (I32 (Int32.of_int (-offset))) ];
      L [ K "i32.sub" ]
    ]
    @ code_value
    @ [ L [ K "i32.store"; K "align=2" ] ]

let pop_to_field var_base field code_value =
  store_offset var_base (4*field) code_value

let pop_to_double_field var_base field code_value =
  [ L [ K "local.get"; ID var_base ];
    L [ K "i32.const";
        N (I32 (Int32.of_int (4 * double_size * field)));
      ];
    L [ K "i32.add" ]
  ]
  @ code_value
  @ [ L [ K "f64.store"; K "align=2" ]]

let pop_to_domain_field field code_value =
  [ L [ K "global.get"; ID "wasicaml_domain_state" ]]
  @ code_value
  @ [ L [ K "i32.store";
          K (sprintf "offset=0x%lx" (Int32.of_int (8 * field)));
          K "align=2";
        ];
    ]

let pop_to_stack fpad pos code_value =
  if pos >= 0 then
    pop_to_field "fp" pos code_value
  else
    pop_to_field "bp" (pos + fpad.maxdepth) code_value

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
          pop_to_stack fpad pos (push_const 1l)
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
      push_local "accu" |> pop_to_stack fpad (-descr.stack_depth-1)
    else
      [] in
  let sexpl_env =
    push_local "env" |> pop_to_stack fpad (-descr.stack_depth-sp_decr) in
  let sexpl_extern_sp =
    [ L [ K "local.get";
          ID "fp";
        ];
      L [ K "i32.const";
          N (I32 (Int32.of_int ( 4 * (descr.stack_depth + sp_decr))));
        ];
      L [ K "i32.sub" ];
    ]
    |> pop_to_domain_field domain_field_extern_sp in
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
          ];
          if !enable_multireturn then [
              L [ K "result"; C "out_accu"; K "i32" ];
              L [ K "result"; C "out_env"; K "i32" ];
            ] else [];
          ( [ L [ K "local.get"; ID "fp" ];
              L [ K "local.get"; ID "stackdepth" ];
              L [ K "i32.sub" ];
              L [ K "i32.const"; N (I32 8l) ];
              L [ K "i32.sub" ];

             (* L [ K "local.tee"; ID "h.i32" ];
                L ( K "block" :: debug2_var 20 "h.i32");
              *)
            ]
            |> pop_to_domain_field domain_field_extern_sp
          )
          @ (push_local "accu" |> pop_to_field "fp" 4)
          @ (push_local "env" |> pop_to_field "fp" 0);

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

          (push_local "env" |> pop_to_field "accu" 2);

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

          (push_local "codeptr" |> pop_to_field "accu" 0);
          (push_const 5l |> pop_to_field "accu" 1);

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
        |> pop_to_stack fpad pos
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
        |>  pop_to_stack fpad pos
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

let emit_unaryeffect fpad op src1 =
  match op with
    | Poffsetref offset ->
        let local = new_local fpad RInt in
        push_as fpad src1 RIntVal
        @ [ L [ K "local.tee"; ID local ];
            L [ K "local.get"; ID local ];
            L [ K "i32.load"; K "align=2" ];
            L [ K "i32.const";
                N (I32 (Int32.shift_left (Int32.of_int offset) 1));
              ];
            L [ K "i32.add" ];
            L [ K "i32.store";  K "align=2" ];
          ]
    | Psetglobal (Global index) ->
        [ L [ K "global.get";
              ID "wasicaml_global_data";
            ];
          L [ K "i32.load" ];
          L [ K "i32.const";
              N (I32 (Int32.of_int (4 * index)));
            ];
          L [ K "i32.add" ]
        ]
        @ push_as fpad src1 RValue
        @ [ L [ K "call";
                ID "caml_modify";
              ]
          ]

let emit_int_binary fpad src1 src2 dest instrs_int =
  push_as fpad src1 RInt
  @ push_as fpad src2 RInt
  @ instrs_int
  @ pop_to fpad dest RInt None

let emit_intval_binary fpad src1 src2 dest instrs_int instrs_intval =
  if repr_of_store src1 = RInt && repr_of_store src2 = RInt then
    push_as fpad src1 RInt
    @ push_as fpad src2 RInt
    @ instrs_int
    @ tovalue fpad RInt
    @ pop_to fpad dest RIntVal None
  else
    push_as fpad src1 RIntVal
    @ push_as fpad src2 RIntVal
    @ instrs_intval
    @ pop_to fpad dest RIntVal None

let emit_binary fpad op src1 src2 dest =
  match op with
    | Paddint ->
        emit_intval_binary
          fpad src1 src2 dest
          [ L [ K "i32.add" ] ]
          [ L [ K "i32.add" ];
            L [ K "i32.const"; N(I32 1l) ];
            L [ K "i32.sub" ]
          ]
    | Psubint ->
        emit_intval_binary
          fpad src1 src2 dest
          [ L [ K "i32.sub" ] ]
          [ L [ K "i32.sub" ];
            L [ K "i32.const"; N(I32 1l) ];
            L [ K "i32.add" ]
          ]
    | Pmulint ->
        emit_int_binary
          fpad src1 src2 dest
          [ L [ K "i32.mul" ] ]
    | Pdivint ->
        (* TODO: div by zero *)
        emit_int_binary
          fpad src1 src2 dest
          [ L [ K "i32.div_s" ] ]
    | Pmodint ->
        (* TODO: div by zero *)
        (* CHECK: is this the right remainder function? Needs to be like % in C *)
        emit_int_binary
          fpad src1 src2 dest
          [ L [ K "i32.mod_s" ] ]
    | Pandint ->
        emit_intval_binary
          fpad src1 src2 dest
          [ L [ K "i32.and" ] ]
          [ L [ K "i32.and" ] ]
    | Porint ->
        emit_intval_binary
          fpad src1 src2 dest
          [ L [ K "i32.or" ] ]
          [ L [ K "i32.or" ] ]
    | Pxorint ->
        emit_intval_binary
          fpad src1 src2 dest
          [ L [ K "i32.xor" ] ]
          [ L [ K "i32.xor" ];
            L [ K "i32.const"; N (I32 1l) ];
            L [ K "i32.or" ];
          ]
    | Plslint ->
        emit_int_binary
          fpad src1 src2 dest
          [ L [ K "i32.shl" ] ]
    | Plsrint ->
        emit_int_binary
          fpad src1 src2 dest
          [ L [ K "i32.shr_u" ] ]
    | Pasrint ->
        emit_int_binary
          fpad src1 src2 dest
          [ L [ K "i32.shr_s" ] ]
    | (Pintcomp cmp | Puintcomp cmp) ->
        let wasm_op, force_int =
          match op with
            | Pintcomp Ceq | Puintcomp Ceq -> "i32.eq", false
            | Pintcomp Cne | Puintcomp Cne -> "i32.ne", false
            | Pintcomp Clt -> "i32.lt_s", true
            | Pintcomp Cle -> "i32.le_s", true
            | Pintcomp Cgt -> "i32.gt_s", true
            | Pintcomp Cge -> "i32.ge_s", true
            | Puintcomp Clt -> "i32.lt_u", true
            | Puintcomp Cle -> "i32.le_u", true
            | Puintcomp Cgt -> "i32.gt_u", true
            | Puintcomp Cge -> "i32.ge_u", true
            | _ -> assert false in
        if force_int || repr_of_store src1 <> repr_of_store src2 then
          emit_int_binary
            fpad src1 src2 dest
            [ L [ K wasm_op ]]
        else (* repr doesn't matter *)
          push fpad src1
          @ push fpad src2
          @ [ L [ K wasm_op ]]
          @ pop_to fpad dest RInt None
    | Pgetvectitem ->
        push_as fpad src1 RValue
        @ ( match src2, repr_of_store src2 with
              | Const c, _ ->
                  [ L [ K "i32.load";
                        K (sprintf "offset=0x%lx" (Int32.shift_left (Int32.of_int c) 2));
                        K "align=2"
                      ]
                  ]
              | _, RInt ->
                  push_as fpad src2 RInt
                  @ [ L [ K "i32.const"; N (I32 2l) ];
                      L [ K "i32.shl" ];
                      L [ K "i32.add" ];
                      L [ K "i32.load"; K "align=2" ]
                    ]
              | _ ->
                  push_as fpad src2 RIntVal
                  @ [ L [ K "i32.const"; N (I32 1l) ];
                      L [ K "i32.shl" ];
                      L [ K "i32.const"; N (I32 0xffff_fffcl) ];
                      L [ K "i32.and" ];
                      L [ K "i32.add" ];
                      L [ K "i32.load"; K "align=2" ]
                    ]
          )
        @ pop_to fpad dest RValue None
    | Pgetstringchar | Pgetbyteschar ->
        push_as fpad src1 RValue
        @ ( match src2 with
              | Const c ->
                  [ L [ K "i32.load8_u";
                        K "align=2";
                        K (sprintf "offset=0x%x" c)
                      ]
                  ]
              | _ ->
                  push_as fpad src2 RInt
                  @ [ L [ K "i32.add" ];
                      L [ K "i32.load8_u"; K "align=2" ]
                    ]
          )
        @ pop_to fpad dest RInt None
    | Pgetmethod ->
        assert false (* TODO *)
    | Pgetdynmet ->
        assert false (* TODO *)

let emit_binaryeffect fpad op src1 src2 =
  match op with
    | Psetfield field ->
        push_as fpad src1 RValue
        @ ( if field <> 0 then
              [ L [ K "i32.const";
                    N (I32 (Int32.of_int (4 * field)));
                  ];
                L [ K "i32.add" ]
              ]
            else
              []
          )
        @ push_as fpad src2 RValue
        @ [ L [ K "call"; ID "caml_modify" ]]
    | Psetfloatfield field ->
        push_as fpad src1 RValue
        @ push_as fpad src2 RFloat
        @ [ L [ K "f64.store";
                K (sprintf "offset=0x%lx" (Int32.of_int (8 * field)));
                K "align=2";
              ]
          ]

let emit_ternaryeffect fpad op src1 src2 src3 =
  match op with
    | Psetvectitem ->
        push_as fpad src1 RValue
        @ ( match repr_of_store src2 with
              | RInt ->
                  push_as fpad src2 RInt
                  @ [ L [ K "i32.const"; N (I32 2l) ];
                      L [ K "i32.shl" ];
                      L [ K "i32.add" ];
                    ]
              | _ ->
                  push_as fpad src2 RIntVal
                  @ [ L [ K "i32.const"; N (I32 1l) ];
                      L [ K "i32.shl" ];
                      L [ K "i32.const"; N (I32 0xffff_fffcl) ];
                      L [ K "i32.and" ];
                      L [ K "i32.add" ];
                    ]
          )
        @ push_as fpad src3 RValue
        @ [ L [ K "caml_modify" ]]
    | Psetbyteschar ->
        push_as fpad src1 RValue
        @ ( match src2 with
              | Const c ->
                  push_as fpad src3 RInt
                  @ [ L [ K "i32.store8";
                          K (sprintf "offset=0x%x" c)
                        ]
                    ]
              | _ ->
                  push_as fpad src2 RInt
                  @ [ L [ K "i32.add" ] ]
                  @ push_as fpad src3 RInt
                  @ [ L [ K "i32.store8" ]]
          )

type mb_elem =
  | MB_store of store
  | MB_code of sexp list

let push_mb_elem fpad =
  function
  | MB_store src ->
      push_as fpad src RValue
  | MB_code code ->
      code

let makeblock fpad descr src_list tag =
  let size = List.length src_list in
  let sexpl_alloc, ptr, young = alloc_set fpad descr size tag in
  let sexpl_init =
    if young then
      List.mapi
        (fun field src ->
          push_mb_elem fpad src
          |>  pop_to_field ptr field
        )
        src_list
    else
      List.mapi
        (fun field src ->
          push_field_addr ptr field
          @ push_mb_elem fpad src
          @ [ L [ K "call"; ID "caml_initialize" ]]
        )
        src_list in
  (ptr, sexpl_alloc @ List.flatten sexpl_init)

let makefloatblock fpad descr src_list =
  let size = List.length src_list in
  let wosize = size * double_size in
  let sexpl_alloc, ptr, _ = alloc_set fpad descr wosize double_array_tag in
  let sexpl_init =
    List.mapi
      (fun field src ->
        ( push_as fpad src RValue
          @ load_double
        ) |> pop_to_double_field ptr field
      )
      src_list in
  (ptr, sexpl_alloc @ List.flatten sexpl_init)


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
  
let closurerec gpad fpad descr src_list dest_list =
  let nfuncs = List.length dest_list in
  let envofs = nfuncs * 3 - 1 in
  let mb_src_list =
    ( List.mapi
        (fun i (_, label) ->
          ( if i > 0 then
              [ MB_store (Const (make_header (3*i) infix_tag)) ]
            else
              []
          )
          @ [ MB_code (push_codeptr gpad label) ]
          @ [ MB_store (Const ((envofs lsl 1) + 1)) ]
        )
        dest_list
      |> List.flatten
    ) @ List.map (fun src -> MB_store src) src_list in
  let ptr, sexpl_mb = makeblock fpad descr mb_src_list closure_tag in
  let sexpl_dest =
    List.mapi
      (fun i (dest, _) ->
        [ L [ K "local.get"; ID ptr ] ]
        @ (if i > 0 then
             [ L [ K "i32.const"; N (I32 (Int32.of_int (3*i-1))) ];
               L [ K "i32.add" ]
             ]
           else
             []
          )
        @ pop_to fpad dest RValue None  (* descr is not up to date *)
      )
      dest_list
    |> List.flatten in
  sexpl_mb @ sexpl_dest

let c_call gpad fpad descr src_list name =
  let descr = { descr with stack_save_accu = false } in (* it is overwritten *)
  let sexpl_setup = setup_for_gc fpad descr in
  let sexpl_restore = restore_after_gc fpad descr in
  let sexpl_args =
    List.map
      (fun src -> push_as fpad src RValue)
      src_list in
  let sexpl_call =
    [ L [ K "call"; ID name ]]
    @ pop_to_local "accu" in
  let p_i32 = L [ K "param"; K "i32" ] in
  let r_i32 = L [ K "result"; K "i32" ] in
  let ty = (src_list |> List.map (fun _ -> p_i32)) @ [ r_i32 ] in
  Hashtbl.replace gpad.primitives name ty;
  sexpl_setup @ List.flatten sexpl_args @ sexpl_call @ sexpl_restore

let c_call_vector gpad fpad descr numargs depth name =
  let descr = { descr with stack_save_accu = false } in (* it is overwritten *)
  let sexpl_setup = setup_for_gc fpad descr in
  let sexpl_restore = restore_after_gc fpad descr in
  let sexpl_call =
    push_field_addr "fp" (-depth)
    @ push_const (Int32.of_int numargs)
    @ [ L [ K "call"; ID name ]]
    @ pop_to_local "accu" in
  let ty =
    [ L [ K "param"; K "i32" ];
      L [ K "param"; K "i32" ];
      L [ K "result"; K "i32" ] ] in
  Hashtbl.replace gpad.primitives name ty;
  sexpl_setup @ sexpl_call @ sexpl_restore

let string_label =
  function
  | Label k -> sprintf "label%d" k
  | Loop k -> sprintf "loop%d" k

let switch fpad src labls_ints labls_blocks =
  fpad.need_panic <- true;
  let value = new_local fpad RValue in
  push_as fpad src RValue
  @ pop_to_local value
  @ [ (* if (!Is_block(value)) *)
      L [ K "local.get"; ID value ];
      L [ K "i32.const"; N (I32 1l) ];
      L [ K "i32.and" ];
      L [ K "if";
          L [ K "then";

              L [ K "local.get"; ID value ];
              L [ K "i32.const"; N (I32 1l) ];
              L [ K "i32.shr_s" ];

              L ( [  K "br_table" ]
                  @ ( Array.map
                        (fun lab -> ID (string_label lab))
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
                        (fun lab -> ID (string_label lab))
                        labls_blocks
                      |>  Array.to_list
                    )
                  @ [ ID "panic" ]
                );
            ];
        ]
    ]

let grab num =
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
  sexpl

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

let apply fpad numargs depth =
  let env_pos = (-depth + numargs + 1) in
  let codeptr = new_local fpad RValue in
  [ L [ K "local.get"; ID "accu" ];
    L [ K "i32.const"; N (I32 (Int32.of_int (numargs-1))) ];
  ]
  @ push_field "accu" 0
  @ [ L [ K "local.tee"; ID codeptr ];

      (*
      L [ K "i32.const"; N (I32 3l) ];
      L [ K "local.get"; ID codeptr ];
      L [ K "call"; ID "debug2" ];
       *)

      L [ K "local.get"; ID "fp" ];
      L [ K "i32.const"; N (I32 (Int32.of_int (4 * depth))) ];
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
  @ push_stack fpad env_pos   (* env might have been moved by GC *)
  @ pop_to_local "env"

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


let appterm fpad numargs oldnumargs depth =
  let sexpl =
    push_const (Int32.of_int depth)
    @ pop_to_local "appterm_depth"
    @ push_const (Int32.of_int oldnumargs)
    @ pop_to_local "appterm_old_num_args"
    @ push_const (Int32.of_int numargs)
    @ pop_to_local "appterm_new_num_args"
    @ [ L [ K "br"; ID "appterm_common" ] ] in
  fpad.need_appterm_common <- true;
  fpad.need_return <- true;
  sexpl

let trap gpad fpad trylabel catchlabel depth =
  (* push 4 values onto stack: 0, 0, env, extra_args *)
  (* get function pointer and codeptr *)
  (* caught = wasicaml_try4(f, env, extra_args, codeptr, fp - depth) *)
  (* if (caught): accu = Caml_state->exn_bucket; jump lab *)
  (* else: accu = global "exn_result" *)
  (* at end of try function: global "exn_result" = accu *)
  let local = new_local fpad RValue in
  let sexpl_stack =
    (push_const 0l |> pop_to_stack fpad (-depth-1))
    @ (push_const 0l |> pop_to_stack fpad (-depth-2))
    @ (push_local "env" |> pop_to_stack fpad (-depth-3))
    @ (push_local "extra_args" |> pop_to_stack fpad (-depth-4)) in
  let sexpl_try =
    push_wasmptr gpad trylabel
    @ push_local "env"
    @ push_local "extra_args"
    @ push_codeptr gpad trylabel
    @ push_local "fp"
    @ [ L [ K "i32.const"; N (I32 (Int32.of_int (4 * (depth + 4))))];
        L [ K "i32.sub" ]
      ]
    (* @ debug2 30 0 *)
    @ [ L [ K "call"; ID "wasicaml_try4" ];
        L [ K "local.set"; ID local ];
      ]
    (* @ debug2 30 1 *)
    @ push_stack fpad (-depth-3)
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
  sexpl_stack @ sexpl_try

let rec emit_instr gpad fpad instr =
  match instr with
    | Wcomment s ->
        []
    | Wblock arg ->
        ( match arg.label with
            | Label lab ->
                [ L ( [ K "block";
                        ID (sprintf "label%d" lab);
                        BR
                      ]
                      @ emit_instrs gpad fpad arg.body
                    )
                ]
            | Loop lab ->
                [ L ( [ K "loop";
                        ID (sprintf "loop%d" lab);
                        BR
                      ]
                      @ emit_instrs gpad fpad arg.body
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
        emit_unary fpad arg.op arg.src1 arg.dest
    | Wunaryeffect arg ->
        emit_unaryeffect fpad arg.op arg.src1
    | Wbinary arg ->
        emit_binary fpad arg.op arg.src1 arg.src2 arg.dest
    | Wbinaryeffect arg ->
        emit_binaryeffect fpad arg.op arg.src1 arg.src2
    | Wternaryeffect arg ->
        emit_ternaryeffect fpad arg.op arg.src1 arg.src2 arg.src3
    | Wmakeblock arg ->
        let src = List.map (fun s -> MB_store s) arg.src in
        let ptr, sexpl = makeblock fpad arg.descr src arg.tag in
        sexpl
        @ push_local ptr
        @ pop_to_local "accu"
    | Wmakefloatblock arg ->
        let ptr, sexpl = makefloatblock fpad arg.descr arg.src in
        sexpl
        @ push_local ptr
        @ pop_to_local "accu"
    | Wccall arg ->
        (* TODO: maintain a list of noalloc primitives *)
        (* TODO: maintain a list of unit primitives (set accu=Const 0 instead),
           or more generally, primitives that never return functions
         *)
        c_call gpad fpad arg.descr arg.src arg.name
    | Wccall_vector arg ->
        c_call_vector gpad fpad arg.descr arg.numargs arg.depth arg.name
    | Wbranch arg ->
        [ L [ K "br"; ID (string_label arg.label) ]]
    | Wbranchif arg ->
        ( match repr_of_store arg.src with
            | RInt ->
                push_as fpad arg.src RInt
            | _ ->
                push_as fpad arg.src RIntVal
                @ [ L [ K "i32.const"; N (I32 1l) ];
                    L [ K "i32.gt_u" ]
                  ]
        )
        @ [ L [ K "br_if"; ID (string_label arg.label) ]]
    | Wbranchifnot arg ->
        ( match repr_of_store arg.src with
            | RInt ->
                push_as fpad arg.src RInt
                @ [ L [ K "i32.eqz" ]]
            | _ ->
                push_as fpad arg.src RIntVal
                @ [ L [ K "i32.const"; N (I32 1l) ];
                    L [ K "i32.le_u" ]
                  ]
        )
        @ [ L [ K "br_if"; ID (string_label arg.label) ]]
    | Wswitch arg ->
        switch fpad arg.src arg.labels_int arg.labels_blk
    | Wapply arg ->
        apply fpad arg.numargs arg.depth
    | Wappterm arg ->
        appterm fpad arg.numargs arg.oldnumargs arg.depth
    | Wreturn arg ->
        ( match repr_of_store arg.src with
            | RValue ->
                (* return value could be another closure *)
                fpad.need_return <- true;
                push_as fpad arg.src RValue
                @ [ L [ K "br"; ID "return" ]]
            | _ ->
                push_as fpad arg.src RValue
                @ [ L [ K "return" ]]
        )
    | Wgrab arg ->
        grab arg.numargs
    | Wclosurerec arg ->
        closurerec gpad fpad arg.descr arg.src arg.dest
    | Wraise arg ->
        push_as fpad arg.src RValue
        @ [ L [ K "global.get"; ID "wasicaml_domain_state" ];
            L [ K "i32.load";
                K (sprintf "offset=0x%lx" (Int32.of_int (8 * domain_field_exn_bucket)));
                K "align=2"
              ];
            L [ K "call"; ID "wasicaml_throw" ];
            L [ K "unreachable" ]
          ]
    | Wtrap arg ->
        trap gpad fpad arg.trylabel arg.catchlabel arg.depth
    | Wtryreturn arg ->
        push_as fpad arg.src RValue
        @ [ L [ K "global.set"; ID "exn_result" ];
            L [ K "i32.const"; N (I32 0l) ];
            (* L ( K "block" :: debug2 1 3); *)
            L [ K "return" ]
          ]
    | Wstop ->
        [ L [ K "i32.const"; N (I32 0l) ];
          (* L ( K "block" :: debug2 1 2); *)
          L [ K "return" ]
        ]

and emit_instrs gpad fpad instrs =
  List.fold_left
    (fun acc instr ->
      let comment = string_of_winstruction instr in
      List.rev_append
        (emit_instr gpad fpad instr)
        (List.rev_append [C comment] acc)
    )
    []
    instrs
  |> List.rev

let emit_fblock gpad fpad fblock =
  let maxdepth = Wc_tracestack.max_stack_depth_of_fblock fblock + 2 in
  (* make maxdepth a bit larger than stricly necessary to completely avoid
     bp[k] with k<0 *)
  fpad.maxdepth <- maxdepth;
  let instrs =
    Wc_unstack.transl_fblock fpad.lpad fblock
    |> emit_instrs gpad fpad in
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
  let fpad = empty_fpad() in
  Hashtbl.add fpad.lpad.locals "accu" RValue;
  Hashtbl.add fpad.lpad.locals "bp" RValue;

  let subfunc_pairs =
    List.map
      (fun func_label ->
        let fblock = IMap.find func_label Wc_control.(scode.functions) in
        let label = sprintf "func%d" func_label in
        let sexpl = emit_fblock gpad fpad fblock in
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
          cond_section fpad.need_panic "panic" [ L [ K "unreachable" ]] sexpl
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
            fpad.need_appterm_common "appterm_common" appterm_common
       |> cond_loop
            fpad.need_appterm_common "startover"
       |> cond_section
            fpad.need_return "return" return
      ) in

  if fpad.need_appterm_common then (
    Hashtbl.add fpad.lpad.locals "appterm_depth" RInt;
    Hashtbl.add fpad.lpad.locals "appterm_old_num_args" RInt;
    Hashtbl.add fpad.lpad.locals "appterm_new_num_args" RInt;
    Hashtbl.add fpad.lpad.locals "appterm_codeptr" RInt;
  );

  let locals =
    Hashtbl.fold (fun name vtype acc -> (name,vtype) :: acc) fpad.lpad.locals [] in

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
               (fun (name,repr) ->
                 L [ K "local";
                     ID name;
                     K (string_of_vtype (vtype repr))
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
