(* Copyright (C) 2021 by Figly, Inc.
   This code is free software, see the file LICENSE for details.
 *)

open Printf
open Bigarray
open Wc_types
open Wc_number
open Wc_sexp
open Wc_instruct

(* OCaml functions are translated to Wasm functions with parameters:
   param 1: envptr
   param 2: extra_args
   param 3: code pointer (overriding the one in the closure)
   param 4: fp

   So far, the function args are passed via the bytecode stack.

   The envptr is a pointer to the stack location of the environment (env).
   This pointer is created when a function is called - the bytecode
   interpreter reserves 3 stack locations for PC, saved env, and extra_args.
   We also reserve stack locations upon function call but differently:
    - need only one location and not three
    - the one location stores the env of the called function, not the
      env of the calling function

   There are also two ways of invoking functions. If there are up to 3
   args, the bytecode only includes the instructions to push the args
   onto the stack, followed by Kapply. It is then the task of Kapply to
   make room for the saved values (as explained). If, however, there are
   more than 3 args, the bytecode includes a Kpush_retaddr instruction
   which reserves 3 positions on the stack in advance. Although we only need
   one of these, there is no fixup that would eliminate the other two.

   In order to indicate an exception to the caller, the function can
   simply return NULL. The exception must be put in domain_field_exn_bucket.
 *)

(* Exception handling: We provide two exception mechanisms:

   1. An OCaml function indicates an exception by returning NULL,
      and putting the exception value into domain_field_exn_bucket.
   2. A C function indicates an exception by calling wasicaml_throw,
      which raises a Wasm or host exception, and this exception is
      caught by one of the wasicaml_wraptry functions. ("wrap"-try
      because these functions wrap some native mechanism.)

   Now, we could call every C function via wasicaml_wraptry. However,
   this turns out as very slow. Instead, we use a more sophisticated
   scheme:

    - It is also allowed that an OCaml function throws a native exception
      (in addition to the other mechanism of returning NULL)
    - If a C or OCaml function is called from inside a "try" block,
      the call is wrapped using wasicaml_wraptry, and any exception
      is caught.
    - If, however, a C or OCaml function is called from outside a "try"
      block, the call is not wrapped, and the native exception - should it
      occur - would fall through to the caller.

   Essentially, exceptions from function inside a "try" block are always
   caught, and it is then jumped to the exception handler. Outside such
   a block nothing special is done, and exceptions fall through to the
   caller - this is the fast path.

   Note some details of the mechanism:

   - We need to save and restore the pointer to local roots when an
     exception is caught. This is simply because C functions do not
     remove local roots before they throw an exception, and the restoration
     needs to be done by the caller. - This type of save&restore is
     done by the generated code, here in this module.
   - There is also a (hidden) pointer to the top of the C shadow stack.
     This pointer also needs to be restored when an exception is caught.
     We force the C compiler to generate such code by defining a local
     variable in the wasicaml_wraptry functions. This variable is also
     declared as "volatile" so that it cannot be optimized away, even
     if it is not used for anything.
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

type letrec_label =
  | Func of int
  | Main of int

type gpad =  (* global pad *)
  { letrec_name : (letrec_label, string) Hashtbl.t;
    (* maps letrec label to name *)
    primitives : (string, sexp list) Hashtbl.t;
    (* maps name to type *)
    funcmapping : (int, letrec_label * int) Hashtbl.t;
    (* maps function label to (letrec_label, subfunction_id) *)
    subfunctions : (letrec_label, int list) Hashtbl.t;
    (* maps letrec_label to list of subfunction labels *)
    wasmindex : (letrec_label, int) Hashtbl.t;
    (* maps letrec label to the (relative) index in the table of functions *)
    mutable need_reinit_frame : bool;
    mutable need_reinit_frame_k : ISet.t;
    mutable need_mlookup : bool;
    mutable globals_table : (int, Wc_traceglobals.initvalue) Hashtbl.t;
    (* knowledge about the globals *)
    mutable glbfun_table : (int,  int * Wc_traceglobals.initvalue array) Hashtbl.t;
    (* for each letrec function: knowledge about its environment *)
    funcprops_table : (int, Wc_tracefuncs.funcprops) Hashtbl.t;
    (* knowledge about functions *)
  }

type fpad =  (* function pad *)
  { mutable lpad : Wc_unstack.lpad;
    fpad_letrec_label : letrec_label;
    mutable fpad_scope : Wc_control.cfg_scope;
    mutable maxdepth : int;
    mutable have_bp : bool;
    mutable need_appterm_common : bool;
    mutable need_startover : bool;   (* back to subfunction selection *)
    mutable need_selfrecurse : bool; (* self-recursion - only when tailcalls are enabled! *)
    mutable need_return : bool;
    mutable need_panic : bool;
    mutable need_tmp1_i32 : bool;
    mutable need_tmp2_i32 : bool;
    mutable need_tmp1_f64 : bool;
    mutable need_xalloc : bool;
    mutable need_lroots : bool;
    mutable disallow_grab : bool;   (* Wgrab is only allowed at the beginning *)
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
(* NB. This seems to be broken in llvm-11 *)

let enable_returncall = ref false
(* whether Wasm code can use the return_call instruction (tail calls) *)

let enable_exceptions = ref false
(* whether we can use try/catch instructions *)

let quick_exceptions = ref true
(* allow to indicate an exception by returning NULL *)

let enable_deadbeef_check = ref false
(* debugging: check that there is no uninitialized memory on the stack *)

let local_limit = ref 5
(* try not to put more than this number of values into local variables *)


let code_pointer_shift = 12
  (* OCaml code pointers:
      - Bit 0: always 1
      - Bit 1: whether to run RESTART
      - Bit 2 - code_pointer_shift-1: subfunction of the letrec
      - Bit code_pointer_shift-31: the Wasm function index

     See also ocaml/runtime/callback.c
   *)
let code_pointer_subfunc_mask = 0xffcl
let code_pointer_letrec_mask = 0xffff_f000l
let code_pointer_restart_mask = 2l

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
let domain_field_local_roots = 36

let global_field_zero_divide = 5       (* ZERO_DIVIDE_EXN *)
let global_field_stack_overflow = 8    (* STACK_OVERFLOW_EXN *)

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
    | RValue | RInt | RIntUnclean | RIntVal | RNatInt | RInt32 ->
        TI32
    | RInt64 ->
        TI64
    | RFloat ->
        TF64

let empty_scope =
  let open Wc_control in
  { cfg_letrec_label = None;
    cfg_func_label = 0;
    cfg_try_labels = [];
    (* cfg_is_pop_label = false; *)
    cfg_main = false
  }

let empty_fpad letrec_label =
  let numeric_label =
    match letrec_label with
      | Func k -> k
      | Main k -> k in
  { lpad = Wc_unstack.empty_lpad
             ~enable_returncall:!enable_returncall
             ~local_limit:!local_limit
             empty_scope numeric_label;
    fpad_letrec_label = letrec_label;
    fpad_scope = empty_scope;
    maxdepth = 0;
    have_bp = false;
    need_appterm_common = false;
    need_startover = false;
    need_selfrecurse = false;
    need_return = false;
    need_panic = false;
    need_tmp1_i32 = false;
    need_tmp2_i32 = false;
    need_tmp1_f64 = false;
    need_xalloc = false;
    need_lroots = false;
    disallow_grab = false;
  }

let returncall name =
  if !enable_returncall then
    [ L [ K "return_call"; ID name ] ]
  else
    [ L [ K "call"; ID name ];
      L [ K "return" ]
    ]

let new_local fpad repr =
  Wc_unstack.new_local fpad.lpad repr

let req_tmp1_i32 fpad =
  if fpad.lpad.local_limit = 0 then (
    fpad.need_tmp1_i32 <- true;
    "tmp1_i32"
  )
  else new_local fpad RInt

let req_tmp2_i32 fpad =
  if fpad.lpad.local_limit = 0 then (
    fpad.need_tmp2_i32 <- true;
    "tmp2_i32"
  )
  else new_local fpad RInt

let req_tmp1_f64 fpad =
  if fpad.lpad.local_limit = 0 then (
    fpad.need_tmp1_f64 <- true;
    "tmp1_f64"
  )
  else new_local fpad RFloat

let push_const n =
  [ L [ K "i32.const"; N (I32 n) ]]

let push_local var =
  [ L [ K "local.get"; ID var ]]

let pop_to_local var =
  [ L [ K "local.set"; ID var ]]

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

let add_offset offset =
  if offset <> 0 then
    [ L [ K "i32.const"; N (I32 (Int32.of_int offset)) ];
      L [ K "i32.add" ]
    ]
  else
    []

let push_env =
  [ L [ K "local.get"; ID "envptr" ];
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
  @ if field > 0 then
      [ L [ K "i32.const";
            N (I32 (Int32.of_int (4 * field)));
          ];
        L [ K "i32.add" ]
      ]
    else if field < 0 then
      [ L [ K "i32.const";
            N (I32 (Int32.of_int (-4 * field)));
          ];
        L [ K "i32.sub" ]
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
  if pos >= 0 || not fpad.have_bp then
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
  if pos >= 0 || not fpad.have_bp then
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

let debugcnt = ref 0

let debugmark() =
  let n = !debugcnt in
  incr debugcnt;
  debug2 100 n

let deadbeef_init =
  (* IBM mainframes used to initialize fresh memory with 0xdeadbeef *)
  [ L ( [ [ K "func";
            ID "deadbeef_init";
            L [ K "param"; ID "bp"; K "i32" ];
            L [ K "param"; ID "fp"; K "i32" ];
          ];

          (* loop *)
          [ L [ K "block"; ID "loop_exit"; BR;
                L [ K "loop"; ID "loop"; BR;

                    (* if (bp >= fp) break *)
                    L [ K "local.get"; ID "bp" ];
                    L [ K "local.get"; ID "fp" ];
                    L [ K "i32.ge_u" ];
                    L [ K "br_if"; ID "loop_exit" ];

                    (* *bp = 0xdeadbeef *)
                    L [ K "local.get"; ID "bp" ];
                    L [ K "i32.const"; N (I32 0xdeadbeefl) ];
                    L [ K "i32.store" ];

                    (* bp++ *)
                    L [ K "local.get"; ID "bp" ];
                    L [ K "i32.const"; N (I32 4l) ];
                    L [ K "i32.add" ];
                    L [ K "local.set"; ID "bp" ];

                    L [ K "br"; ID "loop" ]
                  ]
              ]
          ]
        ] |> List.flatten
      )
  ]

let deadbeef_check =
  (* now check that there's no dead beef on the stack! *)
  [ L ( [ [ K "func";
            ID "deadbeef_check";
            L [ K "param"; ID "ptr"; K "i32" ];
            L [ K "param"; ID "fp"; K "i32" ];
          ];

          (* assert (ptr <= fp) *)
          push_local "ptr";
          push_local "fp";
          [ L [ K "i32.gt_u" ];
            L [ K "if";
                L [ K "then";
                    L [ K "unreachable" ]
                  ]
              ]
          ];

          (* loop *)
          [ L [ K "block"; ID "loop_exit"; BR;
                L [ K "loop"; ID "loop"; BR;

                    (* if (ptr >= fp) break *)
                    L [ K "local.get"; ID "ptr" ];
                    L [ K "local.get"; ID "fp" ];
                    L [ K "i32.ge_u" ];
                    L [ K "br_if"; ID "loop_exit" ];

                    (* assert( *bp != 0xdeadbeef) *)
                    L [ K "local.get"; ID "ptr" ];
                    L [ K "i32.load" ];
                    L [ K "i32.const"; N (I32 0xdeadbeefl) ];
                    L [ K "i32.eq" ];
                    L [ K "if";
                        L [ K "then";
                            L [ K "unreachable" ]
                          ]
                      ];

                    (* ptr++ *)
                    L [ K "local.get"; ID "ptr" ];
                    L [ K "i32.const"; N (I32 4l) ];
                    L [ K "i32.add" ];
                    L [ K "local.set"; ID "ptr" ];

                    L [ K "br"; ID "loop" ]
                  ]
              ]
          ]
        ] |> List.flatten
      )
  ]

let push_global offset =
  [ L [ K "global.get";
        ID "wasicaml_global_data";
      ];
    L [ K "i32.load" ];
    L [ K "i32.load";
        K (sprintf "offset=0x%lx" (Int32.of_int (4 * offset)));
        K "align=2";
      ]
  ]

let follow_path path =
  List.map
    (fun field ->
      L [ K "i32.load";
          K (sprintf "offset=0x%lx" (Int32.of_int (4 * field)));
          K "align=2";
        ]
    )
    path

let throw fpad =
  (* the exception value must be in domain_field_exn_bucket *)
  match fpad.lpad.scope.cfg_try_labels with
    | { catchlabel } :: _ ->
        (* the exception is caught by the surrounding block *)
        (* it is expected that the accu contains the exception value *)
        push_domain_field domain_field_exn_bucket
        @ pop_to_local "accu"
        @ [ L [ K "br"; ID (sprintf "label%d" catchlabel) ] ]
    | [] ->
        if (fpad.lpad.scope.cfg_main && fpad.lpad.scope.cfg_try_labels = []) ||
             not !quick_exceptions
        then
          (* in outermost scope there is no caller - prefer to throw
             a real exception
           *)
          if !enable_exceptions then
            [ L [ K "throw"; ID "ocaml_exception" ]]
          else
            [ L [ K "call"; ID "wasicaml_wrapthrow" ];
              L [ K "unreachable" ]
            ]
        else
          (* a normal function, but outside a "try" block *)
          [ L [ K "i32.const"; N (I32 0l) ];
            L [ K "return" ]
          ]

let raise_predef_exn fpad which drops =
  (* which: e.g. "div_by_zero", "stack_overflow" *)
  ( Wc_util.enum 0 drops |> List.map (fun _ -> L [ K "drop" ]) )
  @ ( match fpad.lpad.scope.cfg_try_labels with
        | { catchlabel } :: _ ->
            (* the next handler is in a surrounding block *)
            [ L [ K "call"; ID ("pick_" ^ which) ];
              L [ K "local.set"; ID "accu" ];
              L [ K "br"; ID (sprintf "label%d" catchlabel) ]
            ]
        | [] ->
            if (fpad.lpad.scope.cfg_main && fpad.lpad.scope.cfg_try_labels = []) ||
                 not !quick_exceptions
            then
              [ L [ K "call"; ID ("pick_" ^ which) ];
                L [ K "drop" ]
              ]
              @ (if !enable_exceptions then
                   [ L [ K "throw"; ID "ocaml_exception" ]]
                 else
                   [ L [ K "call"; ID "wasicaml_wrapthrow" ];
                     L [ K "unreachable" ]
                   ]
                )
            else
              if !enable_returncall then
                [ L [ K "return_call"; ID ("ret_" ^ which) ] ]
              else
                [ L [ K "call"; ID ("ret_" ^ which) ];
                  L [ K "return" ];
                  L [ K "unreachable" ]   (* because of a bug in LLVM assembler *)
               ]
    )

let raise_div_by_zero fpad drops =
  raise_predef_exn fpad "div_by_zero" drops

let raise_stack_overflow fpad drops =
  raise_predef_exn fpad "stack_overflow" drops

let pick_predef_exn name which =
  [ L ( [ [ K "func";
            ID ("pick_" ^ name);
            BR;
            L [ K "result"; C "exn"; K "i32" ];
            L [ K "local"; ID "exn"; K "i32" ];
          ];
          push_global which;
          pop_to_local "exn";
          push_local "exn" |>
            pop_to_domain_field domain_field_exn_bucket;
          push_local "exn";
          [ L [ K "return" ] ]
        ] |> List.flatten
      )
  ]

let ret_predef_exn name =
  [ L ( [ [ K "func";
            ID ("ret_" ^ name);
            BR;
            L [ K "result"; C "exn"; K "i32" ];
            BR;
            L [ K "call"; ID ("pick_" ^ name) ];
            L [ K "drop" ];
            L [ K "i32.const"; N (I32 0l) ];
            L [ K "return" ];
          ]
        ] |> List.flatten
      )
  ]

let pick_div_by_zero =
  pick_predef_exn "div_by_zero" global_field_zero_divide

let ret_div_by_zero =
  ret_predef_exn "div_by_zero"
  
let pick_stack_overflow =
  pick_predef_exn "stack_overflow" global_field_stack_overflow

let ret_stack_overflow =
  ret_predef_exn "stack_overflow"

let locals_to_save descr =
  ISet.elements descr.stack_save_locals
  |> List.map string_of_localPos
  |> (fun l -> if descr.stack_save_accu then "accu" :: l else l)

let stack_init_and_save_locals fpad descr =
  let locals = ref (locals_to_save descr) in
  let instrs1 =
    List.map
      (fun pos ->
        match !locals with
          | local :: rest ->
              locals := rest;
              push_local local |> pop_to_stack fpad pos
          | [] ->
              push_const 1l |> pop_to_stack fpad pos
      )
      descr.stack_uninit
    |> List.flatten in
  let n = ref 0 in
  let instrs2 =
    List.map
      (fun local ->
        incr n;
        push_local local |> pop_to_stack fpad (-descr.stack_depth - !n)
      )
      !locals
    |> List.flatten in
  (instrs1 @ instrs2, !n)

let stack_restore_locals fpad descr =
  let locals = ref (locals_to_save descr) in
  let instrs1 =
    List.map
      (fun pos ->
        match !locals with
          | local :: rest ->
              locals := rest;
              push_stack fpad pos @ pop_to_local local
          | [] ->
              []
      )
      descr.stack_uninit
    |> List.flatten in
  let n = ref 0 in
  let instrs2 =
    List.map
      (fun local ->
        incr n;
        push_stack fpad (-descr.stack_depth - !n) @ pop_to_local local
      )
      !locals
    |> List.flatten in
  (instrs1 @ instrs2, !n)

let setup_for_gc fpad descr =
  let sexpl_stack, sp_decr =
    stack_init_and_save_locals fpad descr in
  let sexpl_extern_sp =
    ( [ L [ K "local.get";
            ID "fp";
          ]
      ]
      @ (if descr.stack_depth + sp_decr > 0 then
           [ L [ K "i32.const";
                 N (I32 (Int32.of_int ( 4 * (descr.stack_depth + sp_decr))));
               ];
             L [ K "i32.sub" ];
           ]
         else
           []
        )
    )
    |> pop_to_domain_field domain_field_extern_sp in
  let sexpl_check =
    ( if !enable_deadbeef_check then
        push_domain_field domain_field_extern_sp
        @ [ L [ K "local.get"; ID "fp" ];
            L [ K "call"; ID "deadbeef_check" ]
          ]
      else
        []
    ) in
  sexpl_stack @ sexpl_extern_sp @ sexpl_check

let restore_after_gc fpad descr =
  let sexpl_stack, _ =
    stack_restore_locals fpad descr in
  sexpl_stack

let alloc_atom fpad tag =
  [ L [ K "global.get";
        ID "wasicaml_atom_table";
      ];
    L [ K "i32.const";
        N (I32 (Int32.of_int (4 * tag)));
      ];
    L [ K "i32.add" ];
  ]

let alloc_fast =
  [ L ( [ [ K "func";
            ID "alloc_fast";
            L [ K "param"; ID "bhsize"; K "i32" ];
            L [ K "param"; ID "header"; K "i32" ];
            BR;
            L [ K "result"; C "ptr"; K "i32" ];
            L [ K "local"; ID "ptr"; K "i32" ];
          ];

          (* ptr = Caml_state_field(young_ptr) - Whsize_wosize (wosize) *)
          push_domain_field domain_field_young_ptr;
          push_local "bhsize";
          [ L [ K "i32.sub" ];
            L [ K "local.tee"; ID "ptr" ];
          ];

          (* Caml_state_field(young_ptr) = ptr *)
          ( push_local "ptr"
            |> pop_to_domain_field domain_field_young_ptr
          );

          (* if (ptr < Caml_state_field(young_limit)) return 0 *)
          push_domain_field domain_field_young_limit;
          [ L [ K "i32.lt_u" ] ];
          [ L [ K "if";
                L [ K "then";
                    L [ K "i32.const"; N (I32 0l) ];
                    L [ K "return" ]
                  ]
              ]
          ];

          (* *ptr = header *)
          ( push_local "header"
            |> pop_to_field "ptr" 0
          );

          (* return ptr+1 *)
          push_local "ptr";
          push_const 4l;
          [ L [ K "i32.add" ];
            L [ K "return" ]
          ]
        ] |> List.flatten
      )
  ]

let wrap_alloc_small_dispatch() =
  [ L [ K "func";
        ID "wrap_alloc_small_dispatch";
        L [ K "param"; ID "wosize"; K "i32" ];
        L [ K "param"; ID "flags"; K "i32" ];
        L [ K "param"; ID "nallocs"; K "i32" ];
        L [ K "param"; ID "encoded_alloc_lens"; K "i32" ];
        BR;
        L [ K "result"; C "dummy"; K "i32" ];

        L [ K "local.get"; ID "wosize" ];
        L [ K "local.get"; ID "flags" ];
        L [ K "local.get"; ID "nallocs" ];
        L [ K "local.get"; ID "encoded_alloc_lens" ];
        L [ K "call"; ID "caml_alloc_small_dispatch" ];

        L [ K "i32.const"; N (I32 1l) ];  (* = success code *)
        L [ K "return" ];
      ];
  ]

let alloc_slow() =
  [ L ( [ [ K "func";
            ID "alloc_slow";
            L [ K "param"; ID "wosize"; K "i32" ];
            L [ K "param"; ID "header"; K "i32" ];
            L [ K "param"; ID "fp"; K "i32" ];
            L [ K "param"; ID "stackdepth"; K "i32" ];
            BR;
            L [ K "result"; C "ptr"; K "i32" ];
            L [ K "local"; ID "lroots"; K "i32" ];
          ];
          [ L [ K "local"; ID "ptr"; K "i32" ];
            L [ K "local"; ID "sp"; K "i32" ]
          ];

          ( [ L [ K "local.get"; ID "fp" ];
              L [ K "local.get"; ID "stackdepth" ];
              L [ K "i32.sub" ];
              L [ K "local.tee"; ID "sp" ];
            ]
            |> pop_to_domain_field domain_field_extern_sp
          );

          ( if !enable_deadbeef_check then
              push_domain_field domain_field_extern_sp
              @ [ L [ K "local.get"; ID "fp" ];
                  L [ K "call"; ID "deadbeef_check" ]
                ]
            else
              []
          );

          (* caml_alloc_small_dispatch(wosize, CAML_FROM_C, 1, NULL) *)
          push_domain_field domain_field_local_roots;
          pop_to_local "lroots";
          [ L [ K "i32.const"; ID "wrap_alloc_small_dispatch" ]];
          push_local "wosize";
          push_const (Int32.of_int caml_from_c);
          push_const 1l;
          push_const 0l;
          [ L [ K "call"; ID "wasicaml_wraptry4" ] ];
          push_local "lroots"
          |> pop_to_domain_field domain_field_local_roots;
          [ L [ K "i32.eqz" ];
            L [ K "if";
                L ( [ K "then";
                      L [ K "i32.const"; N (I32 0l) ];
                      L [ K "return" ]
                    ]
                  )
              ]
          ];

          push_domain_field domain_field_young_ptr;
          pop_to_local "ptr";

          (* *ptr = header *)
          ( push_local "header"
            |> pop_to_field "ptr" 0
          );

          (* return ptr+1 *)
          push_local "ptr";
          push_const 4l;
          [ L [ K "i32.add" ] ];

          [ L [ K "return" ]]

        ] |> List.flatten
      )
  ]

let call_alloc_slow() =
  [ L [ K "call"; ID "alloc_slow" ]]

let alloc_non_atom fpad descr size tag =
  fpad.need_xalloc <- true;
  let ptr = "xalloc" (* new_local fpad RValue *) in
  let young = size <= max_young_wosize in
  let code =
    if young then
      let instrs_save, extra = stack_init_and_save_locals fpad descr in
      let instrs_restore, _ = stack_restore_locals fpad descr in
      [ push_const (Int32.of_int (4 * (size+1)));
        push_const (Int32.of_int (make_header size tag));
        [ L [ K "call"; ID "alloc_fast" ];
          L [ K "local.tee"; ID ptr ];
        ];

        (* if (ptr == NULL) *)
        [ L [ K "i32.eqz" ]];
        [ L [ K "if";
              L ( [ [
                      K "then";
                    ];
                    instrs_save;
                    push_const (Int32.of_int size);
                    push_const (Int32.of_int (make_header size tag));
                    push_local "fp";
                    push_const (Int32.of_int (4 * (descr.stack_depth + extra)));
                    call_alloc_slow();
                    pop_to_local ptr;
                    instrs_restore;
                    [ L [ K "local.get"; ID ptr ];
                      L [ K "i32.eqz" ];
                      L [ K "if";
                          L ([K "then" ] @ throw fpad)
                        ]
                    ]
                  ] |> List.flatten
                )
            ];
        ];
      ] |> List.flatten
    else (
      fpad.need_lroots <- true;
      push_domain_field domain_field_local_roots
      @ pop_to_local "lroots"
      @ setup_for_gc fpad descr
      @ [ L [ K "i32.const"; ID "caml_alloc_shr" ];
          L [ K "i32.const"; N (I32 (Int32.of_int size)) ];
          L [ K "i32.const"; N (I32 (Int32.of_int tag)) ];
          L [ K "call"; ID "wasicaml_wraptry2" ];
        ]
      @ restore_after_gc fpad descr
      @ ( push_local "lroots"
          |> pop_to_domain_field domain_field_local_roots
        )
      @ [ L [ K "local.tee"; ID ptr ];
          L [ K "i32.eqz" ];
          L [ K "if";
              L ([K "then"] @ throw fpad)
            ]
        ]
    ) in
  (code, ptr, young)

let alloc fpad descr size tag =
  if size = 0 then
    alloc_atom fpad tag
  else
    let (code, ptr, _) = alloc_non_atom fpad descr size tag in
    code @ push_local ptr

let alloc_set fpad descr size tag =
  if size = 0 then (
    fpad.need_xalloc <- true;
    let ptr = "xalloc" (* new_local fpad RValue *) in
    let young = false in
    (alloc_atom fpad tag @ push_local ptr, ptr, young)
  ) else
    alloc_non_atom fpad descr size tag

let grab_helper gpad =
  (* generates a helper function:
     $grab_helper(extra_args, codeptr, fp)

     NB. we never catch exceptions from alloc_small - these always fall
     through to the caller.
   *)
  let fpad = empty_fpad (Main 0) in
  let descr = empty_descr in
  assert(descr.stack_save_accu = false);

  [ L ( [ [ K "func";
            ID "grab_helper";
            L [ K "param"; ID "envptr"; K "i32" ];
            L [ K "param"; ID "extra_args"; K "i32" ];
            L [ K "param"; ID "codeptr"; K "i32" ];
            L [ K "param"; ID "fp"; K "i32" ];
            BR;
            L [ K "result"; K "i32" ];
            L [ K "local"; ID "accu"; K "i32" ];
            L [ K "local"; ID "i"; K "i32" ];
            L [ K "local"; ID "lroots"; K "i32" ];
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
          [ L [ K "local.get"; ID "accu" ];
            L [ K "i32.eqz" ];
            L [ K "if";
                L [ K "then";
                    L [ K "i32.const"; N (I32 0l) ];
                    L [ K "return" ]
                  ]
              ]
          ];
          (push_env |> pop_to_field "accu" 2);

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

          ( (push_local "codeptr" @
               [ L [ K "i32.const"; N (I32 code_pointer_restart_mask) ];
                   (* restart flag *)
                 L [ K "i32.or" ]
               ]
            )
            |> pop_to_field "accu" 0
          );
          (push_const 5l |> pop_to_field "accu" 1);

          push_local "accu";
          [  L [ K "return" ] ]

        ] |> List.flatten
      )
  ]

let restart_helper gpad =
  [ L ( [ [ K "func";
            ID "restart_helper";
            L [ K "param"; ID "envptr"; K "i32" ];
            L [ K "param"; ID "extra_args"; K "i32" ];
            L [ K "param"; ID "fp"; K "i32" ];
            BR;
            L [ K "result"; C "out_extra_args"; K "i32" ];
          ];
          if !enable_multireturn then [
            L [ K "result"; C "out_fp"; K "i32" ];
          ] else [];

          [ L [ K "local"; ID "i"; K "i32" ];
            L [ K "local"; ID "num_args"; K "i32" ];
          ];

          (* num_args = Wosize_val(env) - 3 *)
          push_env;
          [ L [ K "i32.const"; N (I32 4l) ];
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
            L ( [ [ K "loop"; ID "args"; BR;
                    (* fp[i[ = ... *)
                    L [ K "local.get"; ID "fp" ];
                    L [ K "local.get"; ID "i" ];
                    L [ K "i32.const"; N (I32 2l) ];
                    L [ K "i32.shl" ];
                    L [ K "i32.add" ]
                  ];

                  (* Field(env, i+3) *)
                  push_env;
                  [ L [ K "local.get"; ID "i" ];
                    L [ K "i32.const"; N (I32 3l) ];
                    L [ K "i32.add" ];
                    L [ K "i32.const"; N (I32 2l) ];
                    L [ K "i32.shl" ];
                    L [ K "i32.add" ];
                    L [ K "i32.load"; K "align=2" ];
                  ];

                  (* assign *)
                  [ L [ K "i32.store"; K "align=2" ] ];

                  (* i++, and jump back if i < num_args *)
                  [ L [ K "local.get"; ID "i" ];
                    L [ K "i32.const"; N (I32 1l) ];
                    L [ K "i32.add" ];
                    L [ K "local.tee"; ID "i" ];
                    L [ K "local.get"; ID "num_args" ];
                    L [ K "i32.lt_u" ];
                    L [ K "br_if"; ID "args" ];
                  ]
                ] |> List.flatten
              )
          ];

          (* env = Field(env, 2) *)
          [ L [ K "local.get"; ID "envptr" ];
            L [ K "local.get"; ID "envptr" ];
            L [ K "i32.load"; K "align=2" ];
            L [ K "i32.load"; K "offset=8"; K "align=2" ];
            L [ K "i32.store"; K "align=2" ]
          ];

          (* extra_args += num_args *)
          [ L [ K "local.get"; ID "extra_args" ];
            L [ K "local.get"; ID "num_args" ];
            L [ K "i32.add" ];
            L [ K "local.set"; ID "extra_args" ];
          ];

          (* return *)
          push_local "extra_args";
          push_local "fp";

          if !enable_multireturn then [] else
            [ L [ K "global.set"; ID "retval2" ] ];

          [ L [ K "return" ] ];
        ] |> List.flatten
      )
  ]

let call_restart_helper() =
  [ L [ K "call"; ID "restart_helper" ]]
  @ if !enable_multireturn then [] else
      [ L [ K "global.get"; ID "retval2" ]]

let reinit_frame =
  [ L [ K "func";
        ID "reinit_frame";
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

let reinit_frame_k new_num_args =
  [ L ( [ K "func";
          ID (sprintf "reinit_frame_%d" new_num_args);
          L [ K "param"; ID "fp"; K "i32" ];
          L [ K "param"; ID "depth"; K "i32" ];
          L [ K "param"; ID "old_num_args"; K "i32" ];  (* >= 1 *)
          BR;
          L [ K "result"; C "out_fp"; K "i32" ];
          L [ K "local"; ID "i"; K "i32" ];
          L [ K "local"; ID "bp"; K "i32" ];
          L [ K "local"; ID "new_fp"; K "i32" ];

          (* new_fp = fp + old_num_args - new_num_args *)
          L [ K "local.get"; ID "fp" ];
          L [ K "local.get"; ID "old_num_args" ];
          L [ K "i32.const"; N (I32 (Int32.of_int new_num_args)) ];
          L [ K "i32.sub" ];
          L [ K "i32.const"; N (I32 2l) ];
          L [ K "i32.shl" ];
          L [ K "i32.add" ];
          L [ K "local.set"; ID "new_fp" ];

          (* bp = fp - depth *)
          L [ K "local.get"; ID "fp" ];
          L [ K "local.get"; ID "depth" ];
          L [ K "i32.const"; N (I32 2l) ];
          L [ K "i32.shl" ];
          L [ K "i32.sub" ];
          L [ K "local.set"; ID "bp" ];
        ]
        @ ( Wc_util.enum 0 new_num_args
            |> List.map
                 (fun j ->
                   let i = new_num_args - 1 - j in

                   [ (* new_fp[i] = ... *)
                     L [ K "local.get"; ID "new_fp" ];

                     (* bp[i)] *)
                     L [ K "local.get"; ID "bp" ];
                     L [ K "i32.load";
                         K (sprintf "offset=0x%x" (4 * i));
                         K "align=2"
                       ];

                     (* assign *)
                     L [ K "i32.store";
                         K (sprintf "offset=0x%x" (4 * i));
                         K "align=2"
                       ];
                   ]
                 )
            |> List.flatten
          )
        @ [ L [ K "local.get"; ID "new_fp" ];
            L [ K "return" ]
          ]
      )
  ]

let return_helper() =
  [ L ( [ [ K "func";
            ID "return_helper";
            L [ K "param"; ID "envptr"; K "i32" ];
            L [ K "param"; ID "extra_args"; K "i32" ];
            L [ K "param"; ID "fp"; K "i32" ];
            L [ K "param"; ID "accu"; K "i32" ];
            BR;
            L [ K "result"; K "i32" ];
            L [ K "local"; ID "codeptr"; K "i32" ];
          ];
          ( push_local "accu"
            |> pop_to_field "envptr" 0
          );
          [ L [ K "local.get"; ID "envptr" ];
            L [ K "local.get"; ID "extra_args" ];
            L [ K "i32.const"; N (I32 1l) ];
            L [ K "i32.sub" ];
          ];
          push_field "accu" 0;
          [ L [ K "local.tee"; ID "codeptr" ] ];

          [ L [ K "local.get"; ID "fp" ];
            L [ K "local.get"; ID "codeptr" ];
            L [ K "i32.const"; N (I32 (Int32.of_int code_pointer_shift)) ];
            L [ K "i32.shr_u" ];
          ];

          if !enable_returncall then
            [ L [ K "return_call_indirect";
                  N (I32 0l);    (* table *)
                  L [ K "param"; K "i32" ];
                  L [ K "param"; K "i32" ];
                  L [ K "param"; K "i32" ];
                  L [ K "param"; K "i32" ];
                  L [ K "result"; K "i32" ];
                ]
            ]
          else
            [ L [ K "call_indirect";
                  N (I32 0l);    (* table *)
                  L [ K "param"; K "i32" ];
                  L [ K "param"; K "i32" ];
                  L [ K "param"; K "i32" ];
                  L [ K "param"; K "i32" ];
                  L [ K "result"; K "i32" ];
                ];
              L [ K "return" ]
            ]
        ] |> List.flatten
      )
  ]

let appterm_helper() =
  if !enable_returncall then
    []
  else
    [ L ( [ [ K "func";
              ID "appterm_helper";
              L [ K "param"; ID "envptr"; K "i32" ];
              L [ K "param"; ID "codeptr"; K "i32" ];
              L [ K "param"; ID "accu"; K "i32" ];
              L [ K "param"; ID "extra_args"; K "i32" ];
              L [ K "param"; ID "new_num_args"; K "i32" ];
              BR;
              L [ K "result"; C "out_codeptr"; K "i32" ];
            ];
            if !enable_multireturn then
              [L [ K "result"; C "out_extra_args"; K "i32" ]]
            else [];
            [ L [ K "local"; ID "out_codeptr"; K "i32" ]];

            push_local "extra_args";
            push_local "new_num_args";
            [ L [ K "i32.add" ]];
            pop_to_local "extra_args";

            (push_local "accu" |> pop_to_field "envptr" 0);

            push_field "accu" 0;
            pop_to_local "out_codeptr";

            [ L [ K "local.get"; ID "out_codeptr" ];
              L [ K "i32.const"; N (I32 code_pointer_letrec_mask) ];
              L [ K "i32.and" ];
              L [ K "local.get"; ID "codeptr" ];
              L [ K "i32.const"; N (I32 code_pointer_letrec_mask) ];
              L [ K "i32.and" ];
              L [ K "i32.eq" ];
              L [ K "if";
                  L ( [ K "then" ;
                        (* same letrec: we can jump! *)
                        L [ K "local.get"; ID "out_codeptr" ];
                        L [ K "local.get"; ID "extra_args" ];
                        L [ K "i32.const"; N (I32 1l) ];
                        L [ K "i32.sub" ]
                      ]
                      @ (if !enable_multireturn then [] else
                           [ L [ K "global.set"; ID "retval2" ] ]
                        )
                      @ [ L [ K "return" ] ]
                    );
                  L ( [ K "else";
                        (* different letrec: call + return. *)
                        L [ K "i32.const"; N (I32 0l) ];
                        L [ K "local.get"; ID "extra_args" ]
                      ]
                      @ (if !enable_multireturn then [] else
                           [ L [ K "global.set"; ID "retval2" ] ]
                        )
                      @ [ L [ K "return" ]]
                    )
                ];
              L [ K "unreachable" ]
            ]
          ] |> List.flatten
        )
    ]

let call_appterm_helper() =
  assert(not !enable_returncall);
  [ L [ K "call"; ID "appterm_helper" ]]
  @ if !enable_multireturn then [] else
      [ L [ K "global.get"; ID "retval2" ]]

let mlookup =
  [ L ( [ [ K "func";
            ID "mlookup";
            L [ K "param"; ID "obj"; K "i32" ];
            L [ K "param"; ID "tag"; K "i32" ];
            L [ K "param"; ID "cache"; K "i32" ];
            BR;
            L [ K "result"; C "method"; K "i32" ];
            L [ K "local"; ID "meths"; K "i32" ];
            L [ K "local"; ID "ofs"; K "i32" ];
            L [ K "local"; ID "li"; K "i32" ];
            L [ K "local"; ID "hi"; K "i32" ];
            L [ K "local"; ID "mi"; K "i32" ];
          ];

          (* get the descriptor: meths = obj[0] *)
          [ L [ K "local.get"; ID "obj" ];
            L [ K "i32.load"; K "align=2" ];
            L [ K "local.set"; ID "meths" ];
          ];

          (* test the cache first, if any: *)
          [ L [ K "local.get"; ID "cache" ];
            L [ K "if";
                L [ K "then";

                    (* ofs = *cache & meths[1] *)
                    L [ K "local.get"; ID "cache" ];
                    L [ K "i32.load"; K "align=2" ];
                    L [ K "local.get"; ID "meths" ];
                    L [ K "i32.load"; K "offset=0x4"; K "align=2" ];
                    L [ K "i32.and" ];
                    L [ K "local.tee"; ID "ofs" ];

                    (* get &meths[3] + ofs *)
                    L [ K "local.get"; ID "meths" ];
                    L [ K "i32.const"; N (I32 12l) ];
                    L [ K "i32.add" ];
                    L [ K "i32.add" ];

                    (* load the tag there, and compare with [tag] *)
                    L [ K "i32.load"; K "align=2" ];
                    L [ K "local.get"; ID "tag" ];
                    L [ K "i32.eq" ];

                    (* if equal, found something *)
                    L [ K "if";
                        L [ K "then";

                            (* get &meths[2] + ofs *)
                            L [ K "local.get"; ID "meths" ];
                            L [ K "i32.const"; N (I32 8l) ];
                            L [ K "i32.add" ];
                            L [ K "local.get"; ID "ofs" ];
                            L [ K "i32.add" ];

                            (* load it *)
                            L [ K "i32.load"; K "align=2" ];
                            L [ K "return" ];
                          ]
                      ]
                  ]
              ]
          ];

          (* li=3 *)
          [ L [ K "i32.const"; N (I32 3l) ];
            L [ K "local.set"; ID "li" ];
          ];

          (* hi=meths[0] *)
          [ L [ K "local.get"; ID "meths" ];
            L [ K "i32.load"; K "align=2" ];
            L [ K "local.set"; ID "hi" ];
          ];

          (* loop *)
          [ L [ K "block"; ID "loop_exit"; BR;
                L [ K "loop"; ID "loop"; BR;

                    (* if (li >= hi) break *)
                    L [ K "local.get"; ID "li" ];
                    L [ K "local.get"; ID "hi" ];
                    L [ K "i32.ge_u" ];
                    L [ K "br_if"; ID "loop_exit" ];

                    (* mi = (li+hi) >> 1 | 1 *)
                    L [ K "local.get"; ID "li" ];
                    L [ K "local.get"; ID "hi" ];
                    L [ K "i32.add" ];
                    L [ K "i32.const"; N (I32 1l) ];
                    L [ K "i32.shr_s" ];
                    L [ K "i32.const"; N (I32 1l) ];
                    L [ K "i32.or" ];
                    L [ K "local.set"; ID "mi" ];

                    (* if (tag < meths[mi]) *)
                    L [ K "local.get"; ID "tag" ];
                    L [ K "local.get"; ID "meths" ];
                    L [ K "local.get"; ID "mi" ];
                    L [ K "i32.const"; N (I32 2l) ];
                    L [ K "i32.shl" ];
                    L [ K "i32.add" ];
                    L [ K "i32.load"; K "align=2" ];
                    L [ K "i32.lt_s" ];
                    L [ K "if";
                        L [ K "then";
                            (* hi = mi-2 *)
                            L [ K "local.get"; ID "mi" ];
                            L [ K "i32.const"; N (I32 2l) ];
                            L [ K "i32.sub" ];
                            L [ K "local.set"; ID "hi" ];
                          ];
                        L [ K "else";
                            (* li = mi *)
                            L [ K "local.get"; ID "mi" ];
                            L [ K "local.set"; ID "li" ];
                          ]
                      ];
                    L [ K "br"; ID "loop" ];
                  ]
              ]
          ];

          (* set cache *)
          [ L [ K "local.get"; ID "cache" ];
            L [ K "if";
                L [ K "then";

                    (* *cache = (li-3) * 4 *)
                    L [ K "local.get"; ID "cache" ];
                    L [ K "local.get"; ID "li" ];
                    L [ K "i32.const"; N (I32 3l) ];
                    L [ K "i32.sub" ];
                    L [ K "i32.const"; N (I32 4l) ];
                    L [ K "i32.mul" ];
                    L [ K "i32.store"; K "align=2" ];
                  ]
              ]
          ];

          (* return meths[li-1] *)
          [ L [ K "local.get"; ID "meths" ];
            L [ K "local.get"; ID "li" ];
            L [ K "i32.const"; N (I32 1l) ];
            L [ K "i32.sub" ];
            L [ K "i32.const"; N (I32 2l) ];
            L [ K "i32.shl" ];
            L [ K "i32.add" ];
            L [ K "i32.load"; K "align=2" ];
            L [ K "return" ];
          ]
        ] |> List.flatten
      )
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
        L [ K "global.get"; ID "wasicaml_domain_state" ];
        L [ K "i32.load";
            K (sprintf "offset=0x%lx" (Int32.of_int (8 * domain_field_stack_threshold)));
            K "align=2";
          ];
        L [ K "global.set"; ID "wasicaml_stack_threshold" ];
        L [ K "return" ]
      ]
  ]


let tovalue_alloc fpad repr descr_opt =
  (* transform the value of the Wasm stack to a proper OCaml value,
     and put that back on the Wasm stack *)
  match repr with
    | RValue | RIntVal ->
        []
    | RInt | RIntUnclean ->
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
                let (instrs_alloc, ptr, _) =
                  alloc_set fpad descr double_size double_tag in
                let local = req_tmp1_f64 fpad in
                let instrs =
                  [ L [ K "local.set"; ID local ] ]
                  @ instrs_alloc
                  @ [ L [ K "local.get"; ID ptr ];
                      L [ K "local.get"; ID local ];
                      L [ K "f64.store"; K "align=2" ];
                      L [ K "local.get"; ID ptr ];
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
    | RIntUnclean ->
        [ L [ K "i32.const"; N (I32 1l) ];
          L [ K "i32.shl" ];
          L [ K "i32.const"; N (I32 1l) ];
          L [ K "i32.shr_s" ];
        ]
    | RValue | RIntVal ->
        [ L [ K "i32.const"; N (I32 1l) ];
          L [ K "i32.shr_s" ];
        ]
    | _ ->
        assert false

let tointunclean repr =
  match repr with
    | RIntUnclean ->
        []
    | _ ->
        toint repr

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
    | RIntUnclean -> tointunclean repr_from
    | RFloat -> tofloat repr_from
    | _ ->
        assert false (* TODO *)

let push fpad store =
  (* put the value in store onto the wasm stack *)
  match store with
    | RealAccu _ ->
        push_local "accu"
    | Local(repr, name) ->
        push_local name
    | Const x ->
        push_const (Int32.of_int x)
    | RealStack pos ->
        push_stack fpad pos
    | LocalPos pos ->
        push_local (Wc_instruct.string_of_localPos pos)
    | Atom tag ->
        alloc_atom fpad tag
    | TracedGlobal(Glb glb_offset, path, _) ->
        push_global glb_offset
        @ follow_path path
    | TracedGlobal(Env env_offset, path, _) ->
        push_env
        @ add_offset (4 * env_offset)
        @ follow_path path
    | Invalid ->
        assert false

let push_alloc_as fpad store req_repr descr_opt =
  match store, req_repr with
    | Const x, (RValue | RIntVal) ->
        push_const (Int32.logor (Int32.shift_left (Int32.of_int x) 1) 1l)
    | Local(RInt, name), (RValue | RIntVal) ->
        (* saves one instr *)
        push_local name
        @ push_local name
        @ [ L [ K "i32.add" ];
            L [ K "i32.const"; N (I32 1l) ];
            L [ K "i32.or" ]
          ]
    | _ ->
        let sexpl_push = push fpad store in
        let repr = repr_of_store store in
        sexpl_push @ convert fpad repr req_repr descr_opt

let push_as fpad store req_repr =
  push_alloc_as fpad store req_repr None

let pop_to fpad store repr descr_opt code_value =
  match store with
    | RealAccu _ ->
        code_value
        @ tovalue_alloc fpad repr descr_opt
        @ pop_to_local "accu"
    | Local(lrepr, name) ->
        code_value
        @ convert fpad repr lrepr descr_opt
        @ pop_to_local name
    | RealStack pos ->
        (code_value @ tovalue_alloc fpad repr descr_opt)
        |> pop_to_stack fpad pos
    | LocalPos pos ->
        (code_value @ tovalue_alloc fpad repr descr_opt)
        @ pop_to_local (Wc_instruct.string_of_localPos pos)
    | _ ->
        assert false

let copy fpad src dest descr_opt =
  match dest with
    | RealAccu _ ->
        push_alloc_as fpad src RValue descr_opt
        @ pop_to_local "accu"
    | Local(repr, name) ->
        push_as fpad src repr
        @ pop_to_local name
    | RealStack pos ->
        push_alloc_as fpad src RValue descr_opt
        |>  pop_to_stack fpad pos
    | LocalPos pos ->
        push_alloc_as fpad src RValue descr_opt
        @ pop_to_local (Wc_instruct.string_of_localPos pos)
    | _ ->
        assert false

let rec drop n l =
  if n > 0 then
    match l with
      | _ :: l -> drop (n-1) l
      | [] -> []
  else
    l

let set_bp_1 fpad =
  [ L [ K "i32.const"; N (I32 (Int32.of_int (4 * fpad.maxdepth))) ];
    L [ K "i32.sub" ];
    L [ K "local.tee"; ID "bp" ];
    L [ K "global.get"; ID "wasicaml_stack_threshold" ];
    L [ K "i32.lt_u" ];
    L [ K "if";
        L (K "then" ::
             raise_stack_overflow fpad 0
          )
      ]
  ]

let set_bp fpad =
  if fpad.maxdepth = 0 then
    []
  else
    [ L [ K "local.get"; ID "fp" ] ]
    @ set_bp_1 fpad

let pop_to_fp fpad =
  if fpad.maxdepth = 0 then
    pop_to_local "fp"
  else
    [ L [ K "local.tee"; ID "fp" ]]
    @ set_bp_1 fpad


let emit_unary gpad fpad op src1 dest =
  match op with
    | Pnegint ->
        ( push_as fpad src1 RIntVal
          @  [ L [ K "i32.const";
                   N (I32 (0xffff_fffel));
                 ];
               L [ K "i32.xor" ];
               L [ K "i32.const";
                   N (I32 2l);
                 ];
               L [ K "i32.add" ];
             ]
        ) |> pop_to fpad dest RIntVal None
    | Pboolnot ->
        ( push_as fpad src1 RIntVal
          @ [ L [ K "i32.const";
                  N (I32 2l);
                ];
              L [ K "i32.xor" ]
            ]
        ) |> pop_to fpad dest RIntVal None
    | Poffsetint offset ->
        ( push_as fpad src1 RIntVal
          @ [ L [ K "i32.const";
                  N (I32 (Int32.shift_left (Int32.of_int offset) 1));
                ];
              L [ K "i32.add" ]
            ]
        ) |> pop_to fpad dest RIntVal None
    | Pisint ->
        ( push_as fpad src1 RValue
          @ [ L [ K "i32.const"; N (I32 1l) ];
              L [ K "i32.and" ];
            ]
        ) |> pop_to fpad dest RInt None
    | Pgetfield field ->
        assert(field >= 0);
        ( push_as fpad src1 RValue
          @ [ L [ K "i32.load";
                  K (sprintf "offset=0x%lx" (Int32.of_int (4 * field)));
                  K "align=2";
                ];
            ]
        ) |> pop_to fpad dest RValue None
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
        let local = req_tmp1_i32 fpad in
        ( push_as fpad src1 RValue
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
        ) |> pop_to fpad dest RIntVal None
    | Pgetpubmet tag ->
        (* TODO: enable the cache: pass a unique pointer to a 4 byte mem block
           instead of NULL (initialized to 0) *)
        gpad.need_mlookup <- true;
        ( (* mlookup(src1, tag, NULL) *)
          push_as fpad src1 RValue
          @ push_const (Int32.succ (Int32.shift_left (Int32.of_int tag) 1))
          @ push_const 0l
          @ [ L [ K "call"; ID "mlookup" ] ]
        ) |> pop_to fpad dest RValue None

let emit_unaryeffect fpad op src1 =
  match op with
    | Poffsetref offset ->
        let local = req_tmp1_i32 fpad in
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
        (* let local = new_local fpad RValue in *)
        [ L [ K "global.get";
              ID "wasicaml_global_data";
            ];
          L [ K "i32.load" ];
          L [ K "i32.const";
              N (I32 (Int32.of_int (4 * index)));
            ];
          L [ K "i32.add" ]
        ]
        (*
        @ debug2 150 index
        @ [ L [ K "local.tee"; ID local ]]
        @ debug2_var 151 local
        @ [ L [ K "local.get"; ID local ]]
         *)

        @ push_as fpad src1 RValue
        @ [ L [ K "call";
                ID "caml_modify";
              ]
          ]

let emit_int_binary fpad src1 src2 dest instrs_repr instrs_int =
  ( push_as fpad src1 RInt
    @ push_as fpad src2 RInt
    @ instrs_int
  ) |> pop_to fpad dest instrs_repr None

let emit_int_binary_unclean_ok fpad src1 src2 dest instrs_int =
  ( push_as fpad src1 RIntUnclean
    @ push_as fpad src2 RIntUnclean
    @ instrs_int
  ) |> pop_to fpad dest RIntUnclean None

let emit_intval_binary fpad src1 src2 dest instrs_int instrs_intval =
  if repr_of_store src1 = RInt && repr_of_store src2 = RInt then
    ( push_as fpad src1 RInt
      @ push_as fpad src2 RInt
      @ instrs_int
      @ tovalue fpad RIntUnclean
    ) |> pop_to fpad dest RIntVal None
  else
    ( push_as fpad src1 RIntVal
      @ push_as fpad src2 RIntVal
      @ instrs_intval
    ) |> pop_to fpad dest RIntVal None

let emit_intval_binary_unclean_ok fpad src1 src2 dest instrs_int instrs_intval =
  let is_ok st =
    let repr = repr_of_store st in
    repr = RInt || repr = RIntUnclean in
  if is_ok src1 && is_ok src2 then
    ( push_as fpad src1 RIntUnclean
      @ push_as fpad src2 RIntUnclean
      @ instrs_int
      @ tovalue fpad RIntUnclean
    ) |> pop_to fpad dest RIntVal None
  else
    emit_intval_binary fpad src1 src2 dest instrs_int instrs_intval

let emit_intval_int_binary fpad src1 src2 dest instrs_int instrs_intval =
  (* src2 is always an RInt *)
  if repr_of_store src1 = RInt then
    ( push_as fpad src1 RInt
      @ push_as fpad src2 RInt
      @ instrs_int
      @ tovalue fpad RIntUnclean
    ) |> pop_to fpad dest RIntVal None
  else
    ( push_as fpad src1 RIntVal
      @ push_as fpad src2 RInt
      @ instrs_intval
    ) |> pop_to fpad dest RIntVal None

let emit_binary gpad fpad op src1 src2 dest =
  match op with
    | Paddint ->
        emit_intval_binary_unclean_ok
          fpad src1 src2 dest
          [ L [ K "i32.add" ] ]
          [ L [ K "i32.add" ];
            L [ K "i32.const"; N(I32 1l) ];
            L [ K "i32.sub" ]
          ]
    | Psubint ->
        emit_intval_binary_unclean_ok
          fpad src1 src2 dest
          [ L [ K "i32.sub" ] ]
          [ L [ K "i32.sub" ];
            L [ K "i32.const"; N(I32 1l) ];
            L [ K "i32.add" ]
          ]
    | Pmulint ->
        emit_int_binary
          fpad src1 src2 dest RIntUnclean
          [ L [ K "i32.mul" ] ]
    | Pdivint ->
        ( match src2 with
            | Const n when n <> 0 ->
                emit_int_binary
                  fpad src1 src2 dest RInt
                  [ L [ K "i32.div_s" ]]
            | _ ->
                let local1 = req_tmp1_i32 fpad in
                let local2 = req_tmp2_i32 fpad in
                emit_int_binary
                  fpad src1 src2 dest RInt
                  [ L [ K "local.set"; ID local2 ];
                    L [ K "local.set"; ID local1 ];
                    L [ K "local.get"; ID local2 ];
                    L [ K "i32.eqz" ];
                    L [ K "if";
                        L ( [ [ K "then" ];
                              raise_div_by_zero fpad 0
                            ] |> List.flatten
                          )
                      ];
                    L [ K "local.get"; ID local1 ];
                    L [ K "local.get"; ID local2 ];
                    L [ K "i32.div_s" ]
                  ]
        )
    | Pmodint ->
        ( match src2 with
            | Const n when n <> 0 ->
                emit_int_binary
                  fpad src1 src2 dest RInt
                  [ L [ K "i32.rem_s" ]]
            | _ ->
                let local1 = req_tmp1_i32 fpad in
                let local2 = req_tmp2_i32 fpad in
                emit_int_binary
                  fpad src1 src2 dest RInt
                  [ L [ K "local.set"; ID local2 ];
                    L [ K "local.set"; ID local1 ];
                    L [ K "local.get"; ID local2 ];
                    L [ K "i32.eqz" ];
                    L [ K "if";
                        L ( [ [ K "then" ];
                              raise_div_by_zero fpad 0
                            ] |> List.flatten
                          )
                      ];
                    L [ K "local.get"; ID local1 ];
                    L [ K "local.get"; ID local2 ];
                    L [ K "i32.rem_s" ]
                  ]
        )
    | Pandint ->
        emit_intval_binary_unclean_ok
          fpad src1 src2 dest
          [ L [ K "i32.and" ] ]
          [ L [ K "i32.and" ] ]
    | Porint ->
        emit_intval_binary_unclean_ok
          fpad src1 src2 dest
          [ L [ K "i32.or" ] ]
          [ L [ K "i32.or" ] ]
    | Pxorint ->
        emit_intval_binary_unclean_ok
          fpad src1 src2 dest
          [ L [ K "i32.xor" ] ]
          [ L [ K "i32.xor" ];
            L [ K "i32.const"; N (I32 1l) ];
            L [ K "i32.or" ];
          ]
    | Plslint ->
        let r1 = repr_of_store src1 in
        if r1 = RInt || r1 = RIntUnclean then
          emit_int_binary_unclean_ok
            fpad src1 src2 dest
            [ L [ K "i32.shl" ]]
        else
          ( push_as fpad src1 RIntVal
            @ [ L [ K "i32.const"; N (I32 0xffff_fffel) ];
                L [ K "i32.and" ]
              ]
            @ push_as fpad src2 RIntUnclean
            @ [ L [ K "i32.shl" ];
                L [ K "i32.const"; N (I32 1l) ];
                L [ K "i32.or" ]
              ]
          ) |> pop_to fpad dest RIntVal None
    | Plsrint ->
        emit_intval_int_binary
          fpad src1 src2 dest
          [ L [ K "i32.shr_u" ];
            L [ K "i32.const"; N (I32 0x3fff_ffffl) ];
            L [ K "i32.and" ]
          ]
          [ L [ K "i32.shr_u" ];
            L [ K "i32.const"; N(I32 1l) ];
            L [ K "i32.or" ]
          ]
    | Pasrint ->
        emit_intval_int_binary
          fpad src1 src2 dest
          [ L [ K "i32.shr_s" ]]
          [ L [ K "i32.shr_s" ];
            L [ K "i32.const"; N(I32 1l) ];
            L [ K "i32.or" ]
          ]
    | (Pintcomp cmp | Puintcomp cmp) ->
        let wasm_op =
          match op with
            | Pintcomp Ceq | Puintcomp Ceq -> "i32.eq"
            | Pintcomp Cne | Puintcomp Cne -> "i32.ne"
            | Pintcomp Clt -> "i32.lt_s"
            | Pintcomp Cle -> "i32.le_s"
            | Pintcomp Cgt -> "i32.gt_s"
            | Pintcomp Cge -> "i32.ge_s"
            | Puintcomp Clt -> "i32.lt_u"
            | Puintcomp Cle -> "i32.le_u"
            | Puintcomp Cgt -> "i32.gt_u"
            | Puintcomp Cge -> "i32.ge_u"
            | _ -> assert false in
        (* TODO: we miss some optimizations when one of the operands is a
           constant *)
        if repr_comparable_as_i32 (repr_of_store src1) (repr_of_store src2) then
          (* repr doesn't matter *)
          ( push fpad src1
            @ push fpad src2
            @ [ L [ K wasm_op ]]
          ) |> pop_to fpad dest RInt None
        else
          emit_int_binary
            fpad src1 src2 dest RInt
            [ L [ K wasm_op ]]
    | Pgetvectitem ->
        ( push_as fpad src1 RValue
          @ ( match src2, repr_of_store src2 with
                | Const c, _ ->
                    [ L [ K "i32.load";
                          K (sprintf "offset=0x%lx" (Int32.shift_left (Int32.of_int c) 2));
                          K "align=2"
                        ]
                    ]
                | _, (RInt | RIntUnclean) ->
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
        ) |> pop_to fpad dest RValue None
    | Pgetstringchar | Pgetbyteschar ->
        ( push_as fpad src1 RValue
          @ ( match src2 with
                | Const c ->
                    [ L [ K "i32.load8_u";
                          K (sprintf "offset=0x%x" c)
                        ]
                    ]
                | _ ->
                    push_as fpad src2 RIntUnclean
                    @ [ L [ K "i32.add" ];
                        L [ K "i32.load8_u" ]
                      ]
            )
        ) |> pop_to fpad dest RInt None
    | Pgetmethod ->
        gpad.need_mlookup <- true;
        ( (* src2[0][src1] *)
          push_as fpad src2 RValue
          @ [ L [ K "i32.load"; K "align=2" ]]
          @ push_as fpad src1 RInt
          @ [ L [ K "i32.const"; N (I32 2l) ];
              L [ K "i32.shl" ];
              L [ K "i32.add" ];
              L [ K "i32.load"; K "align=2" ]
            ]
        ) |> pop_to fpad dest RValue None
    | Pgetdynmet ->
        gpad.need_mlookup <- true;
        ( (* mlookup(src2, arc1, NULL) *)
          push_as fpad src2 RValue
          @ push_as fpad src1 RValue
          @ push_const 0l
          @ [ L [ K "call"; ID "mlookup" ] ]
        ) |> pop_to fpad dest RValue None

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
        @ ( match src2, repr_of_store src2 with
              | Const c, _ ->
                  [ L [ K "i32.const"; N (I32 (Int32.shift_left (Int32.of_int c) 2)) ];
                    L [ K "i32.add" ]
                  ]
              | _, (RInt | RIntUnclean) ->
                  push_as fpad src2 RIntUnclean
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
        @ [ L [ K "call"; ID "caml_modify" ]]
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
                  push_as fpad src2 RIntUnclean
                  @ [ L [ K "i32.add" ] ]
                  @ push_as fpad src3 RIntUnclean
                  @ [ L [ K "i32.store8" ]]
          )

type mb_elem =
  | MB_store of store
  | MB_const of int32
  | MB_code of sexp list

let push_mb_elem fpad =
  function
  | MB_store src ->
      push_as fpad src RValue
  | MB_const n ->
      push_const n
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
  let c1 = [ C "<makeblock>" ] in
  let c2 = [ C "</makeblock>" ] in
  (ptr, c1 @ sexpl_alloc @ List.flatten sexpl_init @ c2)

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
      L [ K "i32.shl" ];
      L [ K "i32.const"; N (I32 (Int32.of_int ((subfunc lsl 2)+1))) ];
      L [ K "i32.or" ];
    ]

let closurerec gpad fpad descr src_list dest_list =
  let nfuncs = List.length dest_list in
  let envofs = nfuncs * 3 - 1 in
  let mb_src_list =
    ( List.mapi
        (fun i (_, label) ->
          ( if i > 0 then
              [ MB_const (Int32.of_int (make_header (3*i) infix_tag)) ]
            else
              []
          )
          @ [ MB_code (push_codeptr gpad label) ]
          @ [ MB_const (Int32.of_int (((envofs - 3*i) lsl 1) + 1)) ]
        )
        dest_list
      |> List.flatten
    ) @ List.map (fun src -> MB_store src) src_list in
  let ptr, sexpl_mb = makeblock fpad descr mb_src_list closure_tag in
  let sexpl_dest =
    List.mapi
      (fun i (dest, _) ->
        ( [ L [ K "local.get"; ID ptr ] ]
          @ (if i > 0 then
               [ L [ K "i32.const"; N (I32 (Int32.of_int (12*i))) ];
                 L [ K "i32.add" ]
               ]
             else
               []
            )
        ) |> pop_to fpad dest RValue None  (* descr is not up to date *)
      )
      dest_list
    |> List.flatten in
  sexpl_mb @ sexpl_dest

let wraptry n =
  if n = 1 then
    "wasicaml_wraptry"
  else
    sprintf "wasicaml_wraptry%d" n

let c_call gpad fpad descr_opt src_list name =
  (* NB. even if enable_exceptions, catch the exceptions coming from C
     because wraptry needs to restore the stack pointer to the C shadow stack
   *)
  fpad.need_lroots <- true;
  let sexpl_setup, sexpl_restore =
    match descr_opt with
      | Some descr ->
          (setup_for_gc fpad descr, restore_after_gc fpad descr)
      | None ->
          ([], []) in
  let sexpl_save_lroots =
    push_domain_field domain_field_local_roots
    @ pop_to_local "lroots" in
  let sexpl_restore_lroots =
    push_local "lroots"
    |> pop_to_domain_field domain_field_local_roots in
  let sexpl_args =
    List.concat_map
      (fun src -> push_as fpad src RValue)
      src_list in
  let sexpl_call =
    [ L [ K "i32.const"; ID name ] ]
    @ sexpl_args
    @ [ L [ K "call";
            ID (wraptry (List.length src_list))
          ]
      ]
    @ pop_to_local "accu" in
  let sexpl_exncheck =
    [ L [ K "local.get"; ID "accu" ];
      L [ K "i32.eqz" ];
      L [ K "if";
          L ([K "then"] @ throw fpad)
        ]
    ] in
  let p_i32 = L [ K "param"; K "i32" ] in
  let r_i32 = L [ K "result"; K "i32" ] in
  let ty = (src_list |> List.map (fun _ -> p_i32)) @ [ r_i32 ] in
  Hashtbl.replace gpad.primitives name ty;
  (* debug2 20 0 *)
  (* @ sexpl_debug_args *)
  sexpl_setup @ sexpl_save_lroots @ sexpl_call
  @ sexpl_restore_lroots @ sexpl_restore @ sexpl_exncheck
  (* @ debug2 20 1 *)

let c_call_vector gpad fpad descr numargs depth name =
  fpad.need_lroots <- true;
  let sexpl_setup = setup_for_gc fpad descr in
  let sexpl_restore = restore_after_gc fpad descr in
  let sexpl_save_lroots =
    push_domain_field domain_field_local_roots
    @ pop_to_local "lroots" in
  let sexpl_restore_lroots =
    push_local "lroots"
    |> pop_to_domain_field domain_field_local_roots in
  let sexpl_call =
    [ L [ K "i32.const"; ID name ] ]
    @ push_field_addr "fp" (-depth)
    @ push_const (Int32.of_int numargs)
    @ [ L [ K "call"; ID "wasicaml_wraptry2" ]]
    @ pop_to_local "accu" in
  let sexpl_exncheck =
    [ L [ K "local.get"; ID "accu" ];
      L [ K "i32.eqz" ];
      L [ K "if";
          L ([K "then"] @ throw fpad)
        ]
    ] in
  let ty =
    [ L [ K "param"; K "i32" ];
      L [ K "param"; K "i32" ];
      L [ K "result"; K "i32" ] ] in
  Hashtbl.replace gpad.primitives name ty;
  (* debug2 20 0 *)
  sexpl_setup @ sexpl_save_lroots @ sexpl_call
  @ sexpl_restore_lroots @ sexpl_restore @ sexpl_exncheck

let string_label =
  function
  | Label k -> sprintf "label%d" k
  | Loop k -> sprintf "loop%d" k

let switch fpad src labls_ints labls_blocks =
  fpad.need_panic <- true;
  let value = req_tmp1_i32 fpad in
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

let grab fpad num =
  let sexpl =
    [ (* if (codeptr & 1) *)
      L [ K "local.get"; ID "codeptr" ];
      L [ K "i32.const"; N (I32 code_pointer_restart_mask) ];
      L [ K "i32.and" ];
      L [ K "if";
          L ( [ K "then";
                (* RESTART *)
                (* (extra_args, fp) = restart_helper(envptr, extra_args, fp) *)
                L [ K "local.get"; ID "envptr" ];
                L [ K "local.get"; ID "extra_args" ];
                L [ K "local.get"; ID "fp" ];
              ]
              @ call_restart_helper()
              @ pop_to_fp fpad
              @ [ L [ K "local.set"; ID "extra_args" ];

                  (* codeptr &= ~1 - I think this is not needed *)
                  (*
                  L [ K "local.get"; ID "codeptr" ];
                  L [ K "i32.const"; N (I32 0xffff_fffel) ];
                  L [ K "i32.and" ];
                  L [ K "local.set"; ID "codeptr" ];
                   *)
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
          L ( [ [ K "else";
                  L [ K "local.get"; ID "envptr" ];
                  L [ K "local.get"; ID "extra_args" ];
                  L [ K "local.get"; ID "codeptr" ];
                  L [ K "local.get"; ID "fp" ];
                ];
                returncall "grab_helper"
              ] |> List.flatten
            )
        ];
    ] in
  sexpl

let return() =
  (* NB. don't use bp here.
     codeptr is already destroyed when coming from appterm_common
   *)
  [ C "$return";
    L [ K "local.get"; ID "extra_args" ];
    L [ K "if";
        L ( [ [ K "then";
                L [ K "local.get"; ID "envptr" ];
                L [ K "local.get"; ID "extra_args" ];
                L [ K "local.get"; ID "fp" ];
                L [ K "local.get"; ID "accu" ];
              ];
              returncall "return_helper"
            ] |> List.flatten
          )
      ];
    L [ K "local.get"; ID "accu" ];
    L [ K "return" ];
  ]

let use_env gpad funlabel numargs =
  let funcprops = Wc_tracefuncs.get_funcprops gpad.funcprops_table funlabel in
  funcprops.use_env ||
    ( match funcprops.func_arity with
        | None -> true
        | Some n -> n <> numargs
    )

let use_codeptr gpad funlabel numargs =
  (not !enable_returncall) ||
    ( let funcprops =
        Wc_tracefuncs.get_funcprops gpad.funcprops_table funlabel in
      ( match funcprops.func_arity with
          | None -> true
          | Some n -> n <> numargs (* GRAB *)
      )
    )

let apply_direct_no_eh gpad fpad funlabel numargs depth =
  let _, letrec_label, _ = lookup_label gpad funlabel in
  let letrec_name = Hashtbl.find gpad.letrec_name letrec_label in
  let env_pos = (-depth + numargs) in
  ( if use_env gpad funlabel numargs then
      ( push_local "accu"
        |> pop_to_stack fpad env_pos
      )
      @ push_field_addr "fp" env_pos (* new envptr *)
    else
      [ C "omit_envptr";
        L [ K "i32.const"; N (I32 (-1l)) ] ]
  )
  @ [ L [ K "i32.const"; N (I32 (Int32.of_int (numargs-1))) ];
    ]
  @ ( if use_codeptr gpad funlabel numargs then
        push_field "accu" 0
      else
        [ L [ K "i32.const"; N (I32 0l) ] ]
    )
  @ push_local "fp"
  @ [L [ K "i32.const"; N (I32 (Int32.of_int (4 * depth))) ];
     L [ K "i32.sub" ];
     L [ K "call";
          ID letrec_name
        ];
    ]
  @ if not !quick_exceptions then
      [ L [ K "local.set"; ID "accu" ]]
    else
      [ L [ K "local.tee"; ID "accu" ];
        L [ K "i32.eqz" ];  (* check for exceptions *)
        L [ K "if";
            L ([ K "then"] @ throw fpad)
          ];
      ]

let apply_direct_eh gpad fpad funlabel numargs depth =
  fpad.need_lroots <- true;
  let _, letrec_label, _ = lookup_label gpad funlabel in
  let letrec_name = Hashtbl.find gpad.letrec_name letrec_label in
  let env_pos = (-depth + numargs) in
  ( if use_env gpad funlabel numargs then
      ( push_local "accu"
        |> pop_to_stack fpad env_pos
      )
    else
      []
  )
  @ push_domain_field domain_field_local_roots
  @ pop_to_local "lroots"
  @ [ L [ K "i32.const"; ID letrec_name ] ]
  @ ( if use_env gpad funlabel numargs then
        push_field_addr "fp" env_pos (* new envptr *)
      else
        [ C "omit_envptr";
          L [ K "i32.const"; N (I32 (-1l)) ] ]
    )
  @ [ L [ K "i32.const"; N (I32 (Int32.of_int (numargs-1))) ];
    ]
  @ ( if use_codeptr gpad funlabel numargs then
        push_field "accu" 0
      else
        [ L [ K "i32.const"; N (I32 0l) ] ]
    )
  @ push_local "fp"
  @ [L [ K "i32.const"; N (I32 (Int32.of_int (4 * depth))) ];
     L [ K "i32.sub" ];

     L [ K "call"; ID "wasicaml_wraptry4" ]
    ]
  @ ( push_local "lroots"
      |> pop_to_domain_field domain_field_local_roots
    )
  @ [ L [ K "local.tee"; ID "accu" ];
      L [ K "i32.eqz" ];  (* check for exceptions *)
      L [ K "if";
          L ([ K "then"] @ throw fpad)
        ];
    ]

let apply_direct gpad fpad funlabel numargs depth =
  if fpad.lpad.scope.cfg_try_labels = [] then
    (* outside "try": no need to catch exceptions *)
    apply_direct_no_eh gpad fpad funlabel numargs depth
  else
    (* inside "try": catch exceptions from every function call *)
    apply_direct_eh gpad fpad funlabel numargs depth

let apply_no_eh fpad numargs depth =
  let env_pos = (-depth + numargs) in
  let codeptr = req_tmp1_i32 fpad in
  ( push_local "accu"
    |> pop_to_stack fpad env_pos
  )
  @ push_field_addr "fp" env_pos (* new envptr *)
  @ [ L [ K "i32.const"; N (I32 (Int32.of_int (numargs-1))) ];
    ]
  @ push_field "accu" 0
  @ (if !enable_deadbeef_check then
       [ L [ K "local.get"; ID "fp" ];
         L [ K "i32.const"; N (I32 (Int32.of_int (4 * depth))) ];
         L [ K "i32.sub" ];
         L [ K "local.get"; ID "fp" ];
         L [ K "call"; ID "deadbeef_check" ]
       ]
     else
       []
    )
  @ [ L [ K "local.tee"; ID codeptr ];

      L [ K "local.get"; ID "fp" ];
      L [ K "i32.const"; N (I32 (Int32.of_int (4 * depth))) ];
      L [ K "i32.sub" ];
      L [ K "local.get"; ID codeptr ];
      L [ K "i32.const"; N (I32 (Int32.of_int code_pointer_shift)) ];
      L [ K "i32.shr_u" ];

      L [ K "call_indirect";
          N (I32 0l);     (* table index *)
          L [ K "param"; K "i32" ];
          L [ K "param"; K "i32" ];
          L [ K "param"; K "i32" ];
          L [ K "param"; K "i32" ];
          L [ K "result"; K "i32" ];
        ];
    ]
  @ (if !quick_exceptions then
       [ L [ K "local.tee"; ID "accu" ];
         L [ K "i32.eqz" ];  (* check for exceptions *)
         L [ K "if";
             L ([ K "then"] @ throw fpad)
           ];
       ]
     else
       [ L [ K "local.set"; ID "accu" ]]
    )

let apply_eh fpad numargs depth =
  fpad.need_lroots <- true;
  let env_pos = (-depth + numargs) in
  let codeptr = req_tmp1_i32 fpad in
  ( push_local "accu"
    |> pop_to_stack fpad env_pos
  )
  @ push_domain_field domain_field_local_roots
  @ pop_to_local "lroots"

  @ push_field "accu" 0
  @ [ L [ K "local.tee"; ID codeptr ];
      L [ K "i32.const"; N (I32 (Int32.of_int code_pointer_shift)) ];
      L [ K "i32.shr_u" ];
    ]
  @ push_field_addr "fp" env_pos (* new envptr *)
  @ [ L [ K "i32.const"; N (I32 (Int32.of_int (numargs-1))) ];
    ]
  @ push_local codeptr
  @ (if !enable_deadbeef_check then
       [ L [ K "local.get"; ID "fp" ];
         L [ K "i32.const"; N (I32 (Int32.of_int (4 * depth))) ];
         L [ K "i32.sub" ];
         L [ K "local.get"; ID "fp" ];
         L [ K "call"; ID "deadbeef_check" ]
       ]
     else
       []
    )
  @ [ L [ K "local.get"; ID "fp" ];
      L [ K "i32.const"; N (I32 (Int32.of_int (4 * depth))) ];
      L [ K "i32.sub" ];

      L [ K "call"; ID "wasicaml_wraptry4" ]
    ]
  @ ( push_local "lroots"
      |> pop_to_domain_field domain_field_local_roots
    )
  @  [ L [ K "local.tee"; ID "accu" ];
       L [ K "i32.eqz" ];  (* check for exceptions *)
       L [ K "if";
           L ([ K "then"] @ throw fpad)
         ];
     ]

let apply fpad numargs depth =
  if fpad.lpad.scope.cfg_try_labels = [] then
    (* outside "try": no need to catch exceptions *)
    apply_no_eh fpad numargs depth
  else
    (* inside "try": catch exceptions from every function call *)
    apply_eh fpad numargs depth

let appterm_common fpad () =
  (* NB. don't use bp here *)
  [ C "$appterm_common";
    L [ K "local.get"; ID "envptr" ];
    L [ K "local.get"; ID "codeptr" ];
    L [ K "local.get"; ID "accu" ];
    L [ K "local.get"; ID "extra_args" ];
    L [ K "local.get"; ID "appterm_new_num_args" ];
  ]
  @ call_appterm_helper()
  @ [ L [ K "local.set"; ID "extra_args" ];
      L [ K "local.tee"; ID "codeptr" ];
      L [ K "if";
          L [ K "then";
              (* same letrec: we can jump! *)
              L [ K "br"; ID "startover" ]
            ];
          L [ K "else";
              (* different letrec: call + return *)
              (* we can jump to $return *)
              L [ K "br"; ID "return" ]
            ]
        ]
    ]

let call_reinit_frame gpad fpad numargs oldnumargs depth =
  if numargs <= 10 then (
    gpad.need_reinit_frame_k <- ISet.add numargs gpad.need_reinit_frame_k;
    push_local "fp"
    @ push_const (Int32.of_int depth)
    @ push_const (Int32.of_int oldnumargs)
    @ [ L [ K "call"; ID (sprintf "reinit_frame_%d" numargs) ] ]
  ) else (
    gpad.need_reinit_frame <- true;
    push_local "fp"
    @ push_const (Int32.of_int depth)
    @ push_const (Int32.of_int oldnumargs)
    @ push_const (Int32.of_int numargs)
    @ [ L [ K "call"; ID "reinit_frame" ] ]
  )

let appterm_push_params gpad funlabel_opt numargs =
  [ L [ K "local.get"; ID "envptr" ];      (* first arg of call *)
  ]
  @ [ L [ K "local.get"; ID "extra_args" ] (* second arg of call *)
    ]
  @ (if numargs >= 2 then
       [ L [ K "i32.const"; N (I32 (Int32.of_int (numargs - 1))) ];
         L [ K "i32.add" ];
       ]
     else
       []
    )
  @ (match funlabel_opt with
       | Some funlabel ->
           if use_codeptr gpad funlabel numargs then
             push_field "accu" 0
           else
             [ L [ K "i32.const"; N (I32 0l) ] ]
       | None ->
           push_field "accu" 0         (* third arg of call: code pointer *)
    )
  @ [ L [ K "local.get"; ID "fp" ]     (* fourth arg of call *)
    ]

let appterm_with_returncall gpad fpad numargs oldnumargs depth =
  let sexpl =
    call_reinit_frame gpad fpad numargs oldnumargs depth
    @ pop_to_local "fp"   (* no need to set bp here *)
    @ (push_local "accu" |> pop_to_field "envptr" 0)
    @ appterm_push_params gpad None numargs
    @ push_field "accu" 0
    @ [ L [ K "i32.const"; N (I32 (Int32.of_int code_pointer_shift)) ];
        L [ K "i32.shr_u" ];             (* which function to call *)
      ]
    @ [ L [ K "return_call_indirect";
            N (I32 0l);    (* table *)
            L [ K "param"; K "i32" ];
            L [ K "param"; K "i32" ];
            L [ K "param"; K "i32" ];
            L [ K "param"; K "i32" ];
            L [ K "result"; K "i32" ];
          ]
      ] in
  sexpl

let appterm_without_returncall gpad fpad numargs oldnumargs depth =
  let sexpl =
    call_reinit_frame gpad fpad numargs oldnumargs depth
    @ pop_to_local "fp"   (* no need to set bp here *)
    @ push_const (Int32.of_int numargs)
    @ pop_to_local "appterm_new_num_args"
    @ [ L [ K "br"; ID "appterm_common" ] ] in
  fpad.need_appterm_common <- true;
  fpad.need_return <- true;
  if not !enable_returncall then
    fpad.need_startover <- true;
  sexpl

let appterm_direct gpad fpad funlabel numargs oldnumargs depth =
  (* Wc_unstack must not emit Wappterm_direct when tail calls are
     unavailable *)
  assert(!enable_returncall);
  let _, letrec_label, _ = lookup_label gpad funlabel in
  let letrec_name = Hashtbl.find gpad.letrec_name letrec_label in
  let goto_selfrecurse =
    letrec_label = fpad.fpad_letrec_label
    && numargs = oldnumargs in
  let sexpl =
    call_reinit_frame gpad fpad numargs oldnumargs depth
    @ pop_to_local "fp"   (* no need to set bp here *)
    @ ( if use_env gpad funlabel numargs then
          push_local "accu" |> pop_to_field "envptr" 0
        else
          [ C "omit_envptr" ]
      )
    @ (if goto_selfrecurse then
         [ L [ K "br"; ID "selfrecurse" ] ]
       else
         appterm_push_params gpad (Some funlabel) numargs
         @ [ L [ K "return_call";
                 ID letrec_name
               ]
           ]
      ) in
  if goto_selfrecurse then
    fpad.need_selfrecurse <- true;
  sexpl

let appterm gpad fpad funlabel_opt numargs oldnumargs depth =
  if !enable_returncall then
    match funlabel_opt with
      | Some funlabel ->
          appterm_direct gpad fpad funlabel numargs oldnumargs depth
      | None ->
          appterm_with_returncall gpad fpad numargs oldnumargs depth
  else
    appterm_without_returncall gpad fpad numargs oldnumargs depth

let appterm_args gpad fpad funlabel_opt funsrc argsrc oldnumargs depth =
  (* Wc_unstack must not emit Wappterm_args when tail calls are
     unavailable *)
  assert(!enable_returncall);
  let newnumargs = List.length argsrc in
  let fp_delta = oldnumargs - newnumargs in
  (* save argsrc in local variables *)
  let arg_locals =
    List.mapi
      (fun i src ->
        match src with
          | RealStack pos ->
              if fp_delta <> 0 || pos <> i then
                let repr = repr_of_store src in
                Local(repr, new_local fpad repr)
              else
                src
          | RealAccu _ -> (* because accu gets destroyed *)
              let repr = repr_of_store src in
              Local(repr, new_local fpad repr)
          | _ -> src
      )
      argsrc in
  let arg_instrs1 =
    List.map2
      (fun src local ->
        if src = local then
          []
        else
          copy fpad src local None
      )
      argsrc
      arg_locals
    |> List.flatten in
  (* set "accu" to the function pointer: *)
  let accu_instrs =
    match funsrc with
      | RealAccu _ ->
          []
      | _ ->
          copy fpad funsrc (RealAccu { no_function = false }) None in
  (* fp = fp + old_num_args - new_num_args *)
  let fp_instrs =
    if fp_delta = 0 then
      []
    else
      [ L [ K "local.get"; ID "fp" ];
        L [ K "i32.const"; N (I32 (Int32.of_int (4 * fp_delta))) ];
        L [ K "i32.add" ];
        L [ K "local.set"; ID "fp" ];
      ] in
  let arg_instrs2 =
    List.map2
      (fun src (src, i) ->
        let dest = RealStack i in
        copy fpad src dest None
      )
      argsrc
      (List.mapi (fun i local -> (local, i)) arg_locals)
    |> List.flatten in
  let envptr_instrs_dfl =
    push_local "accu" |> pop_to_field "envptr" 0 in
  let envptr_instrs =
    match funlabel_opt with
      | Some funlabel ->
          if use_env gpad funlabel newnumargs then
            envptr_instrs_dfl
          else
            [ C "omit_envptr" ]
      | None ->
          envptr_instrs_dfl in
  let call_instrs =
    match funlabel_opt with
      | Some funlabel ->
          let _, letrec_label, _ = lookup_label gpad funlabel in
          let letrec_name = Hashtbl.find gpad.letrec_name letrec_label in
          let goto_selfrecurse =
            letrec_label = fpad.fpad_letrec_label
            && newnumargs = oldnumargs in
          if goto_selfrecurse then (
            fpad.need_selfrecurse <- true;
            [ L [ K "br"; ID "selfrecurse" ] ]
          ) else
            appterm_push_params gpad funlabel_opt newnumargs
            @ [ L [ K "return_call";
                    ID letrec_name
                  ]
              ]
      | None ->
          appterm_push_params gpad funlabel_opt newnumargs
          @ push_field "accu" 0
          @ [ L [ K "i32.const"; N (I32 (Int32.of_int code_pointer_shift)) ];
              L [ K "i32.shr_u" ];             (* which function to call *)
            ]
          @ [ L [ K "return_call_indirect";
                  N (I32 0l);    (* table *)
                  L [ K "param"; K "i32" ];
                  L [ K "param"; K "i32" ];
                  L [ K "param"; K "i32" ];
                  L [ K "param"; K "i32" ];
                  L [ K "result"; K "i32" ];
                ]
            ] in
  arg_instrs1
  @ accu_instrs
  @ fp_instrs
  @ arg_instrs2
  @ envptr_instrs
  @ call_instrs

let rec emit_instr gpad fpad instr =
  match instr with
    | Wcomment s ->
        []
    | Wadjust _ ->
        []
    | Wblock arg ->
        let old_lpad = fpad.lpad in
        let new_lpad = Wc_unstack.lpad_with ~scope:arg.scope old_lpad in
        fpad.lpad <- new_lpad;
        let instrs =
          match arg.label with
            | Label lab ->
                [ L ( [ K "block";
                        ID (sprintf "label%d" lab);
                        BR
                      ]
                      @ emit_instrs gpad fpad arg.body
                    )
                ]
            (* @ debug2 100 lab *)
            | Loop lab ->
                [ L ( [ K "loop";
                        ID (sprintf "loop%d" lab);
                        BR
                      ]
                      @ emit_instrs gpad fpad arg.body
                    )
                ] in
        fpad.lpad <- old_lpad;
        instrs
    | Wcond { cond; ontrue; onfalse } ->
        emit_instr gpad fpad (if !cond then ontrue else onfalse)
    | Wcopy arg ->
        copy fpad arg.src arg.dest None
    | Walloc arg ->
        copy fpad arg.src arg.dest (Some arg.descr)
    | Wenv arg ->
        ( push_env
          @ load_offset (4 * arg.field)
        ) |> pop_to fpad arg.dest RValue None
    | Wcopyenv arg ->
        push_env
        @ add_offset (4 * arg.offset)
        @ pop_to_local "accu"
    | Wgetglobal arg ->
        let Global offset = arg.src in
        (*
        debug2 160 offset
        @ push_const 161l
        @ [
            L [ K "global.get";
              ID "wasicaml_global_data";
            ];
          L [ K "i32.load" ];
          L [ K "i32.const";
              N (I32 (Int32.of_int (4 * offset)));
            ];
          L [ K "i32.add" ];
          L [ K "call"; ID "debug2" ]
        ]
         *)
        push_global offset
        |> pop_to fpad arg.dest RValue None
    | Wunary arg ->
        emit_unary gpad fpad arg.op arg.src1 arg.dest
    | Wunaryeffect arg ->
        emit_unaryeffect fpad arg.op arg.src1
    | Wbinary arg ->
        emit_binary gpad fpad arg.op arg.src1 arg.src2 arg.dest
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
        c_call gpad fpad arg.descr arg.src arg.name
    | Wccall_vector arg ->
        c_call_vector gpad fpad arg.descr arg.numargs arg.depth arg.name
    | Wbranch arg ->
        [ L [ K "br"; ID (string_label arg.label) ]]
    | Wif arg ->
        ( match repr_of_store arg.src with
            | RInt ->
                push_as fpad arg.src RInt
                @ (if arg.neg then [ L [ K "i32.eqz" ]] else [])
            | RIntUnclean ->
                push_as fpad arg.src RIntUnclean
                @ [ L [ K "i32.const"; N (I32 0x7fff_ffffl) ];
                    L [ K "i32.and" ]
                  ]
                @ (if arg.neg then [ L [ K "i32.eqz" ]] else [])
            | _ ->
                push_as fpad arg.src RIntVal
                @ [ L [ K "i32.const"; N (I32 1l) ];
                    L [ K (if arg.neg then "i32.le_u" else "i32.gt_u") ]
                  ]
        )
        @ [ L [ K "if";
                L ( K "then" ::
                      ( List.map (emit_instr gpad fpad) arg.body
                        |> List.flatten
                      )
                  )
              ]
          ]
    | Wswitch arg ->
        switch fpad arg.src arg.labels_int arg.labels_blk
    | Wapply arg ->
        apply fpad arg.numargs arg.depth
    | Wapply_direct arg ->
        (* TODO: the copy is only needed for calling functions that actually
           access the environment. Note that, however, functions not
           accessing the environment at all seem to be relatively rare.
         *)
        let fn = Wc_traceglobals.Unknown in (* this arg is ignored by [copy] *)
        let src = TracedGlobal(arg.global, arg.path, fn) in
        copy fpad src Wc_unstack.real_accu None
        @ apply_direct gpad fpad arg.funlabel arg.numargs arg.depth
    | Wappterm arg ->
        assert(fpad.lpad.scope.cfg_try_labels = []); (* outside "try" *)
        appterm gpad fpad None arg.numargs arg.oldnumargs arg.depth
    | Wappterm_direct arg ->
        (* TODO: the copy is only needed for calling functions that actually
           access the environment. Note that, however, functions not
           accessing the environment at all seem to be relatively rare.
         *)
        assert(fpad.lpad.scope.cfg_try_labels = []); (* outside "try" *)
        let fn = Wc_traceglobals.Unknown in (* this arg is ignored by [copy] *)
        let src = TracedGlobal(arg.global, arg.path, fn) in
        copy fpad src Wc_unstack.real_accu None
        @ appterm
            gpad fpad (Some arg.funlabel) arg.numargs arg.oldnumargs arg.depth
    | Wappterm_args arg ->
        assert(fpad.lpad.scope.cfg_try_labels = []); (* outside "try" *)
        appterm_args
          gpad fpad None arg.funsrc arg.argsrc arg.oldnumargs arg.depth
    | Wappterm_direct_args arg ->
        assert(fpad.lpad.scope.cfg_try_labels = []); (* outside "try" *)
        appterm_args
          gpad fpad (Some arg.funlabel) arg.funsrc arg.argsrc arg.oldnumargs
          arg.depth
    | Wreturn arg ->
        let no_function, is_accu =
        match arg.src with
          | RealAccu { no_function } -> no_function, true
          | _ -> repr_of_store arg.src <> RValue, false in
        if no_function then
          push_as fpad arg.src RValue
          @ [ L [ K "return" ]]
        else (
          (* return value could be another closure *)
          fpad.need_return <- true;
          ( if is_accu then [] else
              push_as fpad arg.src RValue
              @ pop_to_local "accu"
          )
          @ push_field_addr "fp" arg.arity
          @ pop_to_local "fp"
          @ [ L [ K "br"; ID "return" ]]
        )
    | Wgrab arg ->
        if fpad.disallow_grab then (
          (* disallow_grab is set when there shouldn't be (another) Wgrab
             after some structural analysis/transform
           *)
          let letrec_name =
            Hashtbl.find gpad.letrec_name fpad.fpad_letrec_label in
          eprintf "[DEBUG] function: %s\n%!" letrec_name;
          assert false;
        );
        grab fpad (arg.arity-1)
    | Wclosurerec arg ->
        closurerec gpad fpad arg.descr arg.src arg.dest
    | Wraise arg ->
        ( push_as fpad arg.src RValue
          |> pop_to_domain_field domain_field_exn_bucket
        )
        @ ( push_local "fp"
            |> pop_to_domain_field domain_field_extern_sp
          )
        @ throw fpad
    | Wtryreturn arg ->
        push_as fpad arg.src RValue
        @ [ L [ K "return" ] ]
    | Wnextmain { label } ->
        push_local "accu"  (* sic! *)
        @ push_local "extra_args"
        @ push_local "codeptr"
        @ push_local "fp"
        @ returncall (sprintf "letrec_main%d" label)
    | Wstop ->
        [ L [ K "i32.const"; N (I32 0l) ];
          (* L ( K "block" :: debug2 1 2); *)
          L [ K "return" ]
        ]
    | Wnop ->
        []
    | Wunreachable ->
        [ L [ K "unreachable" ]]

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

let rec extract_grab instrs =
  (* Wc_unstack puts the Wgrab always in front *)
  match instrs with
    | Wgrab _ as grab :: rest ->
        (rest, Some grab)
    | _ ->
        (instrs, None)

let emit_fblock_instrs gpad fpad instrs =
  fpad.disallow_grab <- false;
  let lpad = Wc_unstack.lpad_with ~scope:fpad.fpad_scope fpad.lpad in
  fpad.lpad <- lpad;
  let rest, grab_opt = extract_grab instrs in
  match grab_opt with
    | Some grab ->
        let code_grab = emit_instrs gpad fpad [ grab ] in
        fpad.disallow_grab <- true;
        let code_rest = emit_instrs gpad fpad rest in
        if fpad.need_selfrecurse then (
          assert(!enable_returncall);
          code_grab
            @ [ L ( [ K "loop"; ID "selfrecurse"; BR ] @ code_rest) ]
        ) else
          code_grab @ code_rest
    | None ->
        fpad.disallow_grab <- true;
        let code = emit_instrs gpad fpad instrs in
        if fpad.need_selfrecurse then (
          assert(!enable_returncall);
          [ L ( [ K "loop"; ID "selfrecurse"; BR ] @ code) ]
        ) else
          code

let emit_fblock gpad fpad fblock =
  let maxdepth = Wc_tracestack.max_stack_depth_of_fblock fblock in
  (* make maxdepth a bit larger than stricly necessary to completely avoid
     bp[k] with k<0 *)
  fpad.maxdepth <- if maxdepth > 0 then maxdepth + 2 else 0;
  fpad.have_bp <- maxdepth > 0;
  let instrs =
    Wc_unstack.transl_fblock fpad.lpad fblock
    |> emit_fblock_instrs gpad fpad in
  set_bp fpad
  @ (if !enable_deadbeef_check && fpad.maxdepth > 0 then
       [ L [ K "local.get"; ID "bp" ];
         L [ K "local.get"; ID "fp" ];
         L [ K "call"; ID "deadbeef_init" ]
       ]
     else
       []
    )
  @ instrs

let get_funcmapping_without_tailcalls scode =
  let open Wc_control in
  let funcmapping = Hashtbl.create 7 in
  let subfunction_num = Hashtbl.create 7 in
  let subfunctions = Hashtbl.create 7 in
  IMap.iter
    (fun func_label fblock ->
      let letrec_func_label =
        match fblock.scope.cfg_letrec_label with
          | None -> 0
          | Some label -> label in
      let letrec_label =
        if fblock.scope.cfg_main then
          Main letrec_func_label
        else
          Func letrec_func_label in
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

let get_funcmapping_with_tailcalls scode =
  (* simplified: do not generate subfunctions *)
  let open Wc_control in
  let funcmapping = Hashtbl.create 7 in
  let subfunctions = Hashtbl.create 7 in
  IMap.iter
    (fun func_label fblock ->
      let letrec_label =
        if fblock.scope.cfg_main then
          Main func_label
        else
          Func func_label in
      Hashtbl.add funcmapping func_label (letrec_label, 0);
      Hashtbl.replace subfunctions letrec_label [func_label]
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

let get_funcmapping scode =
  if !enable_returncall then
    get_funcmapping_with_tailcalls scode
  else
    get_funcmapping_without_tailcalls scode

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

let cond_section_lz cond label sexpl_section sexpl_users =
  if cond then
    [ L ( [ K "block"; ID label; BR ] @ sexpl_users ) ] @ sexpl_section()
  else
    sexpl_users

let cond_section cond label sexpl_section sexpl_users =
  cond_section_lz cond label (fun () -> sexpl_section) sexpl_users

let cond_loop cond label sexpl =
  if cond then
    [ L ( [ K "loop"; ID label; BR ] @ sexpl ) ]
  else
    sexpl

let eff_label =
  function
  | Func l -> l
  | Main l -> l

let init_lpad_for_subfunc gpad fpad func_label =
  let func_offset, environment =
    ( try Hashtbl.find gpad.glbfun_table func_label
      with Not_found -> 0, [| |]
    ) in
  fpad.lpad <- { fpad.lpad with
                 environment;
                 func_offset
               }

let generate_function scode gpad letrec_label func_name subfunc_labels export_flag =
  let fpad = empty_fpad letrec_label in
  Hashtbl.add fpad.lpad.locals "accu" RValue;
  Hashtbl.add fpad.lpad.locals "bp" RValue;
  fpad.lpad <- { fpad.lpad with
                 local_limit = if export_flag then 0 else fpad.lpad.local_limit;
                   (* avoid local vars in the long main function *)
                 globals_table = gpad.globals_table;
                 funcprops_table = gpad.funcprops_table;
               };

  let subfunc_pairs =
    List.map
      (fun func_label ->
        let fblock = IMap.find func_label Wc_control.(scode.functions) in
        fpad.fpad_scope <- fblock.scope;
        let label = sprintf "func%d" func_label in
        init_lpad_for_subfunc gpad fpad func_label;
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
              L [ K "i32.const"; N (I32 2l) ];
              L [ K "i32.shr_u" ];
              L ( [ K "br_table" ] @ labels )
            ] in
          block_cascade body subfunc_pairs_with_panic in
  let sexpl =
    ( match letrec_label with
        | Main 0 | Func _ ->
            [ L [ K "i32.const"; N (I32 1l) ];
              L [ K "local.set"; ID "accu" ]
            ]
        | Main _ ->
            (* envptr is abused to pass on the accu from one main function
               to the next *)
            [ L [ K "local.get"; ID "envptr" ];
              L [ K "local.set"; ID "accu" ];
              L [ K "i32.const"; N (I32 0xffff_fffcl) ];
              L [ K "local.set"; ID "envptr" ]
            ]
    )
    @ (subfunc_sexpl
       |> cond_section_lz
            fpad.need_appterm_common "appterm_common" (appterm_common fpad)
       |> cond_loop
            fpad.need_startover "startover"
       |> cond_section_lz
            fpad.need_return "return" return
      ) in

  if fpad.need_appterm_common then (
    Hashtbl.add fpad.lpad.locals "appterm_new_num_args" RInt;
  );

  if fpad.need_xalloc then
    Hashtbl.add fpad.lpad.locals "xalloc" RValue;
  if fpad.need_lroots then
    Hashtbl.add fpad.lpad.locals "lroots" RValue;
  if fpad.need_tmp1_i32 then
    Hashtbl.add fpad.lpad.locals "tmp1_i32" RValue;
  if fpad.need_tmp2_i32 then
    Hashtbl.add fpad.lpad.locals "tmp2_i32" RValue;
  if fpad.need_tmp1_f64 then
    Hashtbl.add fpad.lpad.locals "tmp1_f64" RFloat;

  let locals =
    Hashtbl.fold (fun name vtype acc -> (name,vtype) :: acc) fpad.lpad.locals []
    @ (Hashtbl.fold (fun pos _ acc -> (string_of_localPos pos,RValue) :: acc) fpad.lpad.localPos [])
  in

  let letrec =
    [ L ( [ K "func";
            ID func_name;
          ]
          @ (if export_flag then [ L [ K "export"; S func_name ]] else [])
          @ [ L [ K "param"; ID "envptr"; K "i32" ];
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
            (*
             @ debug2 0 (eff_label letrec_label)
             @ debug2_var 10 "fp"
             @ debug2_var 11 "envptr"
             @ debug2_var 12 "codeptr"
             @ debug2_var 13 "extra_args"
             @ [ L [ K "local.get"; ID "envptr" ];   (* print 14: env, if possible *)
              L [ K "i32.const"; N (I32 (-1l)) ];
              L [ K "i32.ne" ];
              L [ K "if";
                  L ( [ K "then";
                        L [ K "i32.const"; N (I32 14l) ]]
                      @ push_env
                      @ [ L [ K "call"; ID "debug2" ]]
                    )
                ]
            ]
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
  let export = (letrec_label = Main 0) in
  generate_function scode gpad letrec_label func_name subfunc_labels export

let globals() =
  [ "wasicaml_global_data", true, TI32;
    "wasicaml_domain_state", true, TI32;
    "wasicaml_atom_table", true, TI32;
    "wasicaml_stack_threshold", true, TI32;
  ]
  @ if !enable_multireturn then [] else
      [ "retval2", true, TI32;
        "retval3", true, TI32;
      ]

let imp_functions =
  [ "env", "caml_alloc_small_dispatch",
    [ L [ K "param"; K "i32" ];
      L [ K "param"; K "i32" ];
      L [ K "param"; K "i32" ];
      L [ K "param"; K "i32" ];
    ];
    "env", "caml_alloc_small",
    [ L [ K "param"; K "i32" ];
      L [ K "param"; K "i32" ];
      L [ K "result"; K "i32" ]
    ];
    "env", "caml_alloc_shr",
    [ L [ K "param"; K "i32" ];
      L [ K "param"; K "i32" ];
      L [ K "result"; K "i32" ]
    ];
    "env", "caml_initialize",
    [ L [ K "param"; K "i32" ];
      L [ K "param"; K "i32" ];
    ];
    "env", "caml_modify",
    [ L [ K "param"; K "i32" ];
      L [ K "param"; K "i32" ];
    ];
    "env", "wasicaml_wraptry",
    [ L [ K "param"; K "i32" ];
      L [ K "param"; K "i32" ];
      L [ K "result"; K "i32" ]
    ];
    "env", "wasicaml_wraptry2",
    [ L [ K "param"; K "i32" ];
      L [ K "param"; K "i32" ];
      L [ K "param"; K "i32" ];
      L [ K "result"; K "i32" ]
    ];
    "env", "wasicaml_wraptry3",
    [ L [ K "param"; K "i32" ];
      L [ K "param"; K "i32" ];
      L [ K "param"; K "i32" ];
      L [ K "param"; K "i32" ];
      L [ K "result"; K "i32" ]
    ];
    "env", "wasicaml_wraptry4",
    [ L [ K "param"; K "i32" ];
      L [ K "param"; K "i32" ];
      L [ K "param"; K "i32" ];
      L [ K "param"; K "i32" ];
      L [ K "param"; K "i32" ];
      L [ K "result"; K "i32" ]
    ];
    "env", "wasicaml_wraptry5",
    [ L [ K "param"; K "i32" ];
      L [ K "param"; K "i32" ];
      L [ K "param"; K "i32" ];
      L [ K "param"; K "i32" ];
      L [ K "param"; K "i32" ];
      L [ K "param"; K "i32" ];
      L [ K "result"; K "i32" ]
    ];
    "env", "wasicaml_get_global_data",
    [ L [ K "result"; K "i32" ]];
    "env", "wasicaml_get_domain_state",
    [ L [ K "result"; K "i32" ]];
    "env", "wasicaml_get_atom_table",
    [ L [ K "result"; K "i32" ]];
    "env", "debug2",
    [ L [ K "param"; K "i32" ];
      L [ K "param"; K "i32" ];
    ];
    "wasicaml", "wasicaml_wrapthrow",
    [];
  ]

let sanitize =
  String.map
    (fun c ->
      match c with
        | 'A' .. 'Z' | 'a' .. 'z' | '0' .. '9' | '_' | '.' -> c
        | _ -> '?'
    )

let bigarray_to_string_list limit ba =
  let n = Array1.dim ba in
  let l = ref [] in
  let p = ref 0 in
  while !p < n do
    let q = min (n - !p) limit in
    let by = Bytes.create q in
    for i = 0 to q-1 do
      Bytes.set by i ba.{!p + i}
    done;
    l := Bytes.to_string by :: !l;
    p := !p + q
  done;
  List.rev !l

let generate scode exe get_defname globals_table funcprops_table =
  let (funcmapping, subfunctions) = get_funcmapping scode in

  let letrec_name = Hashtbl.create 7 in
  Hashtbl.iter
    (fun _ (letrec_label, _) ->
      match letrec_label with
        | Main 0 ->
            Hashtbl.add letrec_name letrec_label "letrec_main"
        | Main lab ->
            Hashtbl.add letrec_name letrec_label (sprintf "letrec_main%d" lab)
        | Func lab ->
            let suffix =
              try
                let defname = get_defname lab in
                "_" ^ sanitize defname
              with
                | Not_found -> "" in
            Hashtbl.add letrec_name letrec_label (sprintf "letrec%d%s" lab suffix)
    )
    funcmapping;

  let wasmindex = Hashtbl.create 7 in
  (* Need 'elements' this only for PIC: *)
  let nextindex = ref 0 in
  let _elements =
    Hashtbl.fold
      (fun _ (letrec_label, _) acc ->
        Hashtbl.add wasmindex letrec_label !nextindex;
        incr nextindex;
        let name = Hashtbl.find letrec_name letrec_label in
        (ID name) :: acc
      )
      funcmapping
      [] in
  let data =
    bigarray_to_string_list 4096 Wc_reader.(exe.data)
    |> List.map (fun s -> S s) in

  let gpad =
    { funcmapping;
      subfunctions;
      primitives = Hashtbl.create 7;
      wasmindex;
      letrec_name;
      need_reinit_frame = false;
      need_reinit_frame_k = ISet.empty;
      need_mlookup = false;
      globals_table;
      glbfun_table = Wc_traceglobals.derive_glbfun_table globals_table;
      funcprops_table;
    } in

  let sexpl_code =
    Hashtbl.fold
      (fun letrec_label _ acc ->
        let sexpl = generate_letrec scode gpad letrec_label in
        sexpl @ acc
      )
      gpad.subfunctions
      [] in

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
                S "env";
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
      (globals()) in
  let sexpl_tags =
    if !enable_exceptions then
      [ L [ K "import";
            S "wasicaml";
            S "ocaml_exception";
            L ( [ K "tag";
                  ID "ocaml_exception"
                ]
              )
          ]
      ]
    else
      [] in

  sexpl_memory
  @ sexpl_functions
  @ sexpl_table
  @ sexpl_globals
  @ sexpl_tags
  @ wasicaml_init
  @ wasicaml_get_data
  @ wasicaml_get_data_size Wc_reader.(Array1.dim exe.data)
  @ pick_div_by_zero
  @ ret_div_by_zero
  @ pick_stack_overflow
  @ ret_stack_overflow
  @ alloc_fast
  @ alloc_slow()
  @ wrap_alloc_small_dispatch()
  @ grab_helper gpad
  @ return_helper()
  @ appterm_helper()
  @ restart_helper gpad
  @ (if gpad.need_reinit_frame then
       reinit_frame
     else
       []
    )
  @ (if gpad.need_mlookup then
       mlookup
     else
       []
    )
  @ (if !enable_deadbeef_check then
       deadbeef_init @ deadbeef_check
     else
       []
    )
  @ (ISet.elements gpad.need_reinit_frame_k
     |> List.map reinit_frame_k
     |> List.flatten
    )
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
  @ [ L ( [ K "data";
            L [ K "memory"; N (I32 0l) ];
            (* PIC: L [ K "offset"; K "global.get"; ID "__memory_base" ]; *)
            (* WAT extension: *)
            ID "data";
          ]
          @ data
        )
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
