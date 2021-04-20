open Printf
open Wc_types
open Wc_number
open Wc_sexp

(* TODO:
   - create global wasicaml_global_data and initialize it with &caml_global_data
     (needs a helper function in C to get the address, and generated init
     code to set the global)
   - wasicaml_domain_state = caml_domain_state
   - wasicaml_builtin_cprim = caml_builtin_cprim
   - wasicaml_atom_table = caml_atom_table
   - wasicaml_functions

   - init code
   - debug print per block
   - initialize fp, accu
   - declare local variables from lpad
   - generate wrapper for caml_alloc_small_dispatch:
     pass args: accu, env, fp, framesize, blocksize
     return: new ptr
     pass back via special global: accu, env
   - recognize backward jumps and use loop label
   - exceptions
   - do a stack check at the beginning of a function (but no realloc)
   - use helper functions for Kapply?
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

type wasm_func_type =
  | TFunc of { args: wasm_value_type list; results: wasm_value_type list }

type gpad =  (* global pad *)
  { (* functypes : (wasm_func_type, int) Hashtbl.t;
       functions : (string, func) Hashtbl.t;
       globals : (string, global) Hashtbl.t;
     *)
    primitives : (string, int) Hashtbl.t;
    funcmapping : (int, int * int) Hashtbl.t;
    (* maps function label to (letrec_label, subfunction_id) *)
    subfunctions : (int, int list) Hashtbl.t;
    (* maps letrec_label to list of subfunction labels *)
  }

 and func =
   { func_type : wasm_func_type;
     func_def : entity;
   }

 and global =
   { global_mut : bool;
     global_type : wasm_value_type;
     global_def : entity
   }

 and entity =
   | Imported of string   (* module name *)
   | Defined of sexp

type lpad =  (* local pad *)
  { locals : (string, wasm_value_type) Hashtbl.t;
    mutable loops : ISet.t;
  }

type repr =
  | RValue
    (* a pointer to an OCaml block, or an OCaml integer when LSB=1 *)
  | RInt
    (* an I32 integer that needs to be converted to an OCaml integer *)
  | RNatInt
    (* an I32 integer that will be stored as nativeint block *)
  | RInt32
    (* an I32 integer that will be stored as int32 block *)
  | RInt64
    (* an I64 integer that will be stored as int64 block *)
  | RFloat
    (* an F64 number that will be stored as float block *)

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


type store =
  | RealStack of int
    (* it's on the real OCaml stack at the given position, i.e. at fp[pos] *)
  | RealAccu
    (* stored in the accu variable as RValue *)
  | Const of int
    (* it's a constant *)
  | Local of repr * string
    (* stored in a local variable with the given name.
       There cannot be heap-allocated values in local variables, i.e.
       repr=RValue is forbidden.
     *)
  | Invalid

type state =
  { camlstack : store list;
    (* where the contents are stored that are supposed to be on the
       OCaml stack - in top to bottom order.
       If an OCaml stack value is stored in the real stack, it is only
       allowed to store it at its original position - in other words,
       swapping of values is not allowed. Formally, if element k is
       set to store=RealStack(pos), then pos=-camldepth+k.
     *)
    camldepth : int;
    (* = List.length camlstack *)
    realstack : ISet.t;
    (* which real stack positions have been initialized *)
    accu : store;
    (* where the accu is stored *)
    realaccu : ISet.t;
    (* in which stack positions the "accu" variable must be saved on flush.
       If additionally accu=RealAccu, the "accu" variable also keeps the
       logical accu value.
     *)
  }

let enable_multireturn = ref false
(* whether Wasm code can use multivalue returns *)

let code_pointer_shift = 11
  (* OCaml code pointers:
      - Bit 0: whether to run RESTART
      - Bit 1 - code_pointer_shift-1: subfunction of the letrec
      - Bit code_pointer_shift-31: the Wasm function index
   *)
let code_pointer_mask = 0x7fe

(* TODO: grab the following values from C: *)
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

let caml_from_c = 0

let rec enum k n =
  if n > 0 then
    k :: enum (k+1) (n-1)
  else
    []

let repr_of_store =
  function
  | RealStack _ -> RValue
  | Const _ -> RInt
  | Local(repr, _) -> repr
  | RealAccu -> RValue
  | Invalid -> RValue (* FIXME: assert false *)

let vtype repr =
  match repr with
    | RValue | RInt | RNatInt | RInt32 ->
        TI32
    | RInt64 ->
        TI64
    | RFloat ->
        TF64

let new_local lpad vtype =
  let k = Hashtbl.length lpad.locals in
  let s = sprintf "x%d" k in
  Hashtbl.add lpad.locals s vtype;
  s

let empty_state =
  { camlstack = [];
    camldepth = 0;
    realstack = ISet.empty;
    accu = RealAccu;
    realaccu = ISet.empty;
  }

let empty_lpad() =
  { locals = Hashtbl.create 7;
    loops = ISet.empty;
  }

let realdepth state =
  (* how many stack positions are really used, counted from the bottom? *)
  match ISet.min_elt_opt state.realstack with
    | None ->
        0
    | Some pos ->
        assert(pos < 0);
        -pos

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

let push_field var_base field =
  [ L [ K "local.get";
        ID var_base;
      ];
    L [ K "i32.load";
        K (sprintf "offset=0x%lx" (Int32.of_int (4 * field)));
        K "align=2";
      ];
  ]

let push_global_field var_base field =
  [ L [ K "global.get";
        ID var_base;
      ];
    L [ K "i32.load";
        K (sprintf "offset=0x%lx" (Int32.of_int (4 * field)));
        K "align=2";
      ];
  ]

let push_field_addr var_base field =
  [ L [ K "local.get";
        ID var_base;
      ];
    L [ K "i32.const";
        N (I32 (Int32.of_int (4 * field)));
      ];
    L [ K "i32.add" ]
  ]

let push_global_field_addr var_base field =
  [ L [ K "global.get";
        ID var_base;
      ];
    L [ K "i32.const";
        N (I32 (Int32.of_int (4 * field)));
      ];
    L [ K "i32.add" ]
  ]

let push_stack pos =
  push_field "fp" pos

let pop_to_field var_base field =
  [ L [ K "local.get";
        ID var_base;
      ];
    L [ K "i32.store";
        K (sprintf "offset=0x%lx" (Int32.of_int (4 * field)));
        K "align=2";
      ];
  ]

let pop_to_double_field var_base field =
  [ L [ K "local.get";
        ID var_base;
      ];
    L [ K "f64.store";
        K (sprintf "offset=0x%lx" (Int32.of_int (4 * double_size * field)));
        K "align=2";
      ];
  ]

let pop_to_global_field var_base field =
  [ L [ K "global.get";
        ID var_base;
      ];
    L [ K "i32.store";
        K (sprintf "offset=0x%lx" (Int32.of_int (4 * field)));
        K "align=2";
      ];
  ]

let pop_to_stack pos =
  pop_to_field "fp" pos

let load_double =
  [ L [ K "f64.load";
        K "align=2"
      ]
  ]

let stack_init gpad lpad state realdepth =
  (* put zeros into the uninitialized stack positions *)
  let rec gen_init k =
    if k >= 1 then
      let pos = -k in
      let is_used = ISet.mem pos state.realstack in
      ( if not is_used then
          push_const 1l @ pop_to_stack pos
        else
          []
      ) @ gen_init (k-1)
    else
      [] in
  gen_init realdepth

let whether_accu_contains_value state =
  state.realaccu <> ISet.empty || state.accu = RealAccu

let setup_for_gc gpad lpad state realdepth =
  let accu_contains_value = whether_accu_contains_value state in
  let sp_decr =
    if accu_contains_value then 2 else 1 in
  let sexpl_stack =
    stack_init gpad lpad state realdepth in
  let sexpl_accu =
    if accu_contains_value then
      push_local "accu" @ pop_to_stack (-realdepth-1)
    else
      [] in
  let sexpl_env =
    push_local "env" @ pop_to_stack (-realdepth-sp_decr) in
  let sexpl_extern_sp =
    [ L [ K "local.get";
          ID "fp";
        ];
      L [ K "i32.const";
          N (I32 (Int32.of_int ( 4 * (realdepth + sp_decr))));
        ];
      L [ K "i32.sub" ];
    ]
    @ pop_to_global_field "wasicaml_domain_state" domain_field_extern_sp in
  sexpl_stack @ sexpl_accu @ sexpl_env @ sexpl_extern_sp

let restore_after_gc gpad lpad state realdepth =
  let accu_contains_value = whether_accu_contains_value state in
  let sp_decr =
    if accu_contains_value then 2 else 1 in
  let sexpl_accu =
    if accu_contains_value then
      push_stack (-realdepth-1) @ pop_to_local "accu"
    else
      [] in
  let sexpl_env =
    push_stack (-realdepth-sp_decr) @ pop_to_local "env" in
  sexpl_accu @ sexpl_env

let setup_for_c_call gpad lpad state extra realdepth =
  let sexpl_stack =
    stack_init gpad lpad state realdepth in
  let sexpl_env =
    push_local "env" @ pop_to_stack (-realdepth-1-extra) in
  let sexpl_extern_sp =
    [ L [ K "local.get";
          ID "fp";
        ];
      L [ K "i32.const";
          N (I32 (Int32.of_int ( 4 * (realdepth + extra + 1))));
        ];
      L [ K "i32.sub" ];
    ]
    @ pop_to_global_field "wasicaml_domain_state" domain_field_extern_sp in
  sexpl_stack @ sexpl_env @ sexpl_extern_sp

let restore_after_c_call gpad lpad state extra realdepth =
  push_stack (-realdepth-1-extra) @ pop_to_local "env"

let alloc_atom gpad lpad state tag =
  let ptr = new_local lpad TI32 in
  let code =
    [ L [ K "global.get";
          ID "wasicaml_atom_table";
        ];
      L [ K "i32.const";
          N (I32 (Int32.of_int (4 * tag)));
        ];
      L [ K "i32.add" ];
      L [ K "local.set";
          ID ptr
        ]
    ] in
  (code, ptr, false)

let alloc_non_atom gpad lpad state size tag =
  (* TODO: use new push/pop functions *)
  let ptr = new_local lpad TI32 in
  let young = size <= max_young_wosize in
  let code =
    if young then
      let rdepth = realdepth state in
      [
        (* ptr = Caml_state_field(young_ptr) - Whsize_wosize (wosize) *)
        L [ K "global.get";
            ID "wasicaml_domain_state";
          ];
        L [ K "i32.load";
            K (sprintf "offset=0x%lx" (Int32.of_int (4 * domain_field_young_ptr)));
            K "align=2"
          ];
        L [ K "i32.const";
            N (I32 (Int32.of_int (4 * (size+1))));
          ];
        L [ K "i32.sub" ];
        L [ K "local.tee";
            ID ptr;
          ];

        (* if (ptr < Caml_state_field(young_limit)) *)
        L [ K "global.get";
            ID "wasicaml_domain_state";
          ];
        L [ K "i32.load";
            K (sprintf "offset=0x%lx" (Int32.of_int (4 * domain_field_young_limit)));
            K "align=2"
          ];
        L [ K "i32.lt_u" ];
        L [ K "if";
            L ( [ [
                    K "then";
                  ];
                  setup_for_gc gpad lpad state rdepth;

                  (* caml_alloc_small_dispatch(size, CAML_FROM_C, 1, NULL); *)
                  [ L [ K "i32.const";
                        N (I32 (Int32.of_int size));
                      ];
                    L [ K "i32.const";
                        N (I32 (Int32.of_int caml_from_c));
                      ];
                    L [ K "i32.const";
                        N (I32 1l);
                      ];
                    L [ K "i32.const";
                        N (I32 0l);
                      ];
                    L [ K "call";
                        ID "caml_alloc_small_dispatch";
                      ];
                  ];
                  restore_after_gc gpad lpad state rdepth;

                  (* ptr = Caml_state_field(young_ptr) *)
                  [ L [ K "global.get";
                        ID "wasicaml_domain_state";
                      ];
                    L [ K "i32.load";
                        K (sprintf "offset=0x%lx" (Int32.of_int (4 * domain_field_young_ptr)));
                        K "align=2"
                      ];
                    L [ K "local.set";
                        ID ptr
                      ];
                  ]
                ] |> List.flatten
              );
            L [
                K "else";
                (* Caml_state_field(young_ptr) = ptr *)
                L [ K "local.get";
                    ID ptr
                  ];
                L [ K "global.get";
                    ID "wasicaml_domain_state";
                  ];
                L [ K "i32.store";
                    K (sprintf "offset=0x%lx" (Int32.of_int (4 * domain_field_young_ptr)));
                    K "align=2"
                  ];
              ];
          ];

        L [ K "local.get";
            ID ptr
          ];
        L [ K "i32.const";
            N (I32 (Int32.logor
                      (Int32.shift_left (Int32.of_int size) 10)
                      (Int32.of_int tag)));
          ];
        L [ K "i32.store";
            K (sprintf "offset=0x%lx" (-4l));
            K "align=2"
          ];
      ]
    else
      [ L [ K "i32.const";
            N (I32 (Int32.of_int size));
          ];
        L [ K "i32.const";
            N (I32 (Int32.of_int tag));
          ];
        L [ K "call";
            ID "caml_alloc_shr"
          ]
      ] in
  (code, ptr, young)

let alloc gpad lpad state size tag =
  if size = 0 then
    alloc_atom gpad lpad state tag
  else
    alloc_non_atom gpad lpad state size tag

let grab_helper gpad =
  (* generates a helper function:
     $grab_helper(env, extra_args, codeptr, fp)
   *)
  let state = { empty_state with accu = Invalid } in
  let lpad = empty_lpad() in

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
          ];

          setup_for_gc gpad lpad state 0;
          [ L [ K "local.get"; ID "extra_args" ];
            L [ K "i32.const"; N (I32 4l) ];
            L [ K "i32.add" ];
            L [ K "i32.const"; N (I32 (Int32.of_int closure_tag))];
            L [ K "call"; ID "caml_alloc_small" ];
            L [ K "local.set"; ID "accu" ];
          ];
          restore_after_gc gpad lpad state 0;  (* won't overwrite accu *)

          push_local "env";
          pop_to_field "accu" 2;

          push_const 0l;
          pop_to_local "i";

          [ L [ K "loop"; ID "fields"; BR;
                (* fp[i] *)
                L [ K "local.get"; ID "fp" ];
                L [ K "local.get"; ID "i" ];
                L [ K "i32.const"; N (I32 2l) ];
                L [ K "i32.shl" ];
                L [ K "i32.add" ];
                L [ K "i32.load"; K "align=2" ];

                (* Field(accu, i+3) = ... *)
                L [ K "local.get"; ID "accu" ];
                L [ K "local.get"; ID "i" ];
                L [ K "i32.const"; N (I32 3l) ];
                L [ K "i32.add" ];
                L [ K "i32.const"; N (I32 2l) ];
                L [ K "i32.shl" ];
                L [ K "i32.add" ];
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
            L [ K "i32.load"; K (sprintf "offset=0x%lx" (-4l)); K "align=2" ];
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
                (* Field(env, i+3) *)
                L [ K "local.get"; ID "env" ];
                L [ K "local.get"; ID "i" ];
                L [ K "i32.const"; N (I32 3l) ];
                L [ K "i32.add" ];
                L [ K "i32.const"; N (I32 2l) ];
                L [ K "i32.shl" ];
                L [ K "i32.add" ];
                L [ K "i32.load"; K "align=2" ];

                (* fp[i[ = ... *)
                L [ K "local.get"; ID "fp" ];
                L [ K "local.get"; ID "i" ];
                L [ K "i32.const"; N (I32 2l) ];
                L [ K "i32.shl" ];
                L [ K "i32.add" ];
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

let tovalue repr =
  (* transform the value of the Wasm stack to a proper OCaml value,
     and put that back on the Wasm stack *)
  match repr with
    | RValue ->
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
    | _ ->
        (* TODO: need to allocate the block *)
        assert false

let toint repr =
  match repr with
    | RInt ->
        []
    | RValue ->
        [ L [ K "i32.const";
              N (I32 1l);
            ];
          L [ K "i32.shr_s" ];
        ]
    | _ ->
        assert false

let convert repr_from repr_to =
  (* convert the value on the wasm stack, from repr_from to repr_to *)
  match repr_to with
    | RValue -> tovalue repr_from
    | RInt -> toint repr_from
    | _ ->
        assert false (* TODO *)

let set_camlstack pos store state =
  let cd = state.camldepth in
  if pos >= (-cd) && pos <= (-1) then
    let camlstack =
      List.mapi
        (fun i old_store ->
          if (-cd+i) = pos then store else old_store
        )
        state.camlstack in
    camlstack
  else
    state.camlstack

let pop_camlstack state =
  let cd = state.camldepth in
  let cpos = (-cd) in
  match state.camlstack with
    | RealStack pos :: tl ->
        { state with
          camlstack = tl;
          camldepth = cd - 1;
          realstack = ISet.remove pos state.realstack;
          realaccu = ISet.remove cpos state.realaccu;
        }
    | RealAccu :: tl ->
        { state with
          camlstack = tl;
          camldepth = cd - 1;
          realaccu = ISet.remove cpos state.realaccu;
        }
    | (Const _ | Local _ | Invalid) :: tl ->
        { state with camlstack = tl; camldepth = cd - 1 }
    | [] ->
        assert false

let rec popn_camlstack state number =
  if number = 0 then
    state
  else
    popn_camlstack (pop_camlstack state) (number-1)

let push_camlstack store state =
  let cd = state.camldepth in
  let cpos = (-cd-1) in
  match store with
    | RealStack pos ->
        { state with
          camlstack = store :: state.camlstack;
          camldepth = cd + 1;
          realstack = ISet.add pos state.realstack;
        }
    | RealAccu ->
        { state with
          camlstack = store :: state.camlstack;
          camldepth = cd + 1;
          realaccu = ISet.add cpos state.realaccu;
        }
    | (Const _ | Local _ | Invalid) ->
        { state with
          camlstack = store :: state.camlstack;
          camldepth = cd + 1;
        }

let flush_accu gpad lpad state =
  (* If the accu is not yet saved, do this now. This function needs to be called
     before setting the accu to a new value. *)
  let sexpll =
    ISet.fold
      (fun pos sexpl_acc ->
        let sexpl = push_local "accu" @ pop_to_stack pos in
        sexpl :: sexpl_acc
      )
      state.realaccu
      [] in
  let sexpl =
    List.rev sexpll |> List.flatten in
  let camlstack =
    List.mapi
      (fun i old ->
        let pos = (-state.camldepth+i) in
        if ISet.mem pos state.realaccu then
          RealStack pos
        else
          old
      )
      state.camlstack in
  let state =
    { state with
      camlstack;
      realaccu = ISet.empty;
      realstack = ISet.union state.realstack state.realaccu;
    } in
  (state, sexpl)

let push gpad lpad state store =
  (* put the value in store onto the wasm stack *)
  match store with
    | RealAccu ->
        push_local "accu"
    | Local(repr, name) ->
        push_local name
    | Const x ->
        push_const (Int32.of_int x)
    | RealStack pos ->
        push_stack pos
    | Invalid ->
        (* FIXME: assert false *)
        push_local "invalid"

let push_as gpad lpad state store req_repr =
  match store, req_repr with
    | Const x, RValue ->
        push_const (Int32.logor (Int32.shift_left (Int32.of_int x) 1) 1l)
    | _ ->
        let sexpl_push = push gpad lpad state store in
        let repr = repr_of_store store in
        sexpl_push @ convert repr req_repr

let straighten_accu gpad lpad state =
  (* if the accu is stored elsewhere, load it now into "accu". This function
     needs to be called before doing something with the value in the accu
   *)
  let state, sexpl_flush = flush_accu gpad lpad state in
  match state.accu with
    | RealAccu ->
        (state, [])
    | Local _ | Const _ ->
        let sexpl_push = push_as gpad lpad state state.accu RValue in
        let state = { state with accu = RealAccu } in
        let sexpl_pop = pop_to_local "accu" in
        (state, sexpl_flush @ sexpl_push @ sexpl_pop)
    | RealStack pos ->
        let sexpl_push = push_as gpad lpad state state.accu RValue in
        let state = { state with accu = RealAccu } in
        let sexpl_pop = pop_to_local "accu" in
        (state, sexpl_flush @ sexpl_push @ sexpl_pop)
    | Invalid ->
        (* FIXME: assert false *)
        (state, [])

let straighten_accu_when_on_stack gpad lpad state =
  (* Load the accu if it is stored in the real stack. This is needed before
     renaming the accu into another stack position, in order to enforece
     the "no swapping of stack positions" rule.
   *)
  match state.accu with
    | RealStack _ ->
        straighten_accu gpad lpad state
    | _ ->
        (state, [])

let flush_real_stack gpad lpad state pos_from pos_to =
  (* Ensure that values stored at pos_from...pos_to are saved to their
     real stores, so that the real stack at pos_from...pos_to can be
     overwritten.

     Essentially, this can only be the accu (because of the "no swapping
     of stack positions" rule).
   *)
  match state.accu with
    | RealStack pos when pos >= pos_from && pos <= pos_to ->
        straighten_accu gpad lpad state
    | _ ->
        (state, [])

let straighten_stack_at gpad lpad state pos =
  (* ensure that the caml stack for pos is set to the real stack *)
  let state, sexpl_flush = flush_real_stack gpad lpad state pos pos in
  let k = pos + state.camldepth in
  let store = List.nth state.camlstack k in
  let sexpl_push = push_as gpad lpad state store RValue in
  let sexpl_pop = pop_to_stack pos in
  let state =
    { state with
      camlstack = set_camlstack pos (RealStack pos) state;
      realstack = ISet.add pos state.realstack;
    } in
  let sexpl =
    sexpl_flush @ sexpl_push @ sexpl_pop in
  (state, sexpl)

let straighten_all gpad lpad state =
  let rec recurse state camlstack pos =
    match camlstack with
      | RealStack p :: camlstack' ->
          assert(p = pos);
          recurse state camlstack' (pos+1)
      | store :: camlstack' ->
          let state, sexpl1 = straighten_stack_at gpad lpad state pos in
          let state, sexpl2 = recurse state camlstack' (pos+1) in
          (state, sexpl1 @ sexpl2)
      | [] ->
          (state, []) in
  let state, sexpl_accu = straighten_accu gpad lpad state in
  let state, sexpl_stack = recurse state state.camlstack (-state.camldepth) in
  (state, sexpl_accu @ sexpl_stack)

let rec drop n l =
  if n > 0 then
    match l with
      | _ :: l -> drop (n-1) l
      | [] -> []
  else
    l

let nullary_operation gpad lpad state op_repr op_sexpl =
  match op_repr with
    | RValue ->
        let state, sexpl_flush = flush_accu gpad lpad state in
        let state = { state with accu = RealAccu } in
        let sexpl =
          sexpl_flush @ op_sexpl @ pop_to_local "accu" in
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
  let result = new_local lpad (vtype op_repr) in
  let state = { state with accu = Local(op_repr, result) } in
  let sexpl_pop = pop_to_local result in
  (state, sexpl_push @ op_sexpl @ sexpl_pop)

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
  let sexpl_push1 = push_as gpad lpad state state.accu op_repr in
  let second = List.hd state.camlstack in
  let sexpl_push2 = push_as gpad lpad state second op_repr in
  let result = new_local lpad (vtype op_repr) in
  let state = pop_camlstack state in
  let state = { state with accu = Local(op_repr, result) } in
  let sexpl_pop = pop_to_local result in
  (state, sexpl_push1 @ sexpl_push2 @ op_sexpl @ sexpl_pop)

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

let makeblock gpad lpad state size tag =
  let sexpl_alloc, ptr, young = alloc gpad lpad state size tag in
  let fields = enum 0 size in
  let camlstack = Array.of_list state.camlstack in
  let store_of_field field =
    if field=0 then state.accu else camlstack.(field-1) in
  let sexpl_init =
    if young then
      List.map
        (fun field ->
          let store = store_of_field field in
          push gpad lpad state store
          @ pop_to_field ptr field
        )
        fields
      |> List.flatten
    else
      List.map
        (fun field ->
          let store = store_of_field field in
          push_field_addr ptr field
          @ push gpad lpad state store
          @ [ L [ K "call";
                  ID "caml_initialize"
                ]
            ]
        )
        fields
      |> List.flatten in
  let state = if size > 0 then popn_camlstack state (size-1) else state in
  let state, sexpl_flush = flush_accu gpad lpad state in
  let state = { state with accu = RealAccu } in
  let sexpl_set = push_local ptr @ pop_to_local "accu" in
  (state, sexpl_alloc @ sexpl_init @ sexpl_flush @ sexpl_set)

let makefloatblock gpad lpad state size =
  let wosize = size * double_size in
  let sexpl_alloc, ptr, _ = alloc gpad lpad state wosize double_array_tag in
  let fields = enum 0 size in
  let camlstack = Array.of_list state.camlstack in
  let store_of_field field =
    if field=0 then state.accu else camlstack.(field-1) in
  let sexpl_init =
    List.map
      (fun field ->
        let store = store_of_field field in
        push gpad lpad state store
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
  let prim =
    try Hashtbl.find gpad.primitives name
    with Not_found -> assert false in
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
            push gpad lpad state store
          )
          args
        |> List.flatten in
      let p_i32 = L [ K "param"; K "i32" ] in
      let r_i32 = L [ K "result"; K "i32" ] in
      let ty = (args |> List.map (fun _ -> p_i32)) @ [ r_i32 ] in
      ( state,
        sexpl_flush
        @ sexpl_setup
        @ sexpl_args
        @ push_global_field "wasicaml_builtin_cprim" prim
        @ [ L ( [ K "call_indirect";
                  N (I32 0l);   (* table index *)
                ] @ ty);
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
        push gpad lpad state state.accu
        @ pop_to_stack (-state.camldepth-1) in
      ( state,
        sexpl_flush
        @ sexpl_stack_fixup
        @ sexpl_accu
        @ sexpl_setup
        @ push_field_addr "fp" (-state.camldepth-1)
        @ push_const (Int32.of_int num_args)
        @ push_global_field "wasicaml_builtin_cprim" prim
        @ [ L [ K "call_indirect";
                N (I32 0l);     (* table index *)
                L [ K "param"; K "i32" ];
                L [ K "param"; K "i32" ];
                L [ K "result"; K "i32" ];
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
  let value = new_local lpad TI32 in
  push gpad lpad state state.accu
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

              L [ K "local.get";
                  ID value
                ];
              L [ K "i32.load";
                  K (sprintf "offset=0x%lx" (-4l));
                  K "align=2"
                ];
              L [ K "i32.const";
                  N (I32 0xffl);
                ];
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
              L [ K "return" ];
            ]
        ];
    ] in
  (state, sexpl)

let return =
  [ L [ K "local.get"; ID "extra_args" ];
    L [ K "if";
        L ( [ K "then";
              L [ K "local.get"; ID "accu" ];
              L [ K "local.get"; ID "extra_args" ];
              L [ K "i32.const"; N (I32 1l) ];
              L [ K "i32.sub" ];
            ]
            @ push_field "accu" 0
            @ [ L [ K "local.tee"; ID "codeptr" ];
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
    L [ K "return" ];
  ]

let apply_code extra_args sp_decr env_pos =
  [ L [ K "local.get"; ID "accu" ];
    L [ K "i32.const"; N (I32 (Int32.of_int extra_args)) ];
  ]
  @ push_field "accu" 0
  @ [ L [ K "local.tee"; ID "accu" ];
      L [ K "local.get"; ID "fp" ];
      L [ K "i32.const"; N (I32 (Int32.of_int (4 * sp_decr))) ];
      L [ K "i32.sub" ];
      L [ K "local.get"; ID "accu" ];
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
      L [ K "local.set"; ID "accu" ];
    ]
  @ push_stack env_pos   (* env might have been moved by GC *)
  @ pop_to_local "env"

let apply123 gpad lpad state num =
  let state, sexpl_str = straighten_all gpad lpad state in
  let sexpl_move =
    enum 0 num
    |> List.map
         (fun k ->
           push_stack (-state.camldepth+k)
           @ pop_to_stack (-state.camldepth+k-3)
         )
    |> List.flatten in
  let sexpl_frame =
    push_const 1l   (* instead of pc *)
    @ pop_to_stack (-state.camldepth+num-3)
    @ push_local "env"
    @ pop_to_stack (-state.camldepth+num-2)
    @ push_const 1l  (* instead of extra_args *)
    @ pop_to_stack (-state.camldepth+num-1) in
  let sexpl_call =
    apply_code (num-1) (state.camldepth+3) (-state.camldepth+num-2) in
  let sexpl = sexpl_move @ sexpl_frame @ sexpl_call in
  let state = popn_camlstack state num in
  (state, sexpl)

let apply4plus gpad lpad state num =
  let state, sexpl_str = straighten_all gpad lpad state in
  let sexpl =
    apply_code (num-1) state.camldepth (-state.camldepth + num + 1) in
  let state = popn_camlstack state (num+3) in
  (state, sexpl)

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
        let state = { state with camlstack;
                                 realaccu = ISet.add (-cd+sp) state.realaccu
                    } in
        (state, sexpl)
    | Kenvacc field ->
        let sexpl = push_field "env" field in
        nullary_operation gpad lpad state RValue sexpl
    | Kgetglobal ident ->
        let offset = global_offset ident in
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
        let negate_sexpl =  (* could also multiply by (-1) *)
          [ L [ K "i32.const";
                N (I32 (-1l));
              ];
            L [ K "i32.xor" ];
            L [ K "i32.const";
                N (I32 1l);
              ];
            L [ K "i32.add" ];
          ] in
        unary_operation gpad lpad state RInt negate_sexpl
    | Kboolnot ->
        let not_sexpl =
          [ L [ K "i32.const";
                N (I32 1l);
              ];
            L [ K "i32.xor" ]
          ] in
        unary_operation gpad lpad state RInt not_sexpl
    | Koffsetint offset ->
        let offset_sexpl =
          [ L [ K "i32.const";
                N (I32 (Int32.of_int offset));
              ];
            L [ K "i32.add" ]
          ] in
        unary_operation gpad lpad state RInt offset_sexpl
    | Koffsetref offset ->
        let local = new_local lpad TI32 in
        let offset_sexpl =
          [ L [ K "local.tee";
                ID local
              ];
            L [ K "local.get";
                ID local
              ];
            L [ K "i32.load";
                K "align=2"
              ];
            L [ K "i32.const";
                N (I32 (Int32.shift_left (Int32.of_int offset) 1));
              ];
            L [ K "i32.add" ];
            L [ K "i32.store";
                K "align=2"
              ];
          ] in
        unary_operation_rvalue_unit gpad lpad state offset_sexpl
    | Kisint ->
        let isint_sexpl =
          [ L [ K "i32.const";
                N (I32 1l);
              ];
            L [ K "i32.and" ];
            L [ K "i32.const";
                N (I32 1l);
              ];
            L [ K "i32.shl" ];
            L [ K "i32.const";
                N (I32 1l);
              ];
            L [ K "i32.or" ]
          ] in
        unary_operation gpad lpad state RValue isint_sexpl
    | Ksetglobal ident ->
        let offset = global_offset ident in
        let local = new_local lpad TI32 in
        let sexpl =
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
        let sexpl =
          [ L [ K "i32.load";
                K (sprintf "offset=0x%lx" (Int32.of_int (4 * field)));
                K "align=2";
              ];
          ] in
        unary_operation gpad lpad state RValue sexpl
    | Kgetfloatfield field ->
        let (sexpl_alloc, ptr, _) =
          alloc gpad lpad state double_size double_tag in
        let sexpl =
          sexpl_alloc
          @ [ L [ K "f64.load";
                  K (sprintf "offset=0x%lx" (Int32.of_int (4 * double_size * field)));
                  K "align=2";
                ];
              L [ K "local.get";
                  ID ptr
                ];
              L [ K "f64.store";
                  K "align=2"
                ];
            ] in
        unary_operation gpad lpad state RValue sexpl
    | Kvectlength ->
        let local = new_local lpad TI32 in
        let sexpl =
          [ L [ K "i32.load";
                K (sprintf "offset=0x%lx" (-4l));
              ];
            L [ K "local.tee";
                ID local
              ];
            L [ K "local.get";
                ID local
              ];
            L [ K "i32.const";
                N (I32 0xffl);
              ];
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
        let add_sexpl = [ L [ K "i32.add" ] ] in
        binary_operation gpad lpad state RInt add_sexpl
    | Ksubint ->
        let sub_sexpl = [ L [ K "i32.sub" ] ] in
        binary_operation gpad lpad state RInt sub_sexpl
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
        binary_operation gpad lpad state RInt and_sexpl
    | Korint ->
        let or_sexpl = [ L [ K "i32.or" ] ] in
        binary_operation gpad lpad state RInt or_sexpl
    | Kxorint ->
        let xor_sexpl = [ L [ K "i32.xor" ] ] in
        binary_operation gpad lpad state RInt xor_sexpl
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
        let cmp_sexpl = [ L [ K wasm_op ]] in
        binary_operation gpad lpad state RInt cmp_sexpl
    | Kisout ->
        let cmp_sexpl = [ L [ K "i32.lt_u" ]] in
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
        let sexpl =
          [ L [ K "f64.load";
                K "align=2";
              ];
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
        let local = new_local lpad TI32 in
        let sexpl =
          [ L [ K "local.set";
                ID local;
              ];
            L [ K "i32.const";
                N (I32 1l);
              ];
            L [ K "i32.shr_s" ];
            L [ K "i32.add" ];
            L [ K "local.get";
                ID local
              ];
            L [ K "i32.const";
                N (I32 1l);
              ];
            L [ K "i32.shr_s" ];
            L [ K "i32.store8" ];
            L [ K "i32.const";
                N (I32 1l);
              ];
          ] in
        ternary_operation_rvalue gpad lpad state sexpl
    | Kmakeblock (size, tag) ->
        makeblock gpad lpad state size tag
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
          @ push_as gpad lpad state state.accu RInt
          @ [ L [ K "br_if"; ID (string_label lpad lab) ] ] in
        (state, sexpl)
    | Kbranchifnot lab ->
        let state, sexpl_str = straighten_all gpad lpad state in
        let sexpl =
          sexpl_str
          @ push_as gpad lpad state state.accu RInt
          @ [ L [ K "i32.eqz" ];
              L [ K "br_if"; ID (string_label lpad lab) ] ] in
        (state, sexpl)
    | Kswitch (labls_ints, labls_blocks) ->
        let state, sexpl_str = straighten_all gpad lpad state in
        (state, sexpl_str @ switch gpad lpad state labls_ints labls_blocks)
    | Kpush_retaddr lab ->
        let d = state.camldepth in
        let state = { state with camlstack = RealStack(-d-3) :: RealStack(-d-2) :: RealStack(-d-1) :: state.camlstack;
                                 camldepth = state.camldepth + 3 } in
        (state, [])   (* TODO *)
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
        (state, [])   (* TODO *)
    | Kreturn slots ->
        (state, [ L [ K "br"; ID "return" ] ])
    | Krestart -> assert false
    | Kgrab num ->
        grab gpad lpad state num
    | Kclosure(lab, num) ->
        let k = if num > 0 then num-1 else 0 in
        let letrec_label, subfunc =
          try Hashtbl.find gpad.funcmapping lab
          with Not_found -> assert false in
        let sexpl = [] in
(*
          [ L [ K "i32.const"; ID (sprintf "letrec%d" letrec_label) ];
            L [ K "drop" ];
          ] in
 *)
        (popn_camlstack state k, sexpl)   (* TODO *)
    | Kclosurerec(funcs, num_args) ->
        let num_funcs = List.length funcs in
        let k = if num_args > 0 then num_args-1 else 0 in
        let state = popn_camlstack state k in
        let d = state.camldepth in
        let nstack =
          enum (-d-num_funcs) num_funcs
          |> List.map (fun pos -> RealStack pos) in
        let state = { state with camlstack = nstack @ state.camlstack;
                                 camldepth = state.camldepth + num_funcs
                    } in
        (state, [])   (* TODO *)
    | Koffsetclosure index ->
        (state, [])   (* TODO *)
    | Kpushtrap lab ->
        (state, [])   (* TODO *)
    | Kpoptrap ->
        (state, [])   (* TODO *)
    | Kraise kind ->
        (state, [])   (* TODO *)
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
        (state, [])   (* TODO *)
    | Kstrictbranchif _ -> assert false
    | Kstrictbranchifnot _ -> assert false

let local_branch_labels =
  function
  | I.Kbranch l -> [l]
  | Kbranchif l -> [l]
  | Kbranchifnot l -> [l]
  | Kswitch (la1,la2) -> Array.to_list la1 @ Array.to_list la2
  | _ -> []

let emit_fblock gpad lpad fblock =
  let open Wc_control in
  let depth_table = Hashtbl.create 7 in
  (* maps label to depth of camlstack *)

  let get_state label =
    let camldepth =
      try Hashtbl.find depth_table label
      with Not_found -> 0 in
    let camlstack =
      enum (-camldepth) camldepth
      |> List.map (fun pos -> RealStack pos) in
    let realstack =
      enum (-camldepth) camldepth
      |> List.fold_left
           (fun acc pos -> ISet.add pos acc)
           ISet.empty in
    { camlstack;
      camldepth;
      realstack;
      accu = RealAccu;
      realaccu = ISet.empty
    } in

  let rec emit_block block loops =
    let state = { empty_state with accu = Invalid } in
    (* eprintf "BLOCK\n%!";*)
    let upd_loops =
      match block.loop_label with
        | Some lab -> ISet.add lab loops
        | _ -> loops in
    let state, acc =
      Array.fold_left
        (fun (state, acc) instr ->
          match instr with
            | Label label ->
                (* eprintf "LABEL %d\n" label; *)
                (get_state label, acc)
            | Simple i ->
                let labels = local_branch_labels i in
                List.iter
                  (fun label ->
                    try
                      let d = Hashtbl.find depth_table label in
                      assert(d = state.camldepth)
                    with
                      | Not_found ->
                          Hashtbl.add depth_table label state.camldepth
                  )
                  labels;
                lpad.loops <- upd_loops;
                let (next_state, sexpl) =
                  emit_instr gpad lpad state i in
                (*
                eprintf "%s predepth=%d postdepth=%d\n%!"
                        (Wc_util.string_of_instruction i)
                        (state.camldepth)
                        (next_state.camldepth);
                 *)
                let comment =
                  C (Wc_util.string_of_instruction i) in
                (next_state, (comment :: sexpl) :: acc)
            | Block inner ->
                let sexpl = emit_block inner upd_loops in
                (state, sexpl :: acc)
        )
        (state, [])
        block.instructions in
    let inner_sexpl =
      List.rev acc |> List.flatten in
    match block.loop_label, block.break_label with
      | Some _, Some _ -> assert false
      | Some label, None ->
          [ L ( [ K "loop";
                  ID (sprintf "loop%d" label);
                  BR
                ]
                @ inner_sexpl
              )
          ]
      | None, Some label ->
          [ L ( [ K "block";
                  ID (sprintf "label%d" label);
                  BR
                ]
                @ inner_sexpl
              )
          ]
      | None, None ->
          inner_sexpl in

  emit_block fblock.block ISet.empty

let get_funcmapping scode =
  let open Wc_control in
  let funcmapping = Hashtbl.create 7 in
  let subfunction_num = Hashtbl.create 7 in
  let subfunctions = Hashtbl.create 7 in
  IMap.iter
    (fun func_label fblock ->
      match fblock.scope.cfg_letrec_label with
        | None -> ()
        | Some letrec_label ->
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


let get_primitives exe =
  let open Wc_reader in
  let primitives = Hashtbl.create 7 in
  Array.iteri
    (fun k name ->
      Hashtbl.add primitives name k
    )
    exe.primitives;
  primitives

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

let generate_subfunction gpad lpad letrec_label func_label fblock =
  emit_fblock gpad lpad fblock

let generate_letrec scode gpad letrec_label =
  let lpad =
    { locals = Hashtbl.create 7;
      loops = ISet.empty;
    } in
  Hashtbl.add lpad.locals "accu" TI32;

  let subfunc_labels =
    try Hashtbl.find gpad.subfunctions letrec_label
    with Not_found -> assert false in
  assert(subfunc_labels <> []);

  let subfunc_pairs =
    List.map
      (fun func_label ->
        let fblock = IMap.find func_label Wc_control.(scode.functions) in
        let label = sprintf "func%d" func_label in
        let sexpl =
          generate_subfunction gpad lpad letrec_label func_label fblock in
        (label, sexpl)
      )
      subfunc_labels
    @ [ "panic", [] ] in

  let letrec_body =
    [ L [ K "local.get";
          ID "codeptr"
        ];
      L [ K "i32.const";
          N (I32 (Int32.of_int code_pointer_mask));
        ];
      L [ K "i32.and" ];
      L [ K "i32.const";
          N (I32 1l);
        ];
      L [ K "i32.shr_u" ];
      L ( [ K "br_table" ]
          @ ( List.map (fun (label, _) -> ID label) subfunc_pairs )
        )
    ] in
  let cascade = block_cascade letrec_body subfunc_pairs in
  let outer =
    [ L [ K "block"; ID "return"; BR;
          L ( [ K "loop"; ID "startover"; BR ] @ cascade);
        ]
    ] @ return in
  (* TODO: only generate the "return" label when it is used *)

  let locals =
    Hashtbl.fold (fun name vtype acc -> (name,vtype) :: acc) lpad.locals [] in

  let letrec =
    [ L ( [ K "func";
            ID (sprintf "letrec%d" letrec_label);
            L [ K "param"; ID "env"; K "i32" ];
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
          @ outer
          @ [ L [ K "unreachable" ]]
        )
    ] in
  letrec

let globals =
  [ "wasicaml_global_data", TI32;
    "wasicaml_domain_state", TI32;
    "wasicaml_builtin_cprim", TI32;
    "wasicaml_atom_table", TI32;
  ]
  @ if !enable_multireturn then [] else
      [ "retval2", TI32;
        "retval3", TI32;
      ]

let imp_functions =
  [ "caml_alloc_small_dispatch",
    [ L [ K "param"; K "i32" ];
      L [ K "param"; K "i32" ];
      L [ K "param"; K "i32" ];
      L [ K "param"; K "i32" ];
    ];
    "caml_alloc_small",
    [ L [ K "param"; K "i32" ];
      L [ K "param"; K "i32" ];
      L [ K "result"; K "i32" ]
    ];
    "caml_alloc_shr",
    [ L [ K "param"; K "i32" ];
      L [ K "param"; K "i32" ];
      L [ K "result"; K "i32" ]
    ];
    "caml_initialize",
    [ L [ K "param"; K "i32" ];
      L [ K "param"; K "i32" ];
    ];
    "caml_modify",
    [ L [ K "param"; K "i32" ];
      L [ K "param"; K "i32" ];
    ];
  ]

let generate scode exe =
  let (funcmapping, subfunctions) = get_funcmapping scode in
  let primitives = get_primitives exe in
  let gpad =
    { funcmapping;
      subfunctions;
      primitives;
    } in

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
              ID "table";
              N (I32 (Int32.of_int (Hashtbl.length subfunctions)));
              K "funcref"
            ]
        ]
    ] in
  
  let sexpl_functions =
    List.map
      (fun (name, typeuse) ->
        L [ K "import";
            S "ocaml";
            S name;
            L ( [ K "func";
                  ID name;
                ] @ typeuse
              )
          ]
      )
      imp_functions in

  let sexpl_globals =
    List.map
      (fun (name, vtype) ->
        L ( [ K "global";
              ID name;
              L [ K "mut"; K (string_of_vtype vtype) ];
            ]
            @ zero_expr_of_vtype vtype
          )
      )
      globals in

  sexpl_memory
  @ sexpl_table
  @ sexpl_functions
  @ sexpl_globals
  @ grab_helper gpad
  @ restart_helper gpad
  @ Hashtbl.fold
      (fun letrec_label _ acc ->
        let sexpl = generate_letrec scode gpad letrec_label in
        sexpl @ acc
      )
      gpad.subfunctions
      []
  @ [ L ( [ K "elem";
            L [ K "i32.const"; N (I32 0l) ];
            K "func";
          ]
          @ Hashtbl.fold
              (fun letrec_label _ acc ->
                (ID (sprintf "letrec%d" letrec_label)) :: acc
              )
              gpad.subfunctions
              []
        )
    ]

