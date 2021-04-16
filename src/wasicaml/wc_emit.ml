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
 *)

(* OCaml functions are translated to Wasm functions with parameters:
   param 1: env
   param 2: closure
   param 3: code pointer (overriding the one in the closure)
 *)


type wasm_value_type =
  | TI32 | TI64 | TF64

type wasm_func_type =
  | TFunc of { args: wasm_value_type list; results: wasm_value_type list }

type gpad =  (* global pad *)
  { functypes : (wasm_func_type, int) Hashtbl.t;
    functions : (string, func) Hashtbl.t;
    globals : (string, global) Hashtbl.t;
    primitives : (string, int) Hashtbl.t;
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

type log_place =
  | CamlStack of repr * int
  | LogAccu of repr
  | Unused

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
    realstack : log_place IMap.t;
    (* the reverse mapping: which logical value is stored in which position
       of the real OCaml stack. The key of the map is the position.
       Any real stack positions not occurring here are still uninitialized
     *)
    accu : store;
    (* where the accu is stored *)
    realaccu : log_place;
  }

let code_pointer_shift = 11
  (* OCaml code pointers:
      - Bit 0: whether to run RESTART
      - Bit 1 - code_pointer_shift-1: subfunction of the letrec
      - Bit code_pointer_shift-31: the Wasm function index
   *)

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
  | Invalid -> assert false

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

let realdepth state =
  (* how many stack positions are really used, counted from the bottom? *)
  let rec recurse k =
    if k > 0 then
      let pos = -k in
      match IMap.find pos state.realstack with
        | Unused -> recurse (k-1)
        | _ -> k
        | exception Not_found -> recurse (k-1)
    else
      k in
  recurse state.camldepth

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
    if k >= 0 then
      let pos = -k-1 in
      let is_used =
        match IMap.find pos state.realstack with
          | Unused -> false
          | _ -> true
          | exception Not_found -> false in
      ( if not is_used then
          push_const 1l @ pop_to_stack pos
        else
          []
      ) @ gen_init (k-1)
    else
      [] in
  gen_init realdepth

let whether_accu_contains_value state =
  match state.realaccu with
    | CamlStack(RValue, _) -> true
    | LogAccu RValue -> true
    | _ -> false

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
          N (I32 (Int32.of_int (realdepth + sp_decr)));
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
          N (I32 (Int32.of_int (realdepth + extra)));
        ];
      L [ K "i32.sub" ];
    ]
    @ pop_to_global_field "wasicaml_domain_state" domain_field_extern_sp in
  sexpl_stack @ sexpl_env @ sexpl_extern_sp

let restore_after_c_call gpad lpad state extra realdepth =
  push_stack (-realdepth-extra) @ pop_to_local "env"

let alloc gpad lpad state size tag =
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

        L [ K "if";
            (* if (ptr < Caml_state_field(young_limit)) *)
            L [ K "global.get";
                ID "wasicaml_domain_state";
              ];
            L [ K "i32.load";
                K (sprintf "offset=0x%lx" (Int32.of_int (4 * domain_field_young_limit)));
                K "align=2"
              ];
            L [ K "i32.lt_u" ];

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
            K "offset=-4";
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
  match state.camlstack with
    | RealStack pos :: tl ->
        { state with camlstack = tl;
                     camldepth = cd - 1;
                     realstack = IMap.add pos Unused state.realstack
        }
    | RealAccu :: tl ->
        { state with camlstack = tl;
                     camldepth = cd - 1;
                     realaccu = Unused;
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
  let cpos = -cd-1 in
  match store with
    | RealStack pos ->
        { state with
          camlstack = store :: state.camlstack;
          camldepth = cd + 1;
          realstack = IMap.add pos (CamlStack(RValue, cpos)) state.realstack
        }
    | RealAccu ->
        { state with
          camlstack = store :: state.camlstack;
          camldepth = cd + 1;
          realaccu = CamlStack(RValue,cpos);
        }
    | (Const _ | Local _ | Invalid) ->
        { state with
          camlstack = store :: state.camlstack;
          camldepth = cd + 1;
        }

let flush_accu gpad lpad state =
  (* If the accu is not yet saved, do this now. This function needs to be called
     before setting the accu to a new value. *)
  match state.realaccu with
    | CamlStack(repr, pos) as place ->
        assert(repr = RValue);
        let state =
          { state with
            realaccu = Unused;
            camlstack = set_camlstack pos (RealStack pos) state;
            realstack = IMap.add pos place state.realstack
          } in
        let sexpl =
          push_local "accu" @ pop_to_stack pos in
        (state, sexpl)
    | LogAccu _ ->
        ( { state with realaccu = Unused; accu = Invalid }, [] )
    | Unused ->
        (state, [])

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
        assert false

let push_as gpad lpad state store req_repr =
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
        let state = { state with accu = RealAccu; realaccu = LogAccu RValue } in
        let sexpl_push = push_as gpad lpad state state.accu RValue in
        let sexpl_pop = pop_to_local "accu" in
        (state, sexpl_flush @ sexpl_push @ sexpl_pop)
    | RealStack pos ->
        let state = { state with
                      accu = RealAccu;
                      realaccu = LogAccu RValue;
                      realstack = IMap.add pos Unused state.realstack;
                    } in
        let sexpl_push = push_as gpad lpad state state.accu RValue in
        let sexpl_pop = pop_to_local "accu" in
        (state, sexpl_flush @ sexpl_push @ sexpl_pop)
    | Invalid ->
        assert false

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
  let sexpl_push = push gpad lpad state store in
  let sexpl_pop = pop_to_stack pos in
  let state =
    { state with
      camlstack = set_camlstack pos (RealStack pos) state;
      realstack = IMap.add pos (CamlStack(RValue, pos)) state.realstack
    } in
  let sexpl =
    sexpl_flush @ sexpl_push @ sexpl_pop in
  (state, sexpl)

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
        let state = { state with accu = RealAccu;
                                 realaccu = LogAccu RValue;
                    } in
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
  let state, sexpl1 = straighten_accu gpad lpad state in
  let sexpl =
    sexpl1 @ push_local "accu" @ op_sexpl @ pop_to_local "accu" in
  (state, sexpl)

let unary_operation_rvalue_unit gpad lpad state op_sexpl =
  (* takes an RValue arg, and returns Const 0 *)
  let state, sexpl1 = straighten_accu gpad lpad state in
  let state = { state with accu = Const 0;
                           realaccu = Unused;
              } in
  let sexpl =
    sexpl1 @ push_local "accu" @ op_sexpl in
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
  let state = { state with accu = Local(op_repr, result) } in
  let sexpl_pop = pop_to_local result in
  (state, sexpl_push1 @ sexpl_push2 @ op_sexpl @ sexpl_pop)

let binary_operation_rvalue gpad lpad state op_sexpl =
  (* Logically, first operand is in accu, second operand on top of stack.
     For running it, put first operand first on wasm stack, then the
     second operand on top.
   *)
  let second_pos = -state.camldepth in
  let state, sexpl1 = straighten_accu gpad lpad state in
  let state, sexpl2 = straighten_stack_at gpad lpad state second_pos in
  let sexpl3 = push_local "accu" in
  let sexpl4 = push_stack second_pos in
  let result = new_local lpad (vtype RValue) in
  let state =
    { state with
      accu = Local(RValue, result);
      camlstack = List.tl state.camlstack;
      camldepth = state.camldepth - 1
    } in
  let sexpl =
    sexpl1 @ sexpl2 @ sexpl3 @ sexpl4 @ op_sexpl @ pop_to_local result in
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
  let second_pos = -state.camldepth in
  let third_pos = -state.camldepth+1 in
  let state, sexpl1 = straighten_accu gpad lpad state in
  let state, sexpl2 = straighten_stack_at gpad lpad state second_pos in
  let state, sexpl3 = straighten_stack_at gpad lpad state third_pos in
  let sexpl4 = push_local "accu" in
  let sexpl5 = push_stack second_pos in
  let sexpl6 = push_stack third_pos in
  let result = new_local lpad (vtype RValue) in
  let state =
    { state with
      accu = Local(RValue, result);
      camlstack = List.tl (List.tl state.camlstack);
      camldepth = state.camldepth - 2
    } in
  let sexpl =
    sexpl1 @ sexpl2 @ sexpl3 @ sexpl4 @ sexpl5 @ sexpl6 @
      op_sexpl @ pop_to_local result in
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
  let state = popn_camlstack state (size-1) in
  let state, sexpl_flush = flush_accu gpad lpad state in
  let state =
    { state with accu = RealAccu;
                 realaccu = LogAccu RValue
    } in
  let sexpl_set =
    push_local ptr @ pop_to_local "accu" in
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
  let state =
    { state with accu = RealAccu;
                 realaccu = LogAccu RValue
    } in
  let sexpl_set =
    push_local ptr @ pop_to_local "accu" in
  (state, sexpl_alloc @ sexpl_init @ sexpl_flush @ sexpl_set)

let c_call gpad lpad state name num_args =
  let prim =
    try Hashtbl.find gpad.primitives name
    with Not_found -> assert false in
  let state, sexpl =
    if num_args <= 5 then
      let rdepth = realdepth state in
      let state, sexpl_flush = flush_accu gpad lpad state in
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
  let state =
    { state with accu = RealAccu;
                 realaccu = LogAccu RValue
    } in
  (state, sexpl)

let switch gpad lpad state labls_ints labls_blocks =
  let value = new_local lpad TI32 in
  push gpad lpad state state.accu
  @ pop_to_local value
  @ [ (* if (!Is_block(value)) *)
      L [ K "if";

          L [ K "local.get";
              ID value;
            ];
          L [ K "i32.const";
              N (I32 1l);
            ];
          L [ K "i32.and" ];

          L [ K "then";

              L [ K "local.get";
                  ID value;
                ];
              L [ K "i32.const";
                  N (I32 1l);
                ];
              L [ K "shr_s" ];

              L ( [  K "br_table" ]
                  @ ( Array.map
                        (fun lab -> ID (sprintf "label%d" lab))
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
                  K "offset=-4";
                  K "align=2"
                ];
              L [ K "i32.const";
                  N (I32 0xffl);
                ];
              L [ K "i32.and" ];

              L ( [  K "br_table" ]
                  @ ( Array.map
                        (fun lab -> ID (sprintf "label%d" lab))
                        labls_blocks
                      |>  Array.to_list
                    )
                  @ [ ID "panic" ]
                );
            ];
        ]
    ]


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
        let state = { state with accu = RealStack (-state.camldepth + sp) } in
        (state, [])
    | Kpush ->
        let state, sexpl = straighten_accu_when_on_stack gpad lpad state in
        let state = { state with camlstack = state.accu :: state.camlstack;
                                 camldepth = state.camldepth + 1 } in
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
        let state = { state with camlstack } in
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
        ( state, [ L [ K "br"; ID (sprintf "label%d" lab) ] ] )
    | Kbranchif lab ->
        let sexpl =
          push_as gpad lpad state state.accu RInt
          @ [ L [ K "br_if"; ID (sprintf "label%d" lab) ] ] in
        (state, sexpl)
    | Kbranchifnot lab ->
        let sexpl =
          push_as gpad lpad state state.accu RInt
          @ [ L [ K "i32.eqz" ];
              L [ K "br_if"; ID (sprintf "label%d" lab) ] ] in
        (state, sexpl)
    | Kswitch (labls_ints, labls_blocks) ->
        (state, switch gpad lpad state labls_ints labls_blocks)
    | Kpush_retaddr lab ->
        XXX
    | Kapply num ->
        XXX
    | Kappterm(num, slots) ->
        XXX
    | Kreturn slots ->
        XXX
    | Krestart ->
        XXX
    | Kgrab num ->
        XXX
    | Kclosure(lab, num) ->
        XXX
    | Kclosurerec(labs, num) ->
        XXX
    | Koffsetclosure index ->
        XXX
    | Kpushtrap lab ->
        XXX
    | Kpoptrap ->
        XXX
    | Kraise kind ->
        XXX
    | Kcheck_signals ->
        XXX
    | Kgetmethod ->
        XXX
    | Kgetpubmet k ->
        XXX
    | Kgetdynmet ->
        XXX
    | Kevent _ ->
        XXX
    | Kstop ->
        XXX
    | Kstrictbranchif _ -> assert false
    | Kstrictbranchifnot _ -> assert false
