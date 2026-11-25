#! /bin/sh

set -e

if [ -z "$OCAMLC" ]; then
    echo "missing var: OCAMLC" >&2
    exit 1
fi

cc="$($OCAMLC -config-var c_compiler)"
stdlib="$($OCAMLC -config-var standard_library)"


eval_c_expr() {
    rm -rf tmp
    mkdir -p tmp
    (
        set -e
        cd tmp
        cat <<EOF >eval.c
#define CAML_NAME_SPACE
#define CAML_INTERNALS
#define CAML_USE_WASICAML
#include <stdio.h>
#include "config.h"
#include "domain_state.h"
#include "fail.h"
#include "mlvalues.h"
#include "memory.h"
int main(int argc, char **argv) {
    caml_domain_state state;
    printf("%td\n", (long) ($1));
    return 0;
}
EOF
        "$cc" -I"$stdlib/caml" -c eval.c
        "$cc" -I"$stdlib/caml" -o eval eval.o
        ./eval
    ) || false
}

domain_field_offset() {
    eval_c_expr "(((void*) &state.$1)-((void*) &state))/8"
}

max_young_wosize="$(eval_c_expr Max_young_wosize)"

df_young_ptr="$(domain_field_offset young_ptr)"
df_young_limit="$(domain_field_offset young_limit)"
df_stack_low="$(domain_field_offset stack_low)"
df_stack_high="$(domain_field_offset stack_high)"
df_stack_threshold="$(domain_field_offset stack_threshold)"
df_extern_sp="$(domain_field_offset extern_sp)"
df_trapsp="$(domain_field_offset trapsp)"
df_trap_barrier="$(domain_field_offset trap_barrier)"
df_external_raise="$(domain_field_offset external_raise)"
df_exn_bucket="$(domain_field_offset exn_bucket)"
df_local_roots="$(domain_field_offset local_roots)"

gf_zero_divide="$(eval_c_expr ZERO_DIVIDE_EXN)"
gf_stack_overflow="$(eval_c_expr STACK_OVERFLOW_EXN)"

double_tag="$(eval_c_expr Double_tag)"
double_size="$(eval_c_expr Double_wosize)"
double_array_tag="$(eval_c_expr Double_array_tag)"
closure_tag="$(eval_c_expr Closure_tag)"
infix_tag="$(eval_c_expr Infix_tag)"

caml_from_c="$(eval_c_expr CAML_FROM_C)"

cat <<EOF >ocaml_config.ml
let max_young_wosize = $max_young_wosize
let domain_field_young_ptr = $df_young_ptr
let domain_field_young_limit = $df_young_limit
let domain_field_stack_low = $df_stack_low
let domain_field_stack_high = $df_stack_high
let domain_field_stack_threshold = $df_stack_threshold
let domain_field_extern_sp = $df_extern_sp
let domain_field_trapsp = $df_trapsp
let domain_field_trap_barrier = $df_trap_barrier
let domain_field_external_raise = $df_external_raise
let domain_field_exn_bucket = $df_exn_bucket
let domain_field_local_roots = $df_local_roots

let global_field_zero_divide = $gf_zero_divide
let global_field_stack_overflow = $gf_stack_overflow

let double_size = $double_size
let double_tag = $double_tag
let double_array_tag = $double_array_tag

let closure_tag = $closure_tag
let infix_tag = $infix_tag

let caml_from_c = $caml_from_c
EOF
