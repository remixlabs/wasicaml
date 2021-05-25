#include <stdio.h>

#include <caml/mlvalues.h>
#include <caml/alloc.h>
#include <caml/memory.h>

value testprint_string(value v_var, value v_str) {
    printf("%s=%s\n", String_val(v_var), String_val(v_str));
    fflush(stdout);
    return Val_unit;
}

value testprint_int(value v_var, value v_int) {
    printf("%s=%ld\n", String_val(v_var), Long_val(v_int));
    fflush(stdout);
    return Val_unit;
}

value testprint_float(value v_var, value v_float) {
    printf("%s=%f\n", String_val(v_var), Double_val(v_float));
    fflush(stdout);
    return Val_unit;
}

value testprint_vector(value v0, value v1, value v2, value v3, value v4, value v5) {
    CAMLparam5(v0, v1, v2, v3, v4);
    CAMLxparam1(v5);
    value a = caml_alloc(6, 0);
    Field(a, 0) = v0;
    Field(a, 1) = v1;
    Field(a, 2) = v2;
    Field(a, 3) = v3;
    Field(a, 4) = v4;
    Field(a, 5) = v5;
    CAMLreturn(a);
}

value testprint_vector_byte(value *argv, int n) {
    return testprint_vector(argv[0], argv[1], argv[2],
                            argv[3], argv[4], argv[5]);
}
