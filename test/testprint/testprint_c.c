#include <stdio.h>

#include <caml/mlvalues.h>

value testprint_string(value v_var, value v_str) {
    printf("%s=%s\n", String_val(v_var), String_val(v_str));
    return Val_unit;
}

value testprint_int(value v_var, value v_int) {
    printf("%s=%ld\n", String_val(v_var), Long_val(v_int));
    return Val_unit;
}

value testprint_float(value v_var, value v_float) {
    printf("%s=%f\n", String_val(v_var), Double_val(v_float));
    return Val_unit;
}
