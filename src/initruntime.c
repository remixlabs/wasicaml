#define CAML_INTERNALS
#include "caml/mlvalues.h"
#include "caml/instruct.h"
#include "caml/prims.h"
#include "caml/domain.h"
#include "caml/startup.h"

#include <stdint.h>
#include <stdbool.h>

// The following functions are provided by the compiled executable:
char *wasicaml_get_data(void);
asize_t wasicaml_get_data_size(void);
void wasicaml_init(void);
value mainfunc(value env, int extra_args, uint32_t codeptr, value *fp);

int zerocode[] = { STOP };
asize_t zerocode_size = 1;

value *wasicaml_get_global_data(void) {
    return &caml_global_data;
}

caml_domain_state *wasicaml_get_domain_state(void) {
    return Caml_state;
}

c_primitive *wasicaml_get_builtin_cprim(void) {
    return caml_builtin_cprim;
}

header_t *wasicaml_get_atom_table(void) {
    return caml_atom_table;
}

void wasicaml_main(char **argv) {
    char *data = wasicaml_get_data();
    asize_t data_size = wasicaml_get_data_size();
    caml_startup_code(zerocode, zerocode_size, data, data_size, NULL, 0, false, argv);
    value env = Atom(0);
    int extra_args = 0;
    uint32_t codeptr = 0;
    value *fp = Caml_state->_extern_sp;
    wasicaml_init();
    mainfunc(env, extra_args, codeptr, fp);
}
