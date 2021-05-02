#define CAML_INTERNALS
#include "caml/mlvalues.h"
#include "caml/instruct.h"
#include "caml/prims.h"
#include "caml/domain.h"
#include "caml/startup.h"

#include <stdint.h>
#include <stdbool.h>

// not needed, can remain empty.
c_primitive caml_builtin_cprim[] = {};

// The following functions are provided by the compiled executable:
char *wasicaml_get_data(void);
asize_t wasicaml_get_data_size(void);
void wasicaml_init(void);
value mainfunc(value env, int extra_args, uint32_t codeptr, value *fp);

int zerocode[] = { 2+STOP };  // FIXME: why is the offset needed?
asize_t zerocode_size = 1;

value *wasicaml_get_global_data(void) {
    return &caml_global_data;
}

caml_domain_state *wasicaml_get_domain_state(void) {
    return Caml_state;
}

header_t *wasicaml_get_atom_table(void) {
    return caml_atom_table;
}

void debug2(int32_t x0, int32_t x1) {
    printf("DEBUG\t%d\t%d\t%x\n", x0, x1, x1);
    fflush(stdout);
}

void wasicaml_main(char **argv) {
    char *data = wasicaml_get_data();
    asize_t data_size = wasicaml_get_data_size();
    caml_startup_code(zerocode, zerocode_size, data, data_size, NULL, 0, false, argv);
    printf("Init done\n"); fflush(stdout);
    printf("Caml_state=%p\n", Caml_state);
    printf("&young_ptr=%p\n", &Caml_state_field(young_ptr));
    printf("young_ptr=%p\n", Caml_state_field(young_ptr));
    printf("&young_limit=%p\n", &Caml_state_field(young_limit));
    printf("young_limit=%p\n", Caml_state_field(young_limit));
    printf("&extern_sp=%p\n", &Caml_state_field(extern_sp));
    printf("delta extern_sp=%lx\n", ((char *) &Caml_state_field(extern_sp)) - ((char *) &Caml_state_field(young_ptr)));
    value env = Atom(0);
    int extra_args = 0;
    uint32_t codeptr = 0;
    value *fp = Caml_state->_extern_sp;
    wasicaml_init();
    mainfunc(env, extra_args, codeptr, fp);
}

int main(int argc, char **argv) {
    wasicaml_main(argv);
    return 0;
}
