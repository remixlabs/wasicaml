#include <stdlib.h>
#include <unistd.h>
#include <stdio.h>

#include "wasicaml.h"

__attribute__((constructor))
static void init(void) {
    char *startdir = getenv("WASICAML_CWD");
    if (startdir != NULL) {
        if (chdir(startdir) == -1) {
            fprintf(stderr, "WASICAML: Cannot chdir to %s at startup.\n", startdir);
            _exit(2);
        }
    }
}

void wasicaml_call(void (*f)(void *), void *ctx) {
    f(ctx);
}


