#include <stdlib.h>
#include <unistd.h>
#include <stdio.h>
#include <string.h>
#include <time.h>
#include <sys/stat.h>
#include <errno.h>

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

bool wasicaml_wraptry(void (*f)(void *), void *ctx) {
    // The point of this wrapper is to save the stack pointer, and to
    // restore it on exit. The code for this is generated by the C compiler
    // when a local variable is present that cannot be optimized away.
    volatile int dummy = 0;
    return wasicaml_try(f, ctx);
}

bool wasicaml_wraptry4(int32_t (*f)(void *, void *, void *, void *),
                       void *ctx1, void *ctx2, void *ctx3, void *ctx4) {
    volatile int dummy = 0;
    return wasicaml_try4(f,ctx1,ctx2,ctx3,ctx4);
}

void wasicaml_call(void (*f)(void *), void *ctx) {
    f(ctx);
}


void wasicaml_call4(int32_t (*f)(void *, void *, void *, void *),
                    void *x1, void *x2, void *x3, void *x4) {
    f(x1,x2,x3,x4);
}


static char *__randname(char *template) {
    int i;
    struct timespec ts;
    unsigned long r;
    
    clock_gettime(CLOCK_REALTIME, &ts);
    r = ts.tv_nsec*65537 ^ (uintptr_t)&ts / 16 + (uintptr_t)template;
    for (i=0; i<6; i++, r>>=5)
        template[i] = 'A'+(r&15)+(r&16)*2;
    
    return template;
}

char *mktemp (char *template) {
    size_t l = strlen(template);
    int retries = 100;
    struct stat st;
    
    if (l < 6 || memcmp(template+l-6, "XXXXXX", 6)) {
        errno = EINVAL;
        *template = 0;
        return template;
    }
    
    do {
        __randname(template+l-6);
        if (stat(template, &st)) {
            if (errno != ENOENT) *template = 0;
            return template;
        }
    } while (--retries);
    
    *template = 0;
    errno = EEXIST;
    return template;
}
