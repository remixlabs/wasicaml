// Same as:
// #! /bin/sh
// exec %PREFIX%/bin/wasi_run "\$0" "\$0" "\$@"

#include <unistd.h>
#include <stdlib.h>
#include <stdio.h>

char *wasi_run1 = PREFIX "/bin/wasi_run";
char *wasi_run2 = "wasi_run";

int main(int argc, char **argv) {
    char *runner = wasi_run1;
    int code = access(runner, X_OK);
    if (code < 0) {
        runner = wasi_run2;
    }
    char **argu = malloc((argc+3) * sizeof(char *));
    argu[0] = runner;
    argu[1] = argv[0];
    for (int k = 0; k < argc; k++)
        argu[2+k] = argv[k];
    argu[argc+2] = NULL;
    code = execvp(runner, argu);
    perror(argv[0]);
    exit(2);
}
