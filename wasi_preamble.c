// Same as:
// #! /bin/sh
// exec %PREFIX%/bin/wasi_run "\$0" "\$0" "\$@"

#include <unistd.h>
#include <stdlib.h>
#include <stdio.h>

char *wasi_run = PREFIX "/bin/wasi_run";

int main(int argc, char **argv) {
    char **argu = malloc((argc+2) * sizeof(char *));
    argu[0] = wasi_run;
    argu[1] = argv[0];
    for (int k = 0; k < argc; k++)
        argu[2+k] = argv[k];
    int code = execv(wasi_run, argu);
    perror(argv[0]);
    exit(2);
}
