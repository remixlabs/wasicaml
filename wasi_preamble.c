// Same as:
// #! /bin/sh
// exec %PREFIX%/bin/wasi_run "\$0" "\$0" "\$@"

#include <unistd.h>
#include <stdlib.h>
#include <stdio.h>
#include <string.h>

#define RUNNER "wasi_run"
#define MIDDLE "/bin/"

int main(int argc, char **argv) {
    char *prefix = getenv("WASICAML_ROOT");
    if (prefix == NULL)
        prefix = PREFIX "";
    int len = strlen(prefix) + strlen(RUNNER) + strlen(MIDDLE) + 1;
    char *runner = malloc(len);
    if (runner == NULL) {
        perror(argv[0]);
        exit(2);
    };
    strcpy(runner, prefix);
    strcat(runner, MIDDLE);
    strcat(runner, RUNNER);
    char **argu = malloc((argc+3) * sizeof(char *));
    if (argu == NULL) {
        perror(argv[0]);
        exit(2);
    };
    argu[0] = runner;
    argu[1] = argv[0];
    for (int k = 0; k < argc; k++)
        argu[2+k] = argv[k];
    argu[argc+2] = NULL;
    int code = execvp(runner, argu);
    perror(runner);
    exit(2);
}
