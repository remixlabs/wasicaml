#ifndef WASICAML_H
#define WASICAML_H

#include <stdbool.h>

// Functions that are defined outside ocaml, either by helper code, or
// even in the host environment.

__attribute__((import_module("wasicaml")))
__attribute__((import_name("try")))
extern bool wasicaml_try(void (*f)(void *), void *ctx);
// runs f(ctx) and catches any exceptions raised via wasicaml_throw.
// Returns true if such an exception was caught, and false if f returns
// normally.

__attribute__((import_module("wasicaml")))
__attribute__((import_name("throw")))
__attribute__((noreturn))
extern void wasicaml_throw();
// throws an exception - to be caught by wasicaml_try.

__attribute__((export_name("wasicaml_call")))
extern void wasicaml_call(void (*f)(void *), void *ctx);

__attribute__((import_module("wasicaml")))
__attribute__((import_name("system")))
extern int wasicaml_system(const char *command);
// support for system()

__attribute__((import_module("wasicaml")))
__attribute__((import_name("rename")))
extern int wasicaml_rename(const char *old, const char *new);
// rename() is broken in wasi-sdk-12


char *mktemp (char *);
// an mktemp emulation

#endif
