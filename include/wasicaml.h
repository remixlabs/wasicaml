#ifndef WASICAML_H
#define WASICAML_H

#include <stdbool.h>
#include <stdint.h>

// Functions that are defined outside ocaml, either by helper code, or
// even in the host environment.

__attribute__((import_module("wasicaml")))
__attribute__((import_name("wasicaml_try")))
extern bool wasicaml_try(void (*f)(void *), void *ctx);
// runs f(ctx) and catches any exceptions raised via wasicaml_throw.
// Returns true if such an exception was caught, and false if f returns
// normally. THIS CAN BE A HOST FUNCTION, AND IT DOESN'T NEED TO FIX UP
// THE SHADOW STACK POINTER AFTER CATCHING THE EXCEPTION.

__attribute__((import_module("wasicaml")))
__attribute__((import_name("wasicaml_try4")))
extern bool wasicaml_try4(int32_t (*f)(void *, void *, void *, void *),
                          void *ctx1, void *ctx2, void *ctx3, void *ctx4);
// Same but f is called with four context params:
// f(ctx1,ctx2,ctx3,ctx4)

__attribute__((export_name("wasicaml_wraptry")))
extern bool wasicaml_wraptry(void (*f)(void *), void *ctx);

__attribute__((export_name("wasicaml_wraptry4")))
extern bool wasicaml_wraptry4(int32_t (*f)(void *, void *, void *, void *),
                              void *ctx1, void *ctx2, void *ctx3, void *ctx4);
// wrappers that call wasicaml_try/try4 and fix up the stack pointer
// afterwards

__attribute__((import_module("wasicaml")))
__attribute__((import_name("wasicaml_throw")))
__attribute__((noreturn))
extern void wasicaml_throw();
// throws an exception - to be caught by wasicaml_try.

__attribute__((export_name("wasicaml_call")))
extern void wasicaml_call(void (*f)(void *), void *ctx);

__attribute__((export_name("wasicaml_call4")))
extern void wasicaml_call4(int32_t (*f)(void *, void *, void *, void *),
                           void *ctx1, void *ctx2, void *ctx3, void *ctx4 );

__attribute__((import_module("wasicaml")))
__attribute__((import_name("wasicaml_system")))
extern int wasicaml_system(const char *command);
// support for system()

__attribute__((import_module("wasicaml")))
__attribute__((import_name("wasicaml_rename")))
extern int wasicaml_rename(const char *old, const char *new);
// rename() is broken in wasi-sdk-12


char *mktemp (char *);
// an mktemp emulation

#endif
