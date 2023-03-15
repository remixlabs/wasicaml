#ifndef WASICAML_H
#define WASICAML_H

#ifndef WASM_EH
#define HOST_EH
#endif

#include <stdbool.h>
#include <stdint.h>

// Functions that are defined outside ocaml, either by helper code, or
// even in the host environment.

__attribute__((export_name("wasicaml_wraptry")))
extern void * wasicaml_wraptry(void *(*f)(void *), void *ctx);

__attribute__((export_name("wasicaml_wraptry2")))
extern void * wasicaml_wraptry2(void * (*f)(void *, void *),
                                void *ctx1, void *ctx2);

__attribute__((export_name("wasicaml_wraptry3")))
extern void * wasicaml_wraptry3(void * (*f)(void *, void *, void *),
                                void *ctx1, void *ctx2, void *ctx3);

__attribute__((export_name("wasicaml_wraptry4")))
extern void * wasicaml_wraptry4(void * (*f)(void *, void *, void *, void *),
                                void *ctx1, void *ctx2, void *ctx3, void *ctx4);

__attribute__((export_name("wasicaml_wraptry5")))
extern void * wasicaml_wraptry5(void * (*f)(void *, void *, void *, void *, void *),
                              void *ctx1, void *ctx2, void *ctx3, void *ctx4,
                              void *ctx5);
// wrappers that call wasicaml_try* and fix up the stack pointer
// afterwards

__attribute__((export_name("wasicaml_wrapthrow")))
__attribute__((noreturn))
extern void wasicaml_wrapthrow(void);
// throws an exception - to be caught by wasicaml_wraptry.

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
