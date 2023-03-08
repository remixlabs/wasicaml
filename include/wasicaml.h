#ifndef WASICAML_H
#define WASICAML_H

#include <stdbool.h>
#include <stdint.h>

// Functions that are defined outside ocaml, either by helper code, or
// even in the host environment.

__attribute__((import_module("wasicaml")))
__attribute__((import_name("wasicaml_try")))
extern void * wasicaml_try(void *(*f)(void *), void *ctx);
// runs f(ctx) and catches any exceptions raised via wasicaml_throw.
// Returns NULL if such an exception was caught, and passes the return
// value of f back otherwise (which shouldn't be NULL).
// THIS CAN BE A HOST FUNCTION, AND IT DOESN'T NEED TO FIX UP
// THE SHADOW STACK POINTER AFTER CATCHING THE EXCEPTION.

__attribute__((import_module("wasicaml")))
__attribute__((import_name("wasicaml_try2")))
extern void * wasicaml_try2(void *(*f)(void *, void *),
                            void *ctx1, void *ctx2);
// Same but f is called with two context params:
// f(ctx1,ctx2)

__attribute__((import_module("wasicaml")))
__attribute__((import_name("wasicaml_try3")))
extern void * wasicaml_try3(void *(*f)(void *, void *, void *),
                            void *ctx1, void *ctx2, void *ctx3);
// Same but f is called with three context params:
// f(ctx1,ctx2,ctx3)

__attribute__((import_module("wasicaml")))
__attribute__((import_name("wasicaml_try4")))
extern void * wasicaml_try4(void * (*f)(void *, void *, void *, void *),
                            void *ctx1, void *ctx2, void *ctx3, void *ctx4);
// Same but f is called with four context params:
// f(ctx1,ctx2,ctx3,ctx4)

__attribute__((import_module("wasicaml")))
__attribute__((import_name("wasicaml_try5")))
extern void * wasicaml_try5(void * (*f)(void *, void *, void *, void *, void *),
                            void *ctx1, void *ctx2, void *ctx3, void *ctx4,
                            void *ctx5);
// Same but f is called with five context params:
// f(ctx1,ctx2,ctx3,ctx4,ctx5)

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

__attribute__((import_module("wasicaml")))
__attribute__((import_name("wasicaml_throw")))
__attribute__((noreturn))
extern void wasicaml_throw();
// throws an exception - to be caught by wasicaml_try.

__attribute__((export_name("wasicaml_call")))
extern void * wasicaml_call(void *(*f)(void *), void *ctx);

__attribute__((export_name("wasicaml_call2")))
extern void * wasicaml_call2(void * (*f)(void *, void *),
                             void *ctx1, void *ctx2);

__attribute__((export_name("wasicaml_call3")))
extern void * wasicaml_call3(void * (*f)(void *, void *, void *),
                             void *ctx1, void *ctx2, void *ctx3);

__attribute__((export_name("wasicaml_call4")))
extern void * wasicaml_call4(void * (*f)(void *, void *, void *, void *),
                             void *ctx1, void *ctx2, void *ctx3, void *ctx4 );

__attribute__((export_name("wasicaml_call5")))
extern void * wasicaml_call5(void * (*f)(void *, void *, void *, void *, void *),
                             void *ctx1, void *ctx2, void *ctx3, void *ctx4,
                             void *ctx5);

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
