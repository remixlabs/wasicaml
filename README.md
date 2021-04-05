# WASICaml

The goal of this project is to add a backend to the OCaml compiler to
generate WebAssembly code that can be run in a WASI environment.

## Status - **This is work in progess**

 - Can build an OCaml bytecode compiler against almost-WASI (see below)

# Build

```
./configure
make
```

By default, the prefix is `~/.wasicaml`, but you can also specify a different
one: `./configure --prefix=/wherever`.

It is so far not recommended to use a shared prefix like `/usr/local`.

Note that `make` installs files from the very beginning.

# Remarks

## A special OCaml branch is needed

See https://github.com/gerdstolpmann/ocaml.git, branch `gerd/wasi`.

## The C toolchain in `bin`

This toolchain is a wrapper around `wasi-sdk`. You get:
 - `wasi_cc`
 - `wasi_ld`
 - `wasi_ar`
 - `wasi_nm`
 - `wasi_objdump`
 - `wasi_ranlib`
 - `wasi_strip`
 - `wasi_run`

Here, `wasi_cc` is a specially configured Clang compiler that uses
a special version of Musl as `libc`. The .o files and the final executables
are WebAssembly modules.

Our wrapper also prepends a preamble to the executables so that
the files can be directly run on the host system. This preamble is
equivalent to these two lines of Shell code (but actually a real
executable):

```
#! /bin/sh
exec <path>/wasicaml/bin/run "$0" "$0" "$@"
```

These two lines are then followed by the "wasm" code. If the name of the
executable has the suffix ".wasm", however, this starter is omitted, and
the file consists ONLY of the WebAssembly module.

The `wasi_run` script is written in Javascript (see the `js/` folder).

## Additions to WASI

We build so far for `wasi-snapshot-preview1`, which is an early pre-release
of WASI mainly featuring access to the file system. Besides that, there
are environment variables, and you can read the clock. That's it!
Anything else is unavailable (e.g. subprocesses, pipes, network, signals, ...).

Also, an important feature is so far missing from both WASI and WebAssembly:
setjmp/longjmp, or (equivalently) exceptions. There is a proposal to add
exceptions to WebAssembly, which is currently being implemented by the
browser vendors, so that the door to standardization is being opened.

All in all this means that pure WASI is currently not sufficient to run
OCaml code. We "added" a few things as host functions:
 - a try/catch mechanism
 - the `system()` function to start external processes
 - a working `rename()` function (as the one in the current `wasi-sdk` is
   broken)

Note that adding try/catch is kind of problematic, as it exploits the
way WebAssembly is implemented in Javascript (where you can use JS
exceptions to jump out of WebAssembly context), and the same trick might
not be available in other WebAssembly implementations (in particular,
I don't see how to do the same in the Rust-based ones). This can first be
improved when WebAssembly-level exceptions become widely available.

We also link `initwasi.o` into every executable. Besides some functions that
are missing in `libc` there is an initializer that sets the current working
directory to the contents of the environment variable `WASICAML_CWD`.
Without this initializer, the working directory would be set to `/` inside
the WASI sandbox when the executable is started. This trick ensures that
the working directory inside the
sandbox is the same as outside.

So far, we do not support to restrict the filesystem in the sandbox,
i.e. `/` in the sandbox is the real root of the host machine.
(This will be configurable again at some point in the future.)

## What works in OCaml

So far, the bytecode interpreter can be built, and we can do a bootstrap
cycle of the compiler.

This is supported:
 - Complete language
 - Standard library, with a few exceptions in `Sys`
 - The library `str`
 - The library `unix` (but many functions are mssing)
 - `ocamlc -custom`

This does not work:
 - Dynamic loading of C libraries ( - there is no `dlopen` in WASI)
 - No threads
 - No debugger
 - No ocamldoc
