# WASICaml

WASICaml is currently:
 - a port of the OCaml bytecode interpreter to WebAssembly using the WASI
   ABI (with very few ABI extensions)
 - a postprocessor that takes a bytecode executable and translates it into
   WebAssembly

The hope is that WASICaml evolves into a full compiler backend targeting
WebAssembly.

The motivation for WASICaml is to be able to run OCaml code in situations
where it is difficult or cumbersome to provide native binaries, for example
 - run code in cross-platform environments, e.g. embed some OCaml in an
   Electron app
 - cross-platform plugins
 - mobile OS
 - user-generated code
 - edge computing devices

Note that I left web browsers out in this list - of course, you can run
WebAssembly also in browsers, but as there are already solutions for
generating Javascript from OCaml, so this was not a driver for this
project.

Also be aware that there are many ways of running WebAssembly, and it is not
always Javascript that serves as a host environment:
 - engine featuring JIT (e.g. all current browsers, node, wasmtime)
 - engine not featuring JIT (e.g. wasm3)
 - translation to other representations (e.g. translate to C with
   wasm2c - note that you can then generate bitcode from OCaml)

Currently, WASICaml is, however, essentially restricted to a Javascript
host environment because we need exceptions, and the non-Javascript
environments generally do not yet implement exceptions (but it's a
coming WebAssembly feature, so this restriction will go away at some
point). At this point, it is assumed that there are a few functions
provided by the host environment that implement a throw/catch mechanism.

The WASI ABI is a very restricted Unix-like ABI allowing so far:
 - access to environment variables
 - access to date and time
 - access to local files (you can mount local directories into a
   virtual file tree)
 - access to stdin, stdout, stderr

This is almost enough for running `ocamlc`. The only addition we need
is the `system` function for starting external processes, i.e. we
assume that `system` is provided by the host environment.



# Build

```
./configure
make
```

Optionally, do `make test` to run a couple of tests.

By default, the prefix is `~/.wasicaml`, but you can also specify a different
one: `./configure --prefix=/wherever`.

It is so far not recommended to use a shared prefix like `/usr/local`.
Also, do not install into the opam hierarchy.

Note that `make` installs files from the very beginning (i.e. we do not
cleanly distinguish between build and install phases yet).

You'll need `node` (14 or 16) and `npm`.

The build involves a git checkout of the OCaml sources (version 4.12.0),
and you'll get WebAssembly versions of the bytecode compiler. Also,
we download the WASI SDK, including a copy of `clang`. The latter is
currently only available for Linux and Mac as precompiled tarballs.
For other OS you are on your own.



# What you get

The installed files are:
 - LLVM toolchain
   + `bin/wasi_cc`: wrapper for `clang` in WASI SDK
   + `bin/wasi_ld`: wrapper for `wasm-ld` in WASI SDK
   + `bin/wasi_ar`: wrapper for `ar` in WASI SDK
   + `bin/wasi_nm`: wrapper for `nm` in WASI SDK
   + `bin/wasi_objdump`: wrapper for `objdump` in WASI SDK
   + `bin/wasi_ranlib`: wrapper for `ranlib` in WASI SDK
   + `bin/wasi_strip`: wrapper for `strip` in WASI SDK
   + `bin/wasi_preamble`: see below
   + `lib/wasi-sdk`: the WASI SDK tree
   + `lib/initwasi.o`: this object is linked into all executables by
      `wasi_cc` and performs global initializations
 - WASI-enabled WebAssembly runtime
   + `bin/wasi_run`: starts a node process running a WebAssembly executable
   + `js/`: the Javascript code for this
 - OCaml toolchain
   + `bin/ocaml`: WebAssembly port of the OCaml toploop (using bytecode)
   + `bin/ocamlc`: WebAssembly port of the OCaml compiler (using bytecode)
   + `bin/ocamlrun`: WebAssembly port of the OCaml bytecode engine
   + a few other standard OCaml tools
 - WASICaml toolchain
   + `bin/wasicaml`: translates OCaml bytecode to WebAssembly
   + `lib/initruntime.o`: linked into executables produced by
      `bin/wasicaml` to initialize the OCaml runtime


## The LLVM toolchain

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
executable, `wasi_preamble`):

```
#! /bin/sh
exec <path>/wasicaml/bin/wasi_run "$0" "$0" "$@"
```

These two lines are then followed by the "wasm" code. If the name of the
executable has the suffix ".wasm", however, this starter is omitted, and
the file consists ONLY of the WebAssembly module. So, e.g.

```
$ wasi_cc -o hello hello.c
```

would create `hello` as WebAssembly code where the preamble is prepended,
and

```
$ wasi_cc -o hello.wasm hello.c
```

would create `hello.wasm` as only the WebAssembly code.

Note that the same convention also applies to `ocamlc` and `wasicaml`:
```
$ ocamlc -o hello ...              # with preamble
$ ocamlc -o hello.wasm ...         # pure WebAssembly
$ wasicaml -o hello ...            # with preamble
$ wasicaml -o hello.wasm ...       # pure WebAssembly
```

## Additions to WASI

We build so far for `wasi-snapshot-preview1`, which is an early pre-release
of WASI mainly featuring access to the file system. Besides that, there
are environment variables, and you can read the clock. That's it!
Anything else is unavailable (e.g. subprocesses, pipes, network, signals, ...).

Also, an important feature is so far missing from both WASI and WebAssembly:
setjmp/longjmp, or (equivalently) exceptions. There is a proposal to add
exceptions to WebAssembly, which is currently being implemented by the
engine vendors, so that the door to standardization is being opened.

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
the working directory inside the sandbox is the same as outside.

So far, we do not support to restrict the filesystem in the sandbox,
i.e. `/` in the sandbox is the real root of the host machine.
(This will be configurable again at some point in the future.)


## The OCaml toolchain

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

Note that WebAssembly is so far always a 32 bit environment.


## The WASICaml postprocessor

`wasicaml` takes a bytecode executable as input, and generates
WebAssembly as output, e.g.

```
$ ocamlc -o hello hello.ml
$ wasicaml -o hello_wasm hello
```

The resulting code is somewhat faster than the bytecode, and sometimes
a lot faster (e.g. small loops on arrays and strings). Occasionally,
it can also happen that WebAssembly is slower, in particular, when
many exceptions are thrown.

It is recommended to pass the `-g` switch to `ocamlc`. This way the
OCaml function names can be encoded in the symbol names, which is
results in better stack traces.

There are a couple of restrictions:
 - Tail calls are generally not available. There is some emulation
   for self calls, and when one function of a `let rec` definition
   calls another function of the same `let rec`. (Note that tail calls
   are another feature announced for WebAssembly.)
 - You cannot obtain backtraces from the running OCaml program.
   You can do from Javascript, though.
 - You cannot change the limit of the OCaml stack from inside the
   running OCaml program. The stack is preallocated at the beginning,
   and the translation scheme prevents that it can be moved to
   another address. (Attempting to change the limit with `Gc.set`
   currently crashes the program.)

You can link in C libraries, e.g.
```
$ wasicaml -o hello_wasm hello -cclib ~/.wasicaml/lib/ocaml/libunix.a
```

Of course, these libraries must be compiled for WebAssembly, too.



# Troubleshooting

## WASI

During the development I figured out that the current WASI SDK
is quite buggy. In particular, WASI files are sometimes not cleanly
mapped to physical files (random characters are inserted into the
names). The `rename()` function was completely unusable.



# How it works

The reason for taking the bytecode as starting point for `wasicaml` is
that it allows us an easy start of the project. It is a good starting
point because it is a stack machine - close to what we need, but there
are also downsides, in particular information is missing that would
be very helpful (e.g. types, or whether the function to call is a
certain static function). But anyway, we don't have to create a full
new toolchain, but can just append to an existing one.

For the following, keep in mind that WebAssembly code is normally not
directly interpreted. There is a JIT pass, and even non-JIT
interpreters do not directly operate on the code as it is, but rather
on a preprocessed version that is optimized for execution. This
normally includes some mapping from logical stores to physical ones,
and often also advanced techniques like function inlining.

## The outer structure

The bytecode is decoded into instructions, and a control flow graph
(CFG) is created.

WebAssembly doesn't allow arbitrary branches, but just those that
are allowed in structured programming (there is `if/then/else`, and
a way to break out of blocks, and a jump back to the start of a loop -
but there is no way to jump to an arbitrary label). Because of this,
the code has to be restructured so that we can emit it in one of
the permitted forms. This is done by a pass over the CFG.

Roughly, one OCaml function is translated to one WebAssembly function.
However, this is not fully true, in particular all functions of a `let
rec` block go into the same WebAssembly function. This allows us
to translate tail calls from one function to the other with a simple
branch statement. The further functions a WebAssembly function implements
are called "subfunctions".

Another complication are exceptions. As there is not yet direct support
for exceptions in WebAssembly, we assume that there is a host mechanism
where a special function `wasicaml_try4(f, arg1, arg2, arg3, arg4)`
calls the function like `f(arg1, arg2, arg3, arg4)` and traps when
the called function invokes `wasicaml_throw()`. Because of this mechanism
the code inside the "try" block must be a function of its own.
In our translation scheme, this is ensured by allocating another
subfunction index for "try" blocks.

Summarized, a WebAssembly function includes the code of all the
OCaml functions of the same `let rec` definition where each function
is identified by a subfunction index. Also, the code inside
`try` blocks are also translated to subfunctions.

The subfunction index is encoded in code pointers.


## Functions

A WebAssembly function takes four parameters:
 - `envptr`: a pointer to the stack location where the closure value is stored
 - `extra_args`: the number of further arguments on the stack (i.e. in total
   there are `extra_args+1` arguments)
 - `codeptr`: the code pointer (initially taken from `*envptr[0]` but
   can be overridden here)
 - `fp`: the address of the first argument

The OCaml arguments are always passed via the stack, and not via the
WebAssembly parameters. (It is unclear whether this would be
an advantage - still need to be figured out.)

There are several deviations from the scheme the bytecode interpreter uses.
First, the closure value (called `env` in the interpreter sources) is
not directly passed, but indirectly. This way we don't have to save it
when calling functions that could run the GC.

Second, `extra_args` is not a global counter, but passed on from function to
function. This also has the nice effect that we don't need to save it
when calling other functions.

Third, the code pointer is not an address. As WebAssembly is a Harvard
architecture, code generally doesn't have addresses. Instead, there is
an indirection mechanism using so-called tables. Essentially, this
mechanism boils down to enumerating the indirectly callable functions
with small integers. We reserve now a part of the 32 bits of the code
pointer for these function IDs. Another part is used for encoding the
subfunction index.  One bit indicates whether a function is restarted
(as if it went through the `RESTART` instruction). The LSB of code
pointers is always 1, so that the GC doesn't touch code pointers.

Fourth, there is no global stack pointer. Instead, each function is
called with a frame pointer, which is the address of the first
argument (and further arguments may follow at higher stack positions).
The function can now use more stack space, so that the non-negative
offsets are arguments, and the negative ones are local values.
(Note that WebAssembly doesn't support negative offsets directly,
and as a workaround we compute another pointer `bp` which is just
`fp` shifted by some value so that negative offsets do not occur
anymore.)

A function returns one 32 bit value, which is normally the result
value. If, however, a `NULL` is returned, this has to be treated by
the caller as if an exception was raised (as a quicker alternative
to calling `wasicaml_throw()`).


## Inside functions

Now, the question is how the OCaml bytecode instructions are translated
into WebAssembly.

The main difficulty is the garbage collector (GC). WebAssembly knows
"local variables" which could be seen as an open number of registers of a
classical CPU. However, there is no way to access the variables from
outside, and hence the GC has no chance of scanning them for addresses.
Because of this, we generally do not put boxed values into variables
(actually, there is one exception: the `accu` variable - leading to
special treatment here and there).

We inherit the stack of the OCaml bytecode interpreter and use it as
"shadow stack" for OCaml functions. All OCaml values are allowed on
the shadow stack because it is scannable.

We have to take care that the shadow stack is always fully initialized
with values (invalid addresses confuse the non-conservative GC).

The OCaml bytecode assumes a stack machine, and uses push/pop for
manipulating the stack. This is non-optimal for us. It is better to
access the stack positions relative to the beginning of the frame.
For this reason, the stack code is transformed to that style
(which is kind of a conversion from stack machine to register machine).

Instructions that give integer results normally do not write the
integers to the stack, but first to a local variable. Only if needed
the variable is later put onto the stack (e.g. for calling other
functions). This gives the WebAssembly engine some more freedom to
generate optimal code in the JIT phase, and chances are that
the lowest-level arithmetic is done in CPU registers.

The translation scheme is still fairly simple, and there are a number
of ideas how to improve it (see the Github issues for this project).


## Emitted code

The `Wc_emit` module returns an S-expression representing the code
in WAT format. We also write this code to a file (which is very
readable).

Unfortunately, we cannot directly feed the `.wat` file to an assembler
for further processing. We had to go another route. LLVM features an
integrated assembler, and it turned out it also understands a form
of textual WebAssembly. The S-expression is converted to this format,
resulting into an `.s` file.
