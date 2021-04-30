-include Makefile.config

.PHONY: default \
        setup setup-downloads setup-git setup-bin setup-js \
        configure configure-ocaml \
	build build-ocaml \
	install install-downloads install-bin install-ocaml install-lib \
	clean clean-ocaml

default: setup configure build install

setup: setup-downloads setup-git setup-bin setup-js

setup-downloads:
	./install_wasi-sdk
	./install_wabt

# ./install_wasmtime

setup-git:
	git submodule update --init

TOOLS=wasi_cc wasi_ld wasi_ar wasi_ranlib wasi_nm wasi_strip wasi_objdump wasi_run

setup-bin: lib/initwasi.o wasi_preamble
	mkdir -p $(prefix)/bin
	for f in $(TOOLS); do \
	    sed -e 's:%ROOT%:'`pwd`: -e 's:%PREFIX%:$(prefix):' bin-template/$$f >$(prefix)/bin/$$f; \
            chmod a+x $(prefix)/bin/$$f; \
	done
	cp wasi_preamble $(prefix)/bin

setup-js:
	cd js && npm install

configure: configure-ocaml

configure-ocaml:
	root=`pwd`; \
	cd ocaml; \
	./configure \
		CC="$(prefix)/bin/wasi_cc" \
		LD="$(prefix)/bin/wasi_ld" \
		AR="$(prefix)/bin/wasi_ar" \
		NM="$(prefix)/bin/wasi_nm" \
		RANLIB="$(prefix)/bin/wasi_ranlib" \
		STRIP="$(prefix)/bin/wasi_strip" \
		OBJDUMP="$(prefix)/bin/wasi_objdump" \
		CPPFLAGS="-DCAML_USE_WASICAML -DWASICAML_PROVIDE_SYSTEM -DWASICAML_PROVIDE_RENAME -I$$root/include" \
		CFLAGS="-DCAML_USE_WASICAML -DWASICAML_PROVIDE_SYSTEM -DWASICAML_PROVIDE_RENAME -I$$root/include" \
		--disable-shared \
		--disable-systhreads \
		--disable-native-compiler \
		--disable-ocamltest \
		--disable-ocamldoc \
		--host wasm32-unknown-wasi \
		--prefix=$(prefix)

lib/initwasi.o: src/initwasi.c
	mkdir -p lib
	./wasi-sdk/bin/clang --sysroot=wasi-sdk/share/wasi-sysroot \
		-Iinclude \
		-o lib/initwasi.o -c src/initwasi.c

lib/initruntime.o: src/initruntime.c
	mkdir -p lib
	./wasi-sdk/bin/clang --sysroot=wasi-sdk/share/wasi-sysroot \
		-Iocaml/runtime \
		-o lib/initruntime.o -c src/initruntime.c

src/prims.c:
	~/.wasicaml/bin/ocamlrun -p > src/primitives
	(echo '#define CAML_INTERNALS'; \
	 echo '#include "caml/mlvalues.h"'; \
	 echo '#include "caml/prims.h"'; \
	 sed -e 's/.*/extern value &();/' src/primitives; \
	 echo 'c_primitive caml_builtin_cprim[] = {'; \
	 sed -e 's/.*/  &,/' src/primitives; \
	 echo '  0 };'; \
	 echo 'char * caml_names_of_builtin_cprim[] = {'; \
	 sed -e 's/.*/  "&",/' src/primitives; \
	 echo '  0 };') > src/prims.c

lib/prims.o: src/prims.c
	mkdir -p lib
	./wasi-sdk/bin/clang --sysroot=wasi-sdk/share/wasi-sysroot \
		-Iocaml/runtime -flto \
		-o lib/prims.o -c src/prims.c

build: build-ocaml

build-ocaml:
	cd ocaml && make

install: install-downloads install-bin install-js install-ocaml install-lib

install-downloads:
	mkdir -p $(prefix)/lib/wasicaml
	rm -rf $(prefix)/lib/wasicaml/*
	cp -a wasi-sdk $(prefix)/lib/wasicaml/
	cp -a wasi-sdk-* $(prefix)/lib/wasicaml/
	cp -a wabt $(prefix)/lib/wasicaml/
	cp -a wabt-* $(prefix)/lib/wasicaml/

install-bin:
	mkdir -p $(prefix)/bin
	for f in $(TOOLS); do \
	    sed -e 's:%ROOT%:'$(prefix): -e 's:%PREFIX%:$(prefix):' bin-template/$$f >$(prefix)/bin/$$f; \
            chmod a+x $(prefix)/bin/$$f; \
	done

install-js:
	mkdir -p $(prefix)
	cp -a js $(prefix)/

install-ocaml:
	cd ocaml && make install

install-lib:
	cp lib/initwasi.o $(prefix)/lib
	cp lib/initruntime.o $(prefix)/lib

clean: clean-ocaml
	rm -f wasi_preamble
	rm -f lib/*.o
	rm -rf js/node_modules
	rm -f js/package-lock.json

clean-ocaml:
	cd ocaml && make clean

wasi_preamble: wasi_preamble.c
	cc -DPREFIX='"$(prefix)"' -o wasi_preamble wasi_preamble.c
