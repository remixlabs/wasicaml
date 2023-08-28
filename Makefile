-include Makefile.config

ifndef prefix
$(error Run ./configure first)
endif

WASI_SDK = lib/wasi-sdk
WASI_LIBC_REPO = https://github.com/gerdstolpmann/wasi-libc.git
WASI_LIBC_BRANCH =
TOP := $(realpath .)

.PHONY: default \
        setup setup-downloads setup-git setup-bin setup-js \
        configure configure-ocaml \
	build build-ocaml \
	build2 build-wasicaml \
	install install-downloads install-bin install-ocaml install-lib \
	install2 install-wasicaml \
	clean clean-ocaml clean-wasicaml \
	test test-wasicaml

default: setup configure build install build2 install2

setup: setup-git setup-downloads setup-bin setup-js

setup-downloads:
	./install_wasi-sdk
	if [ -n "$(WASI_LIBC_BRANCH)" ]; then $(MAKE) build-wasi-libc; fi

# ./install_wabt
# ./install_wasmtime

.PHONE: build-wasi-libc
build-wasi-libc:
	if [ ! -d wasi-libc ]; then \
	    git clone $(WASI_LIBC_REPO) && \
	    cd wasi-libc && \
	    git checkout $(WASI_LIBC_BRANCH); \
	else \
	    cd wasi-libc && \
	    git fetch && \
	    git checkout $(WASI_LIBC_BRANCH) && \
	    git merge --ff-only FETCH_HEAD; \
	fi
	cd wasi-libc && $(MAKE) WASM_CC=$(TOP)/lib/wasi-sdk/bin/clang WASM_AR=$(TOP)/lib/wasi-sdk/bin/llvm-ar WASM_NM=$(TOP)/lib/wasi-sdk/bin/llvm-nm
	rm -rf lib/wasi-sdk/share/wasi-sysroot.old
	mv lib/wasi-sdk/share/wasi-sysroot lib/wasi-sdk/share/wasi-sysroot.old
	cp -a wasi-libc/sysroot lib/wasi-sdk/share/wasi-sysroot

setup-git:
	git submodule update --init --progress

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
	cp include/wasicaml.h ocaml/runtime/caml/; \
	cd ocaml; \
	./configure \
		CC="$(prefix)/bin/wasi_cc" \
		LD="$(prefix)/bin/wasi_ld" \
		AR="$(prefix)/bin/wasi_ar" \
		NM="$(prefix)/bin/wasi_nm" \
		RANLIB="$(prefix)/bin/wasi_ranlib" \
		STRIP="$(prefix)/bin/wasi_strip" \
		OBJDUMP="$(prefix)/bin/wasi_objdump" \
		CPPFLAGS="-DCAML_USE_WASICAML -DWASICAML_PROVIDE_SYSTEM -DWASICAML_PROVIDE_RENAME" \
		CFLAGS="-DCAML_USE_WASICAML -DWASICAML_PROVIDE_SYSTEM -DWASICAML_PROVIDE_RENAME" \
		--disable-shared \
		--disable-systhreads \
		--disable-native-compiler \
		--disable-ocamltest \
		--disable-ocamldoc \
		--host wasm32-unknown-wasi \
		--prefix=$(prefix)
	echo PRIMS_IR=true >> ocaml/Makefile.config

LIBS = \
	lib/initwasi.o \
	lib/initwasi_eh.o \
	lib/initwasi_eh_helpers.o \
	lib/initruntime.o

lib/initwasi.o: src/initwasi.c
	mkdir -p lib
	$(WASI_SDK)/bin/clang --sysroot=$(WASI_SDK)/share/wasi-sysroot \
		-Iinclude \
		-O -o lib/initwasi.o -c src/initwasi.c

lib/initwasi_eh.o: src/initwasi.c
	mkdir -p lib
	$(WASI_SDK)/bin/clang --sysroot=$(WASI_SDK)/share/wasi-sysroot \
		-Iinclude -DWASM_EH -mexception-handling \
		-O -o lib/initwasi_eh.o -c src/initwasi.c

lib/initwasi_eh_helpers.o: src/initwasi_eh_helpers.s
	mkdir -p lib
	$(WASI_SDK)/bin/clang --sysroot=$(WASI_SDK)/share/wasi-sysroot \
		-mexception-handling \
		-O -o lib/initwasi_eh_helpers.o -c src/initwasi_eh_helpers.s

lib/initruntime.o: src/initruntime.c
	mkdir -p lib
	$(WASI_SDK)/bin/clang --sysroot=$(WASI_SDK)/share/wasi-sysroot \
		-Iocaml/runtime \
		-O -o lib/initruntime.o -c src/initruntime.c

build: lib/initruntime.o build-ocaml build-wasicaml

build-ocaml:
	cd ocaml && make

build2: build-wasicaml
	cd src/wasicaml && make

install: install-downloads install-bin install-js install-ocaml install-lib

install-downloads:
	mkdir -p $(prefix)/lib/wasicaml
	rm -rf $(prefix)/lib/wasi-sdk
	rm -rf $(prefix)/lib/wasi-sdk-*
	cp -a lib/wasi-sdk $(prefix)/lib/
	cp -a lib/wasi-sdk-* $(prefix)/lib/

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

install-lib: $(LIBS)
	cp $(LIBS) $(prefix)/lib

install2: install-wasicaml

install-wasicaml:
	cp src/wasicaml/wasicaml $(prefix)/bin/

clean: clean-ocaml clean-wasicaml
	rm -f wasi_preamble
	rm -f lib/*.o
	rm -rf js/node_modules
	rm -f js/package-lock.json
	#if [ -d wasi-libc ]; then cd wasi-libc; $(MAKE) clean; fi

clean-ocaml:
	cd ocaml && make clean

clean-wasicaml:
	cd src/wasicaml && make clean
	cd test && make clean

wasi_preamble: wasi_preamble.c
	cc -DPREFIX='"$(prefix)"' -o wasi_preamble wasi_preamble.c

test: test-wasicaml

test-wasicaml:
	cd test && make

test-tail-calls:
	cd test && ./run -tail-call -multivalue

test-exc-handling:
	cd test && ./run -eh -multivalue
