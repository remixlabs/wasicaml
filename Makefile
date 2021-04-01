GITHUB_PREFIX = git@github.com:
# set GITHUB_PREFIX = https://github.com/ to check out anonymously

setup: setup-downloads setup-git setup-bin

setup-downloads:
	./install_wasi-sdk
	./install_wasmtime
	./install_wabt

# TODO: add as proper subpackage
setup-git:
	git clone $(GITHUB_PREFIX)/gerdstolpmann/ocaml.git
	git checkout gerd/wasi

setup-bin: lib/initwasi.o
	mkdir -p bin
	for f in wasi_cc wasi_ld wasi_ar wasi_ranlib wasi_nm wasi_strip wasi_objdump run; do \
	    sed -e 's:%ROOT%:'`pwd`: bin-template/$$f >bin/$$f; \
            chmod a+x bin/$$f; \
	done

configure:
	root=`pwd`; \
	cd ocaml; \
	./configure \
		CC="$$root/bin/wasi_cc" \
		LD="$$root/bin/wasi_ld" \
		AR="$$root/bin/wasi_ar" \
		NM="$$root/bin/wasi_nm" \
		RANLIB="$$root/bin/wasi_ranlib" \
		STRIP="$$root/bin/wasi_strip" \
		OBJDUMP="$$root/bin/wasi_objdump" \
		CPPFLAGS="-DCAML_USE_WASICAML -I$$root/include" \
		CFLAGS="-DCAML_USE_WASICAML -I$$root/include" \
		--disable-shared \
		--disable-systhreads \
		--disable-native-compiler \
		--host wasm32-unknown-wasi

lib/initwasi.o: src/initwasi.c
	mkdir -p lib
	./wasi-sdk/bin/clang --sysroot=wasi-sdk/share/wasi-sysroot \
		-Iinclude \
		-o lib/initwasi.o -c src/initwasi.c
