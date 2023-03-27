.PHONY: test test_opt

include ../Makefile.config

test: testruntime
	./run

# This test uses the ocamlopt compiler of the host (which also must be 4.12.0)
# as reference:
test_opt: testprint_opt/testprint.cmxa
	./run -ocamlopt

testruntime: testprint/testprint.cma
	$(prefix)/bin/ocamlc -make-runtime -o testruntime -cclib -Ltestprint testprint/testprint.cma

testprint/libtestprint.a: testprint/testprint_c.c
	cd testprint && $(prefix)/bin/ocamlc -c testprint_c.c
	rm -f testprint/libtestprint.a
	$(prefix)/bin/wasi_ar qs testprint/libtestprint.a testprint/testprint_c.o

testprint_opt/libtestprint.a: testprint/testprint_c.c
	mkdir -p testprint_opt
	cp testprint/testprint_c.c testprint_opt/
	cd testprint_opt && ocamlopt -c testprint_c.c
	rm -f testprint_opt/libtestprint.a
	ar qs testprint_opt/libtestprint.a testprint_opt/testprint_c.o

testprint/testprint.cma: testprint/libtestprint.a testprint/testprint.ml
	cd testprint && $(prefix)/bin/ocamlc -a -o testprint.cma -cclib -ltestprint testprint.ml

testprint_opt/testprint.cmxa: testprint_opt/libtestprint.a testprint_opt/testprint.ml
	cd testprint_opt && ocamlopt -a -o testprint.cmxa -cclib -ltestprint testprint.ml

testprint_opt/testprint.ml: testprint/testprint.ml
	mkdir -p testprint_opt
	cp testprint/testprint.ml testprint_opt/

clean:
	rm -f testprint/*.{cmi,cmo,cma,cmx,cmxa,o,a}
	rm -rf testprint_opt
	rm -f *.{cmi,cmo,cma,cmx,cmxa,o,a}
	rm -f testruntime
