.PHONY: test

prefix=~/.wasicaml

test: testruntime
	./run

testruntime: testprint/testprint.cma
	$(prefix)/bin/ocamlc -make-runtime -o testruntime -cclib -Ltestprint testprint/testprint.cma

testprint/libtestprint.a: testprint/testprint_c.c
	cd testprint && $(prefix)/bin/ocamlc -c testprint_c.c
	rm -f testprint/libtestprint.a
	$(prefix)/bin/wasi_ar qs testprint/libtestprint.a testprint/testprint_c.o

testprint/testprint.cma: testprint/libtestprint.a testprint/testprint.ml
	cd testprint && $(prefix)/bin/ocamlc -a -o testprint.cma -cclib -ltestprint testprint.ml

clean:
	rm -f testprint/*.{cmi,cmo,cma,o,a}
	rm -f *.{cmi,cmo,cma,o,a}
	rm -f testruntime
