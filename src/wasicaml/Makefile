ML = wc_reader.ml
PKG = compiler-libs.common compiler-libs.bytecomp

all: wc.cma

wc.cma: $(ML)
	ocamlfind ocamlc -a -o wc.cma -g $(ML) -package "$(PKG)"

clean:
	rm -f *.cmi *.cmo *.cma main

main: wc.cma main.ml
	ocamlfind ocamlc -o main -g wc.cma main.ml -package "$(PKG)" -linkpkg
