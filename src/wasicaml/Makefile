ML = wc_types.ml wc_util.ml \
     wc_reader.mli wc_reader.ml \
     wc_control.mli wc_control.ml \
     wc_number.ml \
     wc_sexp.ml \
     wc_emit.ml \
     wc_sexp2s.ml
PKG = compiler-libs.common compiler-libs.bytecomp

all: wc.cma

wc.cma: $(ML)
	ocamlfind ocamlc -a -o wc.cma -g $(ML) -package "$(PKG)"

clean:
	rm -f *.cmi *.cmo *.cma main

main: wc.cma main.ml
	ocamlfind ocamlc -o main -g wc.cma main.ml -package "$(PKG)" -linkpkg
