-include ../../Makefile.config

ML = wc_types.ml wc_util.ml wc_config.ml \
     wc_reader.mli wc_reader.ml \
     wc_traceinstr.ml \
     wc_control.mli wc_control.ml \
     wc_number.ml \
     wc_sexp.ml \
     wc_prims.ml \
     wc_tracestack.ml \
     wc_traceglobals.ml \
     wc_tracefuncs.ml \
     wc_instruct.ml wc_unstack.ml wc_emit.ml \
     wc_sexp2s.ml
LIBS = ocamlcommon.cma ocamlbytecomp.cma
OCAMLC = $(prefix)/bin/ocamlc

all: wasicaml

wc.cma: $(ML)
	$(OCAMLC) -a -o wc.cma -g -I +compiler-libs $(ML)

clean:
	rm -f *.cmi *.cmo *.cma wasicaml

wasicaml: wc_driver.ml wc.cma
	$(OCAMLC) -o wasicaml -g -I +compiler-libs $(LIBS) wc.cma wc_driver.ml
	@# bug: WASI chmod seems not to be working
	chmod a+x wasicaml
