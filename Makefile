OCAMLC=metaocamlc
OCAMLOPT=metaocamlopt
OCAMLDEP=ocamldep
OCAMLFNC=ocamlfind -toolchain metaocaml ocamlc -package asp
OCAMLFNO=ocamlfind -toolchain metaocaml ocamlopt -package asp

.SUFFIXES: .ml .mli .cmo .cmi .cmx

LIB=my_parser.cma
LIBOPT=my_parser.cmxa

OBJS=type.cmo pair.cmo tokens_base.cmo tokens.cmo my_parser.cmo

.PHONY: all
# all: depend lib opt
all: depend lib


lib: $(LIB)

opt: $(LIBOPT)

.PHONY: intf			#Just the interfaces for testing examples
intf: $(INTF)

$(LIB): $(INTF) $(OBJS)
	$(OCAMLFNC) -a $(INCLUDE) -o $@ -linkpkg $(filter-out %.cmi,$^)

$(LIBOPT): $(INTF) $(OBJS:.cmo=.cmx)
	$(OCAMLFNO) -a -o $@ $(filter-out %.cmi,$^)


.mli.cmi:
	$(OCAMLFNC) -c $(INCLUDE)  $<
.ml.cmo:
	$(OCAMLFNC) -c $(INCLUDE) $<
.ml.cmx:
	$(OCAMLFNO) -c $(INCLUDE) $<

depend: 
	$(OCAMLDEP) *.mli *.ml > .depend

clean::
	rm -f *.cm[ixoa] *.cmxa *.[oa] .depend

-include .depend

