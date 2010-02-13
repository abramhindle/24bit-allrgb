include ./Makefile.camlimages

COMPFLAGS= -I `ocamlfind query camlimages` $(COMPFLAG_CAMLIMAGES)
LINKOPT=$(LINKFLAGS_CAMLIMAGES) -I `ocamlfind query camlimages` 
munkres.cmx: munkres.ml
	ocamlopt.opt -c munkres.ml

permuter: permuter.ml munkres.cmx
	ocamlopt.opt -o permuter ${COMPFLAGS} ${LINKOPT} munkres.cmx permuter.ml

test: permuter
	./permuter fp1.jpg 32 0 32

bigtest: permuter
	./permuter fp1.jpg 8 0 4096
