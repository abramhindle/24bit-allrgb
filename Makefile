include ./Makefile.camlimages

COMPFLAGS= -I `ocamlfind query camlimages` $(COMPFLAG_CAMLIMAGES)
LINKOPT=$(LINKFLAGS_CAMLIMAGES) -I `ocamlfind query camlimages` 
munkres.cmx: munkres.ml
	ocamlopt.opt -c munkres.ml

permuter: permuter_driver.ml permuter.cmx munkres.cmx hilbert.cmx
	ocamlopt.opt -o permuter ${COMPFLAGS} ${LINKOPT} munkres.cmx hilbert.cmx permuter.cmx permuter_driver.ml

permuter.cmx: permuter.ml munkres.cmx hilbert.cmx
	ocamlopt.opt ${COMPFLAGS} munkres.cmx hilbert.cmx -c permuter.ml

verify: verify.ml permuter.cmx munkres.cmx hilbert.cmx
	ocamlopt.opt -o verify ${COMPFLAGS} ${LINKOPT} munkres.cmx hilbert.cmx permuter.cmx verify.ml

hilbert.cmx: hilbert.ml
	ocamlopt.opt ${COMPFLAGS} -c hilbert.ml

vtest: permuter
	./permuter valentines.png 32 0 32
test: permuter
	./permuter fp1.jpg 32 0 32
ctest: permuter
	./permuter circles.png 32 0 32
htest: permuter
	./permuter hilbert-curve.png 32 0 32

vtest: permuter
	./permuter valentines.png 32 0 32


bigtest: permuter
	./permuter circles.png 8 0 4096

bigvtest: permuter
	./permuter valentines.png 8 0 4096

