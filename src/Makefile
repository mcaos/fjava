OCAMLC=ocamlc
OCAMLOPT=ocamlopt
OCAMLDEP=ocamldep
OCAMLYACC=ocamlyacc
OCAMLLEX=ocamllex
MENHIR=menhir

PROGNAME=fjava

OBJS=parser.cmo lexer.cmo main.cmo

DEPEND += lexer.ml parser.ml

all: $(DEPEND) $(OBJS)
	$(OCAMLC) -o $(PROGNAME) $(OBJS)

.SUFFIXES: .ml .mli .cmo .cmi

.ml.cmo:
	$(OCAMLC) -c $<

.mli.cmi:
	$(OCAMLC) -c $<

parser.ml parser.mli: parser.mly
	@rm -f $@
	$(MENHIR) $<

lexer.ml: lexer.mll
	@rm -f $@
	$(OCAMLLEX) $<

# Clean up
clean:
	rm -f $(PROGNAME)
	rm -f *.cm[iox] *.o *~ parser.ml parser.mli parser.output lexer.ml .depend

# Dependencies
depend:: $(DEPEND)
	$(OCAMLDEP) $(INCLUDES) -native *.mli *.ml > .depend

-include .depend
