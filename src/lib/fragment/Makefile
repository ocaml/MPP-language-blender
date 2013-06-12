OCAMLLIBS = str
OCAMLLIBS_CMA = $(addsuffix .cma, $(OCAMLLIBS))
OCAMLLIBS_CMXA = $(addsuffix .cmxa, $(OCAMLLIBS))

OCAMLC = ocamlc $(OCAMLLIBS_CMA)
OCAMLOPT = ocamlopt $(OCAMLLIBS_CMXA)

FRAG_MLs = libmpp_frag.ml frag.ml


frag:$(FRAG_CMOs)
	$(OCAMLC) -g -o $@ $+

frag.opt:$(FRAG_CMXs)
	$(OCAMLOPT) -g -o $@ $+


