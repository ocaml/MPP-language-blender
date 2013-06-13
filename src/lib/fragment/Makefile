# OCAMLLIBS = str
# OCAMLLIBS_CMA = $(addsuffix .cma, $(OCAMLLIBS))
# OCAMLLIBS_CMXA = $(addsuffix .cmxa, $(OCAMLLIBS))

# OCAMLC = ocamlc $(OCAMLLIBS_CMA)
# OCAMLOPT = ocamlopt $(OCAMLLIBS_CMXA)

# FRAG_MLs = libmpp_frag.ml frag.ml


# frag:$(FRAG_CMOs)
# 	$(OCAMLC) -g -o $@ $+

# frag.opt:$(FRAG_CMXs)
# 	$(OCAMLOPT) -g -o $@ $+


frag:../../mpp_charset.cmo ../../mpp_charstream.cmo ../../mpp_out.cmo libmpp_fragment.ml mpp_tool_fragment.ml
	ocamlfind ocamlc -thread -package re -package core -linkpkg str.cma -I ../../ mpp_charset.cmo mpp_charstream.cmo mpp_out.cmo -o $@ libmpp_fragment.ml mpp_tool_fragment.ml

frag.opt:../../mpp_charset.cmx ../../mpp_charstream.cmx ../../mpp_out.cmx libmpp_fragment.ml mpp_tool_fragment.ml
	ocamlfind ocamlopt -thread -package re -package core -linkpkg str.cmxa -I ../../ mpp_charset.cmx mpp_charstream.cmx mpp_out.cmx -o $@ libmpp_fragment.ml mpp_tool_fragment.ml

../../mpp_charset.cmx ../../mpp_charstream.cmx ../../mpp_out.cmx:
	cd ../.. && make mpp_charset.cmx mpp_charstream.cmx mpp_out.cmx

../../mpp_charset.cmo ../../mpp_charstream.cmo ../../mpp_out.cmo:
	cd ../.. && make mpp_charset.cmo mpp_charstream.cmo mpp_out.cmo

