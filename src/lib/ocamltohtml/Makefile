MPP_LIB =  ../../mpp_charset.ml ../../mpp_charstream.ml ../../mpp_out.ml ../../mpp_stringmap.ml ../../mpp_actions.ml 
MPP_CMOs = $(addsuffix .cmo, $(basename $(MPP_LIB)))
MPP_CMXs = $(addsuffix .cmx, $(basename $(MPP_LIB)))

OCAMLC   = ocamlc -I ../../
OCAMLOPT = ocamlopt  -I ../../

all : ocamltohtml

ocamltohtml : ocamltohtml_lexer.cmo ocamltohtml_main.cmo
	$(OCAMLC) $(MPP_CMOs) $+ -o ocamltohtml

ocamltohtml.opt : ocamltohtml_lexer.cmx ocamltohtml_main.cmx
	$(OCAMLOPT) $(MPP_CMXs) $+ -o ocamltohtml

%.cmo:%.ml
	$(OCAMLC) -c $<

%.cmx:%.ml
	$(OCAMLOPT) -c $<

ocamltohtml_main.cmx:ocamltohtml_lexer.cmx
ocamltohtml_main.cmo:ocamltohtml_lexer.cmo

ocamltohtml_lexer.ml:ocamltohtml_lexer.mll
	ocamllex $<

clean:
	rm -f *~ *.cm[xoi]  *.o ocamltohtml ocamltohtml_lexer.ml


all2:
	ocamllex ocamltohtml_lexer.mll  && ocamlc ocamltohtml_lexer.ml && ocamlc ocamltohtml_lexer.cmo  ocamltohtml_main.ml -o ocamltohtml


