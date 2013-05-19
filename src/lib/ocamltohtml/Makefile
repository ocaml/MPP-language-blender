all : ocamltohtml

ocamltohtml : ocamltohtml_lexer.cmo ocamltohtml_main.cmo
	ocamlc $+ -o ocamltohtml

ocamltohtml.opt : ocamltohtml_lexer.cmx ocamltohtml_main.cmx
	ocamlopt $+ -o ocamltohtml

%.cmo:%.ml
	ocamlc -c $<

%.cmx:%.ml
	ocamlopt -c $<

ocamltohtml_main.cmx:ocamltohtml_lexer.cmx
ocamltohtml_main.cmo:ocamltohtml_lexer.cmo

ocamltohtml_lexer.ml:ocamltohtml_lexer.mll
	ocamllex $<

clean:
	rm -f *~ *.cm[xoi]  *.o ocamltohtml ocamltohtml_lexer.ml


all2:
	ocamllex ocamltohtml_lexer.mll  && ocamlc ocamltohtml_lexer.ml && ocamlc ocamltohtml_lexer.cmo  ocamltohtml_main.ml -o ocamltohtml
