
all:frag
	@echo If you want to install frag, please run the following command:
	@echo PREFIX=/path/to/your/prefix make install

# Re will eventually replace Str.

frag.byte:../../mpp_charset.cmo ../../mpp_charstream.cmo ../../mpp_out.cmo libmpp_fragment.ml mpp_tool_fragment.ml
	ocamlfind ocamlc -thread -package str,re,core -linkpkg -I ../../ mpp_charset.cmo mpp_charstream.cmo mpp_out.cmo -o $@ libmpp_fragment.ml mpp_tool_fragment.ml

# Default uses ocamlopt because the Core library makes bytecode binaries too fat and too slow to load.
frag:frag.opt

frag.opt:../../mpp_charset.cmx ../../mpp_charstream.cmx ../../mpp_out.cmx libmpp_fragment.ml mpp_tool_fragment.ml
	ocamlfind ocamlopt -thread -package str,re,core -linkpkg -I ../../ mpp_charset.cmx mpp_charstream.cmx mpp_out.cmx -o $@ libmpp_fragment.ml mpp_tool_fragment.ml

../../mpp_charset.cmx ../../mpp_charstream.cmx ../../mpp_out.cmx:
	cd ../.. && make mpp_charset.cmx mpp_charstream.cmx mpp_out.cmx

../../mpp_charset.cmo ../../mpp_charstream.cmo ../../mpp_out.cmo:
	cd ../.. && make mpp_charset.cmo mpp_charstream.cmo mpp_out.cmo


install:frag.opt
	cp frag.opt ${PREFIX}/bin/

