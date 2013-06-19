all:
	@T="MPP-language-blender/snapshots/MPP-snapshot-$$(date +%Y%m%d-%H%M%S).tgz" ;\
	 echo "# New snapshot: $$(basename $$T)" ; \
	 cd ../.. && tar czf $$T MPP-language-blender/src/*.ml MPP-language-blender/src/Makefile MPP-language-blender/README.md MPP-language-blender/src/lib/fragment/* MPP-language-blender/src/lib/ocamltohtml/* ; \
	 echo "# To display the list of files:" ; echo "tar tzf $$(basename $$T)" ; \
	 echo "# To add the snapshot: " ; echo "git add $$(basename $$T)"


