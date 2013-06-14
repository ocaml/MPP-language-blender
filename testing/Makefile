testall:
	for i in *.run ; do make $$(basename $$i run)test ; done

%.test:%.run
	@echo test of $<: begin
	PATH="../src:$${PATH}" bash $<
	@echo test of $<: end


clean:
	rm -f *~ *.out
