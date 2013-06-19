testall:
	@echo
	@printf '\033[01;33m========== %s ==========\033[00m\n' 'Running all tests'
	@for i in *.run ; do make $$(basename $$i run)test ; done
	@echo

%.test:%.run
	@echo Testing $<
	@PATH="../src:$${PATH}" bash $< && printf '\033[01;32m%s\033[00m\n' "=> Result: successful" || printf '\033[01;31m%s\033[00m\n' "=> Result: unsuccessful"

clean:
	rm -f *~ *.out *.expected *.mpp

%.inspect:
	@for i in $(basename $@)* ; do echo ============================== $$i ; cat $$i ; done