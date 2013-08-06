all:
	@echo \# To run all tests:
	@echo make testall
	@echo \# To run a specific test, e.g., the test f42:
	@echo make f42.test
	@echo \# To inspect a specific test, e.g., the test f42:
	@echo make f42.inspect
	@echo \# To clean the mess that the testing process has made:
	@echo make clean


testall:
	@echo
	@printf '\033[01;33m========== %s ==========\033[00m\n' 'Running all tests'
	@for i in *.run ; do make $$(basename $$i run)test ; done
	@echo

%.test:%.run
	@echo -ne Testing $< '\t\t'
	@PATH="../src:$${PATH}" bash $< && printf '\033[01;32m%s\033[00m\n' "=> PASSED" || printf '\033[01;31m%s\033[00m\t\033[01;31m%s\033[00m\n' "=> FAILED" "You might want to run make $(basename $<).inspect"

clean:
	rm -f *~ *.out *.expected *.mpp

%.inspect:
	@for i in $(basename $@)*[a-z] ; do echo ============================== $$i ; cat $$i ; done


registernewtests:
	git add *.run


