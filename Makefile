all:
	@echo '# I suggest you run the following command:'
	@echo 'cd src && make'
	@echo '# Or this one:'
	@echo 'make install'

install:
	cd src && make install

installall:
	cd src && make installall

mpp:
	cd src && make mpp
frag:
	cd src && make frag
