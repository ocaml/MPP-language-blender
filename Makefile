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

clean:
	cd src && make clean
	cd snapshots && make clean
	cd testing && make clean

snapshot:
	cd snapshots && make

tests:
	cd testing && make testall

