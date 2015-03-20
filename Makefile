
all: build

clean:
	rm -rf _build

setup.ml:
	oasis setup -setup-update dynamic

setup.data:
	ocaml setup.ml -configure

build: setup.ml setup.data
	ocaml setup.ml -build
	
install:
	ocamlfind install oml META \
		_build/src/lib/oml.a \
		_build/src/lib/oml.o \
		_build/src/lib/oml.cma \
		_build/src/lib/oml.cmxa \
		_build/src/lib/*.cmi \
  	_build/src/lib/*.cmo


.PHONY: all clean build install
