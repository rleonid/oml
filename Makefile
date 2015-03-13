
all: build

build:
	ocamlbuild src/oml/oml.cma

install:
	ocamlfind install oml META \
		_build/src/oml/oml.cma \
		_build/src/oml/oml.mlpack \
		_build/src/oml/*.cmi \
		_build/src/oml/*.cmo


.PHONY: all build
