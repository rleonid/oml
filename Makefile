
all: build

build:
	ocamlbuild src/oml/oml.cmo

install:
	ocamlfind install oml META _build/src/oml/oml.cmo

.PHONY: all build
