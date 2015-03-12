
all: build

build:
	ocamlbuild src/oml/oml.cmo

.PHONY: all build
