
.PHONY: all clean build

all: build

oml.cmxa:
	ocamlbuild -I src/lib oml.cmo oml.cmx oml.cma oml.cmxa

build: oml.cmxa

driver.test: oml.cmxa
	ocamlbuild -use-ocamlfind -package kaputt -I src/lib -I src/test driver.test

test: driver.test
	./driver.test

clean:
	ocamlbuild -clean

install:
	ocamlfind install oml META \
		_build/src/lib/oml.a \
		_build/src/lib/oml.o \
		_build/src/lib/oml.cma \
		_build/src/lib/oml.cmxa \
		_build/src/lib/*.cmi \
		_build/src/lib/*.cmo
