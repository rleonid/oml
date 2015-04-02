
DRIVER_BUILD_DIR=_driver

.PHONY: all clean build install uninstall setup default

default: build

all: build

# This should be called something else.
setup:
	opam install kaputt
	opam install bisect

oml.cmxa:
	ocamlbuild -I src/lib oml.cmo oml.cmx oml.cma oml.cmxa oml.cmxs

build: oml.cmxa

joiner.native:
	ocamlbuild -build-dir $(DRIVER_BUILD_DIR) -I tools joiner.native

driver.test: joiner.native
	ocamlbuild -build-dir $(DRIVER_BUILD_DIR) -use-ocamlfind -package kaputt -I src/lib -I src/test driver.test

test: driver.test
	./driver.test

clean:
	ocamlbuild -clean
	ocamlbuild -build-dir $(DRIVER_BUILD_DIR) -clean
	rm -rf driver.test

install:
	ocamlfind install oml META \
		_build/src/lib/oml.a \
		_build/src/lib/oml.o \
		_build/src/lib/oml.cma \
		_build/src/lib/oml.cmxa \
		_build/src/lib/oml.cmxs \
		_build/src/lib/*.cmi \
		_build/src/lib/*.cmo \
		_build/src/lib/*.cmx

uninstall:
	ocamlfind remove oml

report_dir:
	mkdir report_dir

report: report_dir
	bisect-report -I $(DRIVER_BUILD_DIR) -html report_dir $(shell ls -t bisect*.out | head -1)

clean_reports:
	rm -rf report_dir bisect*.out
