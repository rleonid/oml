
DRIVER_BUILD_DIR=_driver
PACKAGES=ctypes ctypes.foreign lacaml lbfgs
PACKAGES_TEST=$(PACKAGES) kaputt
PACKAGES_COVERED=$(PACKAGES_TEST) bisect_ppx
PACKAGES_INSTALL=$(subst .,-,$(PACKAGES_COVERED))

.PHONY: all clean build install uninstall setup default

default: build

all: build

# This should be called something else.
setup:
	opam install $(PACKAGES_INSTALL)

oml.cmxa:
	ocamlbuild -use-ocamlfind $(foreach package, $(PACKAGES),-package $(package)) -I src/lib/datasets data.cma data.cmxa data.cmxs -I src/lib oml.cma oml.cmxa oml.cmxs

build: oml.cmxa

joiner.native:
	ocamlbuild -build-dir $(DRIVER_BUILD_DIR) -I tools joiner.native

test: joiner.native
	ocamlbuild -build-dir $(DRIVER_BUILD_DIR) -use-ocamlfind $(foreach package, $(PACKAGES_TEST),-package $(package)) -I src/lib/datasets -I src/lib -I src/test driver.test && \
	time ./driver.test

covered_test: joiner.native
	ocamlbuild -build-dir $(DRIVER_BUILD_DIR) -use-ocamlfind $(foreach package, $(PACKAGES_COVERED),-package $(package)) -I src/lib/datasets -I src/lib -I src/test driver.test && \
	time ./driver.test

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

# By running bisect-ppx-report in the actual directory with the modified source path
# (ie. the *.ml has the *.mlt inside of it with our label), we get proper
# alignment of the html!
report: report_dir
	cd $(DRIVER_BUILD_DIR) && \
	bisect-ppx-report -html ../report_dir ../$(shell ls -t bisect*.out | head -1) && \
	cd -

clean_reports:
	rm -rf report_dir bisect*.out
