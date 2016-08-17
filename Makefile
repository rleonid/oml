
DRIVER_BUILD_DIR=_driver
PACKAGES=lacaml lbfgs ocephes
PACKAGES_TEST=$(PACKAGES) kaputt dsfo
PACKAGES_COVERED:=$(PACKAGES_TEST) bisect_ppx
PACKAGES_INSTALL=$(PACKAGES_COVERED)

SOURCE_DIRS=/util /unc /stats /cls /rgr /uns

.PHONY: all clean test build install uninstall setup default doc driver.test

default: build

all: build

# This should be called something else.
setup:
	opam pin add dsfo git://github.com/rleonid/dsfo
	opam install $(PACKAGES_INSTALL)

oml.cmxa:
	ocamlbuild -use-ocamlfind $(foreach package, $(PACKAGES),-package $(package)) -I src/lib oml.cma oml.cmxa oml.cmxs

build: oml.cmxa

driver.test:
	ocamlbuild -build-dir $(DRIVER_BUILD_DIR) \
		-use-ocamlfind $(foreach package, $(PACKAGES_TEST),-package $(package)) \
		-I src/lib $(foreach sd, $(SOURCE_DIRS), -I src/lib$(sd)) -I src/test driver.test

test: driver.test
	time ./driver.test ${TEST}

covered_driver.test:
	ocamlbuild -build-dir $(DRIVER_BUILD_DIR) \
		-use-ocamlfind $(foreach package, $(PACKAGES_COVERED),-package $(package)) \
		-I src/lib $(foreach sd, $(SOURCE_DIRS), -I src/lib$(sd)) -I src/test driver.test

covered_test: covered_driver.test
	time ./driver.test ${TEST}

test_environment:
	ocamlbuild -build-dir $(DRIVER_BUILD_DIR) \
		-use-ocamlfind $(foreach package, $(PACKAGES_COVERED),-package $(package)) \
		-I src/lib -I src/test oml.cma driver.test

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

oml.odocl:
	cp src/lib/oml.mlpack oml.odocl

doc: oml.odocl
	ocamlbuild -use-ocamlfind $(foreach package, $(PACKAGES),-package $(package)) -I src/lib  oml.docdir/index.html
