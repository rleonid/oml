
TEST_BUILD_DIR=_test_build
# Used when building just lite
LITE_BUILD_DIR=_lite_build
DOC_BUILD_DIR=_doc_build
PACKAGES=lacaml lbfgs ocephes
PACKAGES_TEST=$(PACKAGES) kaputt dsfo
PACKAGES_COVERED:=$(PACKAGES_TEST) bisect_ppx
PACKAGES_INSTALL=$(PACKAGES)
PACKAGES_INSTALL_TEST=$(PACKAGES_COVERED)

SOURCE_DIRS=util unc stats cls rgr uns
# Do NOT install the cmo's, since we're packing oml_lite.cma into oml.cma.
INSTALL_EXTS=a o cma cmi cmt cmx cmxa cmxs

# One more and we might as well add a genuine configure step.
# Remember that the comma's in Make 'if' are space sensitive!
WITH_OML:=$(if $(shell ocamlfind query ocephes 2>/dev/null),0,1)
WITH_OML:=$(if $(shell ocamlfind query lacaml 2>/dev/null),$(WITH_OML),1)
WITH_OML:=$(if $(shell ocamlfind query lbfgs 2>/dev/null),$(WITH_OML),1)


# Str is necessary for building the documentation, which unfortunately, is in a
# half broken state because "include Module" logic doesn't work with OCamldoc.
OCAMLBUILD=ocamlbuild -use-ocamlfind -plugin-tag "package(str)"

.PHONY: all clean test build install uninstall setup default doc omltest.native oml.cmxa oml_lite.cmxa lite

default: FORCE
	@echo "available targets:"
	@echo "  build      	compiles Oml_lite and Oml if possible"
	@echo "  lite       	compiles only Oml_lite"
	@echo "  test       	runs unit tests"
	@echo "  doc        	generates ocamldoc documentations"
	@echo "  clean      	deletes all produced files"
	@echo "  setup      	opam install Oml dependencies"
	@echo "  setup-test 	opam install Oml and testing dependencies"
	@echo "  install    	copies executable and library files"
	@echo "  install-lite	copies executable and library files"
	@echo "  covered_test	runs unit tests with coverage"
	@echo "  report     	generate Bisect_ppx coverage report"


# This should be called something else.
setup:
	opam install $(PACKAGES_INSTALL)

setup-test:
	opam pin add dsfo git://github.com/rleonid/dsfo
	opam install $(PACKAGES_INSTALL_TEST)

#### Building

oml.cmxa:
	$(OCAMLBUILD) $(foreach package, $(PACKAGES),-package $(package)) \
		$(foreach d, $(SOURCE_DIRS), -I src/lib/$(d)) -I src/lib oml.cma oml.cmxa oml.cmxs

lite:
	$(OCAMLBUILD) -build-dir $(LITE_BUILD_DIR) \
		-I src/lib $(foreach d, $(SOURCE_DIRS), -I src/lib/$(d)) \
		oml_lite.cma oml_lite.cmxa oml_lite.cmxs

build:
ifeq (0, $(WITH_OML))
	$(OCAMLBUILD) $(foreach package, $(PACKAGES),-package $(package)) \
		$(foreach d, $(SOURCE_DIRS), -I src/lib/$(d)) -I src/lib \
		oml_lite.cma oml_lite.cmxa oml_lite.cmxs oml.cma oml.cmxa oml.cmxs
else
	$(OCAMLBUILD) \
		$(foreach d, $(SOURCE_DIRS), -I src/lib/$(d)) -I src/lib oml_lite.cma oml_lite.cmxa oml_lite.cmxs
endif

clean:
	$(OCAMLBUILD) -clean
	$(OCAMLBUILD) -build-dir $(TEST_BUILD_DIR) -clean
	$(OCAMLBUILD) -build-dir $(LITE_BUILD_DIR) -clean
	$(OCAMLBUILD) -build-dir $(DOC_BUILD_DIR) -clean

#### Testing

omltest.native:
	$(OCAMLBUILD) -build-dir $(TEST_BUILD_DIR) \
		$(foreach package, $(PACKAGES_TEST),-package $(package)) \
		$(foreach sd, $(SOURCE_DIRS), -I src/lib$(sd)) -I src/lib -I src/test omltest.native

test: omltest.native
	time ./omltest.native ${TEST}

covered_test.native:
	$(OCAMLBUILD) -build-dir $(TEST_BUILD_DIR) \
		$(foreach package, $(PACKAGES_COVERED),-package $(package)) \
		$(foreach sd, $(SOURCE_DIRS), -I src/lib$(sd)) -I src/lib -I src/test omltest.native

covered_test: covered_test.native
	time ./omltest.native ${TEST}

test_environment:
	$(OCAMLBUILD) -build-dir $(TEST_BUILD_DIR) \
		$(foreach package, $(PACKAGES_COVERED),-package $(package)) \
		-I src/lib -I src/test oml.cma omltest.native

#### Installing

install:
ifeq (0, $(WITH_OML))
	cd pkg && ocamlfind install oml META $(foreach ext, $(INSTALL_EXTS), ../_build/src/lib/oml.$(ext)) \
		$(foreach ext, $(INSTALL_EXTS), ../_build/src/lib/oml_lite.$(ext))
else
	cd pkg && ocamlfind install oml META $(foreach ext, $(INSTALL_EXTS), ../_build/src/lib/oml_lite.$(ext))
endif

uninstall:
	ocamlfind remove oml

install-lite:
	cd pkg/lite && ocamlfind install oml-lite META $(foreach ext, $(INSTALL_EXTS), ../../${LITE_BUILD_DIR}/src/lib/oml_lite.$(ext))

uninstall-lite:
	ocamlfind remove oml-lite

#### Test Coverage

report_dir:
	mkdir report_dir

# By running bisect-ppx-report in the actual directory with the modified source path
# (ie. the *.ml has the *.mlt inside of it with our label), we get proper
# alignment of the html!
report: report_dir
	cd $(TEST_BUILD_DIR) && \
	bisect-ppx-report -html ../report_dir ../$(shell ls -t bisect*.out | head -1) && \
	cd -

clean_reports:
	rm -rf report_dir bisect*.out


#### Documentation

oml.odocl:
	cp src/lib/oml.mlpack oml.odocl

# including the cmi as build targets triggers all of the including logic
# to get saner documentation.
doc:
	$(OCAMLBUILD) -build-dir $(DOC_BUILD_DIR) \
		$(foreach package, $(PACKAGES),-package $(package)) \
		$(foreach sd, $(SOURCE_DIRS), -I src/lib$(sd)) -I src/lib oml.cmi doc.docdir/index.html

FORCE:
