TEST_BUILD_DIR="_test_build"
COVERED_TEST_BUILD_DIR="_covered_test_build"
DOC_BUILD_DIR="_doc_build"
PACKAGES_INSTALL=ocephes lacaml lbfgs
PACKAGES_INSTALL_TEST=kaputt bisect_ppx dsfo

.PHONY: all clean test build full setup doc

default: FORCE
	@echo "available targets:"
	@echo "	build			compiles"
	@echo "	build_lite		compiles without C dependencies."
	@echo "	test			runs unit tests"
	@echo "	test_lite		test without C dependencies"
	@echo "	covered_test		runs unit tests with coverage"
	@echo "	covered_test_lite	runs unit tests without C dependencies with coverage"
	@echo "	setup			opam install packages necessary for full build"
	@echo "	setup_test		opam install packages necessary for testing"
	@echo "	doc			generates ocamldoc documentations"
	@echo "	clean			deletes all produced files"
	@echo "	report			generate Bisect_ppx coverage report"
	@echo " showed  generate showed.mli that contains the current interfaces"

# This should be called something else.
setup:
	opam install $(PACKAGES_INSTALL)

setup_test:
	opam pin add dsfo git://github.com/rleonid/dsfo
	opam install $(PACKAGES_INSTALL_TEST)

#### Building

build:
	ocaml pkg/pkg.ml build

build_lite:
	ocaml pkg/pkg.ml build --with-lacaml false --with-lbfgs false --with-ocephes false


clean:
	ocaml pkg/pkg.ml clean
	ocaml pkg/pkg.ml clean --build-dir $(TEST_BUILD_DIR)
	ocaml pkg/pkg.ml clean --build-dir $(COVERED_TEST_BUILD_DIR)
	ocaml pkg/pkg.ml clean --build-dir $(DOC_BUILD_DIR)

#### Testing

test:
	ocaml pkg/pkg.ml build --build-dir $(TEST_BUILD_DIR) -n omltest && \
		time ocaml pkg/pkg.ml test --build-dir $(TEST_BUILD_DIR)

test_lite:
	ocaml pkg/pkg.ml build --build-dir $(TEST_BUILD_DIR) --with-lacaml false --with-lbfgs false --with-ocephes false -n omltest && \
		time ocaml pkg/pkg.ml test --build-dir $(TEST_BUILD_DIR)

covered_test:
	ocaml pkg/pkg.ml build --build-dir $(COVERED_TEST_BUILD_DIR) --with-coverage true -n omltest && \
		time ocaml pkg/pkg.ml test --build-dir $(COVERED_TEST_BUILD_DIR)

covered_test_lite:
	ocaml pkg/pkg.ml build --build-dir $(COVERED_TEST_BUILD_DIR) --with-lacaml false --with-lbfgs false --with-ocephes false --with-coverage true -n omltest && \
		time ocaml pkg/pkg.ml test --build-dir $(COVERED_TEST_BUILD_DIR)


#### Test Coverage

report_dir:
	mkdir report_dir

# By running bisect-ppx-report in the actual directory with the modified source path
# (ie. the *.ml has the *.mlt inside of it with our label), we get proper
# alignment of the html!
report: report_dir
	cd $(COVERED_TEST_BUILD_DIR) && \
	bisect-ppx-report -html ../report_dir ../$(shell ls -t bisect*.out | head -1) && \
	cd -

clean_reports:
	rm -rf report_dir bisect*.out

#### Documentation

doc:
	ocamlbuild -classic-display -use-ocamlfind -plugin-tag 'package(str)' -no-links -build-dir $(DOC_BUILD_DIR) -docflags '-colorize-code,-charset,utf-8' doc/api.docdir/index.html

showed: build
	utop -require lacaml -require lbfgs -require ocephes -I _build/src oml.cma -I _build/src-full oml_full.cma tools/show.ml > showed.mli

# topkg doc --build-dir $(DOC_BUILD_DIR)
FORCE:
