OCaml Math Library
------------------

[![Build Status](https://travis-ci.org/hammerlab/oml.svg?branch=master)](https://travis-ci.org/hammerlab/oml/)
[![Coverage Status](https://coveralls.io/repos/hammerlab/oml/badge.svg?branch=HEAD&service=github)](https://coveralls.io/github/hammerlab/oml?branch=HEAD)
[![Gitter](https://badges.gitter.im/Join%20Chat.svg)](https://gitter.im/hammerlab/oml?utm_source=badge&utm_medium=badge&utm_campaign=pr-badge)


A collection of OCaml Math and Statistics functions.
The API is available [online](http://hammerlab.github.io/oml/index.html).

### Goals

  1. Perform simple and sophisticated mathematical and statistical analysis
      inside of (mostly) OCaml. Please see [Oml_lite](#oml_lite) for details.
  2. Have a descriptive, simple, and typed approach to those algorithms.
      - _descriptive_: what a function does should be easy to understand from
        the type and name.
      - _simple_: don't worry about the corner cases of the algorithms, until
        you have to.
      - __typed__: obviously.
  3. Well tested. There are a subtle dependency between `float` capabilities
     and these algorithms. The test suite aims to provide bounds as well as a
     benchmark for comparing algorithms.
  4. Informative examples.

### Building

  - `make setup` will `opam install` the necessary packages for Oml.
    [Oml_lite](#oml_lite) has no dependencies.
  - `make` will compile source.
  - `make test
  - `make test` for tests.
        - We use [Kaputt](http://kaputt.x9c.fr/) as the testing framework. Tests
        are found in `*.mlt` files and are joined with their respective source
        files only when we build a test target.
        - `make TEST={ModuleName} test` will run the test in `ModuleName`,
           ex `make TEST=Descriptive test`
        - `make setup-test` will install packages necessary for testing.
  - `make covered_test` for [Bisect_ppx](https://github.com/rleonid/bisect_ppx)
      instrumented test coverage.

### <a name="oml_lite">Oml_lite</a>

  Oml_lite is a subset of Oml that excludes any code with a `C` or `Fortran`
  dependency.

### Dependencies

  `make setup` will `opam install` these:

  - [Lacaml](https://github.com/mmottl/lacaml) for BLAS/LAPACK bindings.
  - [LBFGS](https://github.com/Chris00/L-BFGS-ocaml) for bindings to LBFGS
      optimization routines.
  - [ocephes](https://github.com/rleonid/ocephes) for special functions.
  - [Kaputt](http://kaputt.x9c.fr/) for testing.
  - [Bisect_ppx](https://github.com/rleonid/bisect_ppx) for code coverage.
      - See [Bisect](http://bisect.x9c.fr/) for initial implementation
      - and [Ocveralls](https://github.com/sagotch/ocveralls/) for pushing to
          [Coveralls](https://coveralls.io/).

### Contributing

Anything within the problem domain is welcome.

#### Questions, Bugs, Issues

The best place to discuss Oml is within Github's
[issues](https://github.com/hammerlab/oml/issues) (even for questions).

#### Guideline

"Favor readers over writers" and _Favor users over readers_.
