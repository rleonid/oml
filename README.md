OCaml Math Library
------------------

[![Build Status](https://travis-ci.org/rleonid/oml.svg)](https://travis-ci.org/rleonid/oml)
[![Coverage Status](https://coveralls.io/repos/rleonid/oml/badge.svg?branch=HEAD&service=github)](https://coveralls.io/github/rleonid/oml?branch=HEAD)


A collection of OCaml Math and Statistics functions.

### Goals

  1. Perform simple and sophisticated mathematical and statistical analysis
      inside of (mostly) OCaml.
  2. Have a descriptive, simple, and typed approach to those algorithms.
      - _descriptive_: what a function does should be easy to understand from
        the type and name.
      - _simple_: don't worry about the corner cases of the alogrithms, until
        you have to.
      - __typed__: obviously.
  3. Well tested. There are a subtle dependency between `float` capabilities
     and these algorithms. The test suite aims to provide bounds as well as a
     benchmark for comparing algorithms.
  4. Informative examples.

### Building

  - `make` will compile source.
  - `make test` for tests.
        We use [Kaputt](http://kaputt.x9c.fr/) as the testing framework. Tests
        are found in `*.mlt` files and are joined with their respective source
        files only when we build a test target.
  - `make covered_test` for [Bisect](http://bisect.x9c.fr/) instrumented test
      coverage.

### Dependencies

  `make setup` will `opam install` these:

  - [Lacaml](https://github.com/mmottl/lacaml) for BLAS/LAPACK bindings.
  - [Kaputt](http://kaputt.x9c.fr/) for testing.
  - [Bisect](http://bisect.x9c.fr/) for coverage of tests.

