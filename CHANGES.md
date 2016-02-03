Version 0.0.5 (2016-02-03):
---------------------------
  - Refine Logistic Regression to tune gradient calculation in L-BFGS.
    - Changed to a more general softmax algorithm.
    - Split apart binary vs multiclass.
    - MNIST training script.
  - Finish exposing regression capabilities.
    - Coefficient test and stastics.
  - Add Dirichlet.
  - Running -> Online now functorized.
  - Move datasets out of Oml
  - Hierarchical reorganization into 6 principal packs.
    - Cleaned up the 'optional' argument logic, now all of these
      types are contained in the module/functor that uses them.
    - Build system changed for testing and documentation logic.
      No more joiner. mli's are custom generated for subpacks.

Version 0.0.3 (2015-10-14):
---------------------------
  - Clarify the Regression interface with `Optional_arg_intf`.
  - Add Cubic spline interpolation.
  - Array.zip/unzip
  - fst3,snd3,thr3

Version 0.0.2 (2015-09-09):
---------------------------
  - Clean up the Classify API via a functor.
  - Clean up test suite to use bounded floats and
    have a simpler testing framework.
  - Add bisection method.

Version 0.0.1 (2015-08-06):
---------------------------
  - First public release: ready for other people to start hacking.
