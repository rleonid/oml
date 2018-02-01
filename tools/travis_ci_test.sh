travis_install_on_linux () {
  sudo apt-get install liblapack-dev
  sudo apt-get install gfortran       # for lbgfs
  sudo apt-get install libffi-dev     # for Ctypes
}

travis_install_on_osx () {
  echo "brew install lapack"
  brew install lapack > /dev/null
  echo "brew install gcc"
  brew install gcc > /dev/null  # for gfortran
  echo "brew install libffi"
  brew install libffi > /dev/null # for ocephes
  echo "brew install finished!"
}

case $TRAVIS_OS_NAME in
  osx) travis_install_on_osx ;;
  linux) travis_install_on_linux ;;
  *) echo "Unknown $TRAVIS_OS_NAME"; exit 1
esac

eval `opam config env`
export OPAMYES="true"
echo ----------Installing basic deps----------
opam install ocamlfind topkg ocamlbuild

#echo Installing Libraries
#make setup-test

echo ----------Compiling lite----------
make build_lite

echo ----------Installing Testing libs----------
opam install kaputt
opam pin add dsfo git://github.com/rleonid/dsfo

echo ----------Testing lite----------
make test_lite

echo ----------Install bisect ppx and ocveralls for coverage reporting----------
echo ----------Installing C and Fortran deps----------
opam install bisect_ppx ocveralls ocephes lacaml.9.2.2 lbfgs

echo ----------Compiling full----------
make build

echo ----------Examples ----
make examples

echo ----------Testing full----------
make covered_test
cd _covered_test_build
ocveralls --repo_token $COVERALLSTOKEN --git --send ../bisect0001.out
