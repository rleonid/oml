travis_install_on_linux () {
  sudo apt-get install liblapack-dev
  sudo apt-get install gfortran       # for lbgfs
  sudo apt-get install libffi-dev     # for Ctypes
}

travis_install_on_osx () {
    echo "brew install lapack"
    brew install homebrew/dupes/lapack > /dev/null
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
echo Installing basic deps
opam install ocamlfind topkg ocamlbuild

#echo Installing Libraries
#make setup-test

echo Compiling
make build

echo Installing Testing libs
opam install kaputt
opam pin add dsfo git://github.com/rleonid/dsfo

echo Testing
make test

echo Installing C and Fortran deps
opam install ocephes lacaml lbfgs

echo Compiling with C/Fortran deps
make build

echo Install bisect ppx and ocveralls for coverage reporting
opam install bisect_ppx ocveralls

echo Testing with C/Fortran dest
make covered_test
ocveralls --repo_token $COVERALLSTOKEN --git --send ../bisect0001.out
