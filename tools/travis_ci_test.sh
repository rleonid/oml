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
opam install ocamlbuild
opam pin add dsfo git://github.com/rleonid/dsfo

#echo Installing Libraries
#make setup-test

echo Compiling
topkg build
topkg build -n omltest

echo Testing
topkg test

#echo PostingCoverage
#opam install ocveralls

#cd _test_build
#ocveralls --repo_token $COVERALLSTOKEN --git --send ../bisect0001.out
