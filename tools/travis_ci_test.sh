#!/usr/bin/env bash

export opam_pin_add=""
travis_install_on_linux () {
    # Install OCaml and OPAM PPAs
    case "$OCAML_VERSION,$OPAM_VERSION" in
        3.12.1,1.0.0) ppa=avsm/ocaml312+opam10 ;;
        3.12.1,1.1.0) ppa=avsm/ocaml312+opam11 ;;
        4.00.1,1.0.0) ppa=avsm/ocaml40+opam10 ;;
        4.00.1,1.1.0) ppa=avsm/ocaml40+opam11 ;;
        4.01.0,1.0.0) ppa=avsm/ocaml41+opam10 ;;
        4.01.0,1.1.0) ppa=avsm/ocaml41+opam11 ;;
        4.01.0,1.2.0) ppa=avsm/ocaml41+opam12; export opam_pin_add="add" ;;
        4.02.0,1.1.0) ppa=avsm/ocaml42+opam11 ;;
        4.02.0,1.2.0) ppa=avsm/ocaml42+opam12; export opam_pin_add="add" ;;
      *) echo Unknown $OCAML_VERSION,$OPAM_VERSION; exit 1 ;;
    esac

    echo "yes" | sudo add-apt-repository ppa:$ppa
    sudo apt-get update -qq

    sudo apt-get install -qq ocaml ocaml-native-compilers camlp4-extra opam time git
    sudo apt-get install liblapack-dev
    sudo apt-get install gfortran       # for lbgfs
    sudo apt-get install libffi-dev     # for Ctypes
}

travis_install_on_osx () {
    #curl -OL "http://xquartz.macosforge.org/downloads/SL/XQuartz-2.7.6.dmg"
    #sudo hdiutil attach XQuartz-2.7.6.dmg
    #sudo installer -verbose -pkg /Volumes/XQuartz-2.7.6/XQuartz.pkg -target /

    brew update
    brew install homebrew/dupes/lapack
    brew install gcc  # for gfortran
    brew install opam 
    brew install libffi # for ocephes
    export opam_init_options="--comp=$OCAML_VERSION"
    export opam_pin_add="add"
}

case $TRAVIS_OS_NAME in
  osx) travis_install_on_osx ;;
  linux) travis_install_on_linux ;;
  *) echo "Unknown $TRAVIS_OS_NAME"; exit 1
esac

# configure and view settings
export OPAMYES=1
echo "ocaml -version"
ocaml -version
echo "ocamlbuild version"
ocamlbuild -version
echo "opam --version"
opam --version
echo "git --version"
git --version

# install OCaml packages
opam init $opam_init_options
eval `opam config env`

# Bypass opam bug #1747
git config --global user.email "you@example.com"
git config --global user.name "Your Name"

echo Installing Libraries
make setup

echo Compiling
make

echo Testing
make covered_test

echo PostingCoverage
opam install ocveralls

cd _driver
ocveralls --repo_token $COVERALLSTOKEN --git --send ../bisect0001.out
