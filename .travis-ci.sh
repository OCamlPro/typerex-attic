#!/bin/sh

export OPAMYES=1 OPAMVERBOSE=1
eval `opam config env`

echo Architecture
uname -a
echo OCaml version
ocaml -version
echo OPAM versions
opam --version
opam --git-version

PREFIX=$HOME/.opam/$OCAML_VERSION

./configure \
    --prefix ${PREFIX} \
    --with-ocamldir=${PREFIX}/lib  \
    --with-metadir=${PREFIX}/lib
make
make install

