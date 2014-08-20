#!/bin/bash

# This file Copyright (c) 2011 Mitchell Johnson.
#
# This software is free software; you can redistribute it and/or
# modify it under the terms of the GNU Library General Public
# License version 2, with the special exception on linking
# described in file LICENSE.
#
# This software is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

set -e

PACKAGES="lablgl,lablgl.glut,oUnit,batteries,sdl,sdl.sdlimage,pcre"
SELF=$0
TARGET=$1
FLAGS="-use-ocamlfind -cflags -g \
       -pkgs $PACKAGES"
OCAMLBUILD=ocamlbuild
BIN="pngs2pls pls2pg graphdisplay vis skeletonize"

ocb()
{
  $OCAMLBUILD $FLAGS $*
}

rule() {
  case $1 in
    clean) 
      ocb -clean
      for FI in $BIN ; do if [[ -e $FI ]] ; then rm $FI ; fi ; done
      ;;
    run)    
      shift
      export OCAMLRUNPARAM=b
      ocb $TARGET.native -- $@ ;;
    native) ocb $TARGET.native;;
    bin)
      for FI in $BIN 
      do ./make.sh $FI native && cp $FI.native bin/$FI
      done ;;
    typeset) 
      source-highlight -i $TARGET.ml -o doc/code/$TARGET.tex \
        --outlang-def=doc/code/latex.outlang ;;
    profile) ocb $TARGET.p.native;;
    byte)   ocb $TARGET.byte;;
    all)    ocb $TARGET.native $TARGET.byte;;
    test)   
      for TEST in *_t.ml ; do
        TEST_TARGET=`echo $TEST | sed -e s/\.ml$//g`
        ocb $TEST_TARGET.native -- 
      done ;;
    doc) 
      FILES=`ls *.ml | grep -v -e _t.ml -e _ti.ml`
      echo $FILES
      ocamlfind ocamldoc -package $PACKAGES \
                -html -I _build -I $CI_PATH -d doc/html $FILES ;;
    depend) echo "Not needed.";;
    *)      echo "Unknown action $1";;
  esac;
}

if [ $TARGET == 'test' ] || [ $TARGET == 'doc' ] || [ $TARGET == 'bin' ] \
   || [ $TARGET == 'clean' ]; then
  rule $TARGET
  echo
  exit 0
else 
  shift
fi

if [ $# -eq 0 ]; then
  rule all
else 
  while [ $# -gt 0 ]; do
    rule $@;
    if [ $1 == 'run' ] ; then
      break
    fi
    shift
  done
fi
