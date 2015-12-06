#!/bin/bash

if [ "$(dirname $0)" != "." ]
then
  echo "You should run this from its directory. Run:"
  echo "  cd $(dirname $0)"
  echo "and try again.  :)"
  exit 1
fi


src=$1

base="$(basename $1 | sed 's/\.pts//')"
ast=examples/pgms/$base

cd systemu
./.cabal-sandbox/bin/runEquivTest $ast

