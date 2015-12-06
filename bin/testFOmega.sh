#! /bin/bash

exe="../dist/build/FOmega-tests/FOmega-tests"
if [ ! -e "$exe" ]
then
    echo "Cannot run test. Did you build FOmega? Try:"
    echo "$ cabal build"
    exit 1
fi

$exe
