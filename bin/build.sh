#!/bin/bash

pushd "$(dirname $0)/../"
cabal sandbox init
cabal update
cabal install --enable-tests
cabal test
popd
