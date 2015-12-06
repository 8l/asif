#! /bin/bash

function runTest {
    echo -n "$1 $2 ..... "
    "$1" "$2"
}

echo "--- Section 5 Tests ---"
runTest bin/runEquivTest.sh fomega/reprIdType1.fw
runTest bin/runEquivTest.sh fomega/reprIdType2.fw
runTest bin/runTypecheckTest.sh fomega/Theorem5-1.fw

echo "--- Section 6 Tests ---"
runTest bin/runEquivTest.sh fomega/reprId.fw
runTest bin/runEquivTest.sh fomega/reprSelfApply.fw
runTest bin/runTypecheckTest.sh fomega/Theorem6-1.fw

echo "--- Section 7 Tests ---"
runTest bin/runEquivTest.sh fomega/Theorem7-3a.fw
runTest bin/runEquivTest.sh fomega/Theorem7-3b.fw
runTest bin/runEquivTest.sh fomega/Theorem7-3c.fw
runTest bin/runEquivTest.sh fomega/Theorem7-3d.fw

runTest bin/runEquivTest.sh fomega/Theorem7-4a.fw
runTest bin/runEquivTest.sh fomega/Theorem7-4b.fw
runTest bin/runEquivTest.sh fomega/Theorem7-4c.fw
runTest bin/runEquivTest.sh fomega/Theorem7-4d.fw

runTest bin/runEquivTest.sh fomega/Theorem7-5a.fw
runTest bin/runEquivTest.sh fomega/Theorem7-5b.fw
runTest bin/runEquivTest.sh fomega/Theorem7-5c.fw
runTest bin/runEquivTest.sh fomega/Theorem7-5d.fw

runTest bin/runEquivTest.sh fomega/Theorem7-6a.fw
runTest bin/runEquivTest.sh fomega/Theorem7-6b.fw
runTest bin/runEquivTest.sh fomega/Theorem7-6c.fw
runTest bin/runEquivTest.sh fomega/Theorem7-6d.fw

runTest bin/runEquivTest.sh fomega/Theorem7-7a.fw
runTest bin/runEquivTest.sh fomega/Theorem7-7b.fw
runTest bin/runEquivTest.sh fomega/Theorem7-7c.fw
runTest bin/runEquivTest.sh fomega/Theorem7-7d.fw

runTest bin/runEquivTest.sh fomega/Theorem7-8a.fw
runTest bin/runEquivTest.sh fomega/Theorem7-8b.fw
runTest bin/runEquivTest.sh fomega/Theorem7-8c.fw

runTest bin/runEquivTest.sh fomega/Theorem7-9a.fw
runTest bin/runEquivTest.sh fomega/Theorem7-9b.fw
runTest bin/runEquivTest.sh fomega/Theorem7-9c.fw
runTest bin/runEquivTest.sh fomega/Theorem7-9d.fw

runTest bin/runEquivTest.sh fomega/Theorem7-10a.fw
runTest bin/runEquivTest.sh fomega/Theorem7-10b.fw
runTest bin/runEquivTest.sh fomega/Theorem7-10c.fw
runTest bin/runEquivTest.sh fomega/Theorem7-10d.fw

runTest bin/runEquivTest.sh fomega/CPSId.fw

runTest bin/runEquivTest.sh fomega/cpsId1.fw
runTest bin/runEquivTest.sh fomega/cpsId2.fw
runTest bin/runEquivTest.sh fomega/cpsId3.fw
runTest bin/runEquivTest.sh fomega/cpsId4.fw
runTest bin/runEquivTest.sh fomega/cpsSelfApply1.fw
runTest bin/runEquivTest.sh fomega/cpsSelfApply2.fw
runTest bin/runEquivTest.sh fomega/cpsSelfApply3.fw
runTest bin/runEquivTest.sh fomega/cpsSelfApply4.fw
runTest bin/runEquivTest.sh fomega/cpsSelfApply5.fw
runTest bin/runEquivTest.sh fomega/cpsSelfApply6.fw

echo "--- Section 8 Tests ---"
runTest bin/runEquivTest.sh fomega/selfApply_unquote.fw
runTest bin/runEquivTest.sh fomega/selfApply_isAbs.fw
runTest bin/runTypecheckTest.sh fomega/selfApply_size.fw
runTest bin/runEquivTest.sh fomega/selfApply_nf.fw
runTest bin/runTypecheckTest.sh fomega/selfApply_cps.fw
