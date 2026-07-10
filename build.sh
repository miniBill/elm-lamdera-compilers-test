#!/usr/bin/env sh
set -e
rm ../elm-lamdera-compilers-test-repos/elm-lamdera-compilers-test-repos || true
bun start -- --jobs 4
ln -sf ../elm-lamdera-compilers-test-repos/$(readlink out | cut -d '/' -f2) out 
cat out > out.xlsx 
rm out
