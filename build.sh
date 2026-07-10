#!/usr/bin/env sh
set -e
bun start -- --remove-stale
ln -sf ../elm-lamdera-compilers-test-repos/$(readlink out | cut -d '/' -f2) out 
cat out > out.xlsx 
rm out
