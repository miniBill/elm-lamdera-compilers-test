#!/usr/bin/env sh
set -e
bun start -- --jobs 4
cat out > out.xlsx
rm out
