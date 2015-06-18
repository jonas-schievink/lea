#!/usr/bin/env bash

set -e

# Run all unit and integration tests for all crates inside the "src" directory.

for proj in $(ls src); do
    echo "[TESTING] $proj"
    cd "src/lea"
    cargo test -p "$proj"
    cd "../.."
done

# Parallel build. Recompiles every dependency for each crate, but can be slightly faster.
#ls src | xargs -n 1 --max-procs $(nproc) sh -c '(cd "src/$@"; exec cargo test)' -
