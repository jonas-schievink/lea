#!/usr/bin/env bash

set -e

# Run all unit and integration tests for all crates inside the "src" directory.

for proj in $(ls src); do
    echo "[TESTING] $proj"
    cargo test -p "$proj"
done
