#!/usr/bin/env bash

set -e

# Run all unit and integration tests for all crates inside the "src" directory.

ls src | xargs -n 1 --max-procs $(nproc) sh -c '(cd "src/$@"; exec cargo test)' -
