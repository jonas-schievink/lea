#!/usr/bin/env bash

# Runs `cargo update` on all crates.

set -e

ls src | xargs -n 1 --max-procs 0 sh -c '(cd "src/$@"; exec cargo update)' -
