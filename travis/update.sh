#!/usr/bin/env bash

# Runs `cargo update` on all crates.

set -e

cargo update
ls src | xargs -n 1 --max-procs 0 cargo update -p

