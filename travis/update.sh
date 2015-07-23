#!/usr/bin/env bash

# Runs `cargo update` on all crates.

set -e

for proj in $(ls src); do
    echo "Updating $proj..."
    cd "src/$proj"

    if ! cargo update; then
        echo "Trying to create Cargo.lock"
        cargo generate-lockfile
        cargo update
    fi

    cd ../..
done
