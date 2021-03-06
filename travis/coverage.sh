#!/usr/bin/env bash

# Hook to collect coverage statistics via `kcov`.
#
# NOTE: kcov requires the following apt packages: libcurl4-openssl-dev libelf-dev libdw-dev
# These need to be installed before running this script.

# FIXME: Rust now has `-Clink-dead-code`, so this can be enabled again!
# (but use travis-cargo for this!)

set -e

if [ -z "$TRAVIS_JOB_ID" ]; then
    echo "TRAVIS_JOB_ID not set!"
    exit 1
fi

if [ ! -e "./usr/local/bin/kcov" ]; then
    # Download and build kcov
    wget https://github.com/SimonKagstrom/kcov/archive/master.tar.gz
    tar xzf master.tar.gz
    mkdir -p kcov-master/build
    cd kcov-master/build
    cmake ..
    make
    make DESTDIR=../.. install
    cd ../..
fi

# Collect coverage for all crates and upload to coveralls.io
PROGS=()
for proj in $(ls src); do
    cd src/lea

    # Capture all programs run by cargo (except rustdoc tests, they don't work as-is and are a bit
    # pointless to collect coverage for)
    IFS=$'\n'
    for prog in $(cargo test -p "$proj" -v | grep 'Running' | sed 's/     Running `\(.*\)`/\1/' | grep -v '^rustdoc'); do
        echo "=>>" $prog
        PROGS+=$'\n'$prog
    done
    unset IFS

    cd ../..
done

IFS=$'\n'
for p in $PROGS; do
    echo "[COVERAGE] $p"
    ./usr/local/bin/kcov --merge --collect-only --exclude-pattern=.cargo kcov-out "$p"
done
unset IFS

for p in $PROGS; do
    echo "[UPLOAD] $p"
    ./usr/local/bin/kcov --merge --report-only --coveralls-id=$TRAVIS_JOB_ID --exclude-pattern=.cargo kcov-out "$p"
done
