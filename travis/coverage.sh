#!/usr/bin/env bash

# Hook to collect coverage statistics via `kcov`.
#
# NOTE: kcov requires the following apt packages: libcurl4-openssl-dev libelf-dev libdw-dev
# These need to be installed before running this script.

set -e

[ -n "$TRAVIS_JOB_ID" ]

# Download and build kcov
wget https://github.com/SimonKagstrom/kcov/archive/master.tar.gz
tar xzf master.tar.gz
mkdir kcov-master/build
cd kcov-master/build
cmake ..
make
make DESTDIR=../.. install
cd ../..

# Collect coverage and upload to coveralls.io
./usr/local/bin/kcov --coveralls-id=$TRAVIS_JOB_ID --exclude-pattern=.cargo target/kcov target/debug/lea-*
