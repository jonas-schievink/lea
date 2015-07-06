#!/usr/bin/env bash

# `after_success` hook to generate documentation using `cargo doc` and push it to the `gh-pages`
# branch. Works without root.

set -e

# Only build docs when on master and not building a PR (this would be bad)
if [ "$TRAVIS_BRANCH" != master ]; then
    echo "Aborting: TRAVIS_BRANCH = $TRAVIS_BRANCH"
    exit
fi

if [ "$TRAVIS_PULL_REQUEST" != false ]; then
    echo "Aborting: TRAVIS_PULL_REQUEST = $TRAVIS_PULL_REQUEST"
    exit
fi

if [ -z "$GH_SECRET" ]; then
    echo "GH_SECRET unset, aborting!"
    exit 1
fi

if [ -z "$TRAVIS_REPO_SLUG" ]; then
    echo "TRAVIS_REPO_SLUG unset, aborting!"
    exit 1
fi

# Build docs for the main crate. It is assumed that this crate depends on all other crates
# (otherwise these will not have docs built for them)
cargo doc
echo "<meta http-equiv=refresh content=0;url=`echo $TRAVIS_REPO_SLUG | cut -d '/' -f 2`/index.html>" > target/doc/index.html

# Init a new git repo in the docs
cd target/doc
git init
git config user.email "jonas@schievink.net"
git config user.name "Jonas Schievink"
git add .
git commit -qm "Update docs"
git push -fq https://${GH_SECRET}@github.com/${TRAVIS_REPO_SLUG}.git master:gh-pages
