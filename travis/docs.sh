#!/usr/bin/env bash

# `after_success` hook to generate documentation using `cargo doc` and push it to the `gh-pages`
# branch. Works without root.

set -e

# Only build docs when on master and not building a PR (this would be bad)
[ $TRAVIS_BRANCH = master ]
[ $TRAVIS_PULL_REQUEST = false ]
[ -n "$GH_SECRET" ]
[ -n "$TRAVIS_REPO_SLUG" ]

# Build docs for the main crate. It is assumed that this crate depends on all other crates
# (otherwise these will not have docs built for them)
cd src/lea
cargo doc
echo "<meta http-equiv=refresh content=0;url=`echo $TRAVIS_REPO_SLUG | cut -d '/' -f 2`/index.html>" > target/doc/index.html

# Init a new git repo in the docs
cd target/doc
git config user.email "jonas@schievink.net"
git config user.name "Travis CI"
git init
git add .
git commit -m "Update docs"
git push -fq https://${GH_SECRET}@github.com/${TRAVIS_REPO_SLUG}.git master:gh-pages
