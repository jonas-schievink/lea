#!/usr/bin/env bash

# Cleans all crates. Not used by travis, but might come in handy.

set -e

ls src | xargs -n 1 --max-procs $(nproc) cargo clean -p
