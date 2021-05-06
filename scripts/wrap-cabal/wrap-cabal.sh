#!/usr/bin/env bash

# Overrides the cabal in PATH with a ./bin/cabal wrapper that removes all
# source-repository-package sections from the cabal.project file in PWD

DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" >/dev/null 2>&1 && pwd )"

export CABAL_EXECUTABLE=$(command -v cabal)
export CABAL_PROJECT_FILE=$PWD/cabal.project
export PATH=$DIR/bin:$PATH
