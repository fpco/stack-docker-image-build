#!/usr/bin/env bash

set -eux

# Build a new Docker image using the Dockerfile in this directory
docker build --tag example .

# Run GHC in interpreter mode to call a function from my-package, to prove that
# it was installed correctly
docker run --rm example ghc -e MyModule.myFunction
