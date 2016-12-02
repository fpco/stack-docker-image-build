#!/usr/bin/env bash

set -eux

LAST_LINE=$(stack sdist 2>&1 | tail -n 1)
SDIST=${LAST_LINE##* }

rm -rf build-static
mkdir build-static
mkdir -p build-home
docker run --rm \
    -v $(pwd)/build-static:/host-bin \
    -v $SDIST:/sdist.tar.gz \
    -v $(pwd)/build-home:/home/build \
    fpco/docker-static-haskell:8.0.1 \
    /bin/bash -c \
    'chown $(id -u) $HOME && rm -rf $HOME/src && mkdir $HOME/src && cd $HOME/src && tar zxfv /sdist.tar.gz && cd * && stack install --test --local-bin-path /host-bin --ghc-options "-optl-static -fPIC -optc-Os" && upx --best --ultra-brute /host-bin/*'
