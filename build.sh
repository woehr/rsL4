#!/bin/sh

export RUSTC_BRANCH="master"
export TOOLCHAIN="arm-none-eabi-"
export TARGET="arm-unknown-linux-gnueabi"
export PROCESSOR="cortex-a8"

if [ ! -d ".cabal-sandbox" ]; then
  cabal sandbox init;
fi

if [ ! -f ".cabal-sandbox/bin/shake" ]; then
  cabal update;
  cabal install shake;
fi

if [ ! -d "rust-src" ]; then
  git clone https://github.com/rust-lang/rust.git rust-src;
fi

cd rust-src
git fetch origin
git checkout --force $RUSTC_BRANCH
git merge origin/$RUSTC_BRANCH
cd ..

mkdir -p _shake
cabal exec ghc -- --make Build.hs -rtsopts -with-rtsopts=-IO -outputdir=_shake -o _shake/build && _shake/build "$@"

