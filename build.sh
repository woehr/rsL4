#!/bin/sh

# Environment variables used to compile rust code
# Note that seL4 compilation options are defined in its .config
export RUSTC_BRANCH="1.0.0-alpha"
export TOOLCHAIN="arm-none-eabi-"
export TARGET="arm-unknown-linux-gnueabi"
export PROCESSOR="cortex-a8"

# Required for gen_boot_image.sh
export PLAT="am335x"
# TODO: Unify TOOLCHAIN and TOOLPREFIX in build script
export TOOLPREFIX=$TOOLCHAIN

# Used for compiling the seL4 kernel
export PYTHON_EXE="python2.7"

if [ ! -d ".cabal-sandbox" ]; then
  cabal sandbox init;
fi

if [ ! -f ".cabal-sandbox/bin/shake" ]; then
  cabal update;
  cabal install "shake>=0.14";
fi

if [ ! -d "rust-src" ]; then
  git clone https://github.com/rust-lang/rust.git rust-src;
fi

cd rust-src
git fetch origin
git checkout --force $RUSTC_BRANCH
#git merge origin/$RUSTC_BRANCH
cd ..

mkdir -p _shake
cabal exec ghc -- --make Build.hs -rtsopts -with-rtsopts=-IO -outputdir=_shake -o _shake/build && _shake/build "$@"

