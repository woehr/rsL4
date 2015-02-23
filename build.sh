#!/bin/sh

NIXPKGS_VER=05b97395ae05b4b89538998333cef8e7c2abb5c6

if [ ! -d "nixpkgs" ]; then
  git clone https://github.com/nixos/nixpkgs nixpkgs;
fi

pushd nixpkgs
git fetch origin
git reset --hard
git checkout --force $NIXPKGS_VER
popd

nix-build -I nixpkgs=./nixpkgs -f ./default.nix -A rsL4

