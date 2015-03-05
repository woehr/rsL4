#!/bin/sh

# This is the version of nixpkgs that introduces an easier way of creating
# rustc derivations.
NIXPKGS_VER=5821e91bd1270dcab53337e5bc1c6ad4c79e0f23

if [ ! -d "nixpkgs" ]; then
  git clone https://github.com/nixos/nixpkgs nixpkgs;
fi

pushd nixpkgs
git fetch origin
git reset --hard
git checkout --force $NIXPKGS_VER
popd

nix-build -I nixpkgs=./nixpkgs ./default.nix -A rsl4-librsl4

