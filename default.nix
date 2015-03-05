{ pkgs ? import <nixpkgs> {}
}:
let
  stdenv = pkgs.stdenv;

  callPackage = pkgs.lib.callPackageWith (pkgs // self);
  foldSpace = pkgs.lib.concatStringsSep " ";

  inherit (pkgs) fetchgit fetchurl srcOnly writeScript;
  inherit (stdenv) mkDerivation;
  inherit (stdenv.lib) overrideDerivation;

  ############ Modifiable parametres ############
  # It would make more sense for these parameters to be function arguments
  # in the future.

  # Rust version
  rustc = pkgs.rustcAlpha2;
  cargo = pkgs.cargoSnapshot;

  # Kernel build parametres
  sel4-platform = "am335x";

  # Version of source code to use which are git commit hashes
  # master as of 2015/02/27 for seL4 repos
  sel4-rev          = "b62b20f24dc4411db84c69fdbc0f9fdf9d02655a";
  sel4-common-rev    = "39faa87bd8fbce1d9c2d1718eda8677c939a1262";
  sel4-kbuild-rev    = "820f7efb4fbceeb1d0223f48f34dacfe8378cfdb";
  sel4-elfloader-rev = "4d3cb13da092f83de13e9a27e18ca9755474e88d";
  # Make sure to be on sel4 branch for the musllibc repo
  sel4-musl-rev      = "6aa6001a2413db18f056daa14801e64c0c089e6f";
  sel4-cpio-rev      = "fb50b5f4e0eedf1da877989a7a22b38ee3e77f4d";

  ########## End Modifiable parametres ##########

  # NOT FULLY SUPPORTED. Calculated from the sel4 platform we're compiling for
  # but only works for am335x
  rsl4-cpu = if sel4-platform == "am335x"
               then "cortex-a8"
               else throw "Unsupported sel4-platform";

  # Can also be determined from the sel4-platform
  rsl4-target = "arm-unknown-linux-gnueabi";

  # For kernel and rsl4 compilation
  target-toolchain = pkgs.gcc-arm-embedded;

  # Rust compiler parametres for arm cortex-a8
  rsl4-cc = "${target-toolchain}/bin/arm-none-eabi-gcc";
  rsl4-ar = "${target-toolchain}/bin/arm-none-eabi-ar";
  rsl4-objcopy = "${target-toolchain}/bin/arm-none-eabi-objcopy";
  rsl4-rust-flags = foldSpace [ "--verbose"
                                "--target ${rsl4-target}"
                                "-A dead_code"
                                "-A non_camel_case_types"
                                "-A non_snake_case"
                                "-A non_upper_case_globals"
                                "-C target-cpu=${rsl4-cpu}"
                                "-C ar=${rsl4-ar}"
                                "-C linker=${rsl4-cc}"
                                "-C no-stack-check"
                              ];
  self = rec {

    rsl4-boot = let sel4-bootloader-dir = "${rsl4-sel4}/stage/arm/${sel4-platform}/common/elfloader";
    in mkDerivation {
      name = "rsl4-boot";
      src = sel4-bootloader-dir;
      buildInputs = [ rsl4-cpio-strip ];
      builder = writeScript "builder.sh" ''
        source $stdenv/setup
        mkdir -p $out
      '';
    };

    rsl4-init = mkDerivation {
      name = "rsl4-init";
      src = ./rsl4-init;
      buildInputs = [ rustc ];
      builder = writeScript "builder.sh" ''
        source $stdenv
        mkdir -p $out
      '';
    };

    rsl4-librsl4 = mkDerivation {
      name = "rsl4-librsl4";
      src = ./librsl4;
      buildInputs = [ rustc ];
      builder = writeScript "builder.sh" ''
        source $stdenv/setup
        mkdir -p $out
        cp $src ./src
        mkdir -p ./src/types/generated
        cp ${rsl4-generated}/generated.rs ./src/types/generated/mod.rs
        rustc --crate-type static ${rsl4-rust-flags} --out-dir $out
      '';
    };

    rsl4-core = mkDerivation {
      name = "rsl4-core";
      src = "${rsl4-rust-src}/src/libcore";
      buildInputs = [ rustc ];
      builder = writeScript "builder.sh" ''
        source $stdenv/setup
        mkdir -p $out
        rustc ${rsl4-rust-flags} --out-dir $out $src/lib.rs
      '';
    };

    rsl4-generated = mkDerivation {
      name = "rsl4-generated";
      srcs = [ "${rsl4-sel4-src}/libsel4/include/api/syscall.xml"
               "${rsl4-sel4-src}/libsel4/include/interfaces/sel4.xml"
               "${rsl4-sel4-src}/libsel4/arch_include/arm/interfaces/sel4arch.xml"
             ];
      buildInputs = [ rsl4-gen-tool ];
      builder = builtins.toFile "builder.sh" ''
        source $stdenv/setup
        mkdir -p $out
        rsl4-gen-tool $srcs > $out/generated.rs
      '';
    };

    rsl4-gen-tool = mkDerivation {
      name = "rsl4-gen-tool";
      src = ./rsl4-gen-tool;
      buildInputs = [ cargo rustc ];
      builder = builtins.toFile "builder.sh" ''
        source $stdenv/setup
        mkdir -p $out/bin
        cp -r $src/* .
        cargo build --verbose
      '';
    };

    rsl4-compiler-rt = mkDerivation {
      name = "rsl4-compiler-rt";
      src = ./compiler-rt;
      builder = writeScript "builder.sh" ''
        source $stdenv/setup
        mkdir -p $out
        ${rsl4-cc} -c $src/compiler-rt.s -o compiler-rt.o
        ${rsl4-ar} rcs $out/libcompiler-rt.a compiler-rt.o
      '';
    };

    rsl4-morestack = mkDerivation {
      name = "rsl4-morestack";
      src = "${rsl4-rust-src}/src/rt/arch/arm";
      builder = writeScript "builder.sh" ''
        source $stdenv/setup
        mkdir -p $out
        ${rsl4-cc} -c $src/morestack.S -o morestack.o
        ${rsl4-ar} rcs $out/libmorestack.a morestack.o
      '';
    };

    rsl4-runtime = mkDerivation {
      name = "rsl4-runtime";
      src = ./rsL4-runtime;
      builder = writeScript "builder.sh" ''
        source $stdenv/setup
        mkdir -p $out
        ${rsl4-cc} -c $src/rrt.S -o $out/rsL4-runtime.o
      '';
    };

    rsl4-cpio-strip = mkDerivation {
      name = "rsl4-cpio-strip";
      srcs = [ "${rsl4-cpio-src}/src/cpio.c"
               "${rsl4-common-src}/cpio-strip.c"
             ];
      builder = writeScript "builder.sh" ''
        source $stdenv/setup
        mkdir -p $out
        mkdir $out/bin
        $CC -W -Wall -Wextra -std=gnu1x -I ${rsl4-cpio-src}/include $srcs -o $out/bin/cpio-strip
      '';
    };

    rsl4-sel4 = mkDerivation {
      name = "rsl4-sel4";
      src = ./sel4-build-files;
      buildInputs = [
        pkgs.which
        pkgs.python2Packages.tempita
        target-toolchain
        pkgs.libxml2                  # for xmllint
        pkgs.clang                    # for clang-format
        pkgs.moreutils                # for sponge
      ];
      
      builder = writeScript "builder.sh" ''
        source $stdenv/setup

        # Setup the directory structure. This is what repo would do in its manifest
        # We copy everything into the build directory because we can't write in nix store.
        cp -r ${rsl4-sel4-src} ./kernel

        mkdir ./libs
        cp -r ${rsl4-musl-src} ./libs/libmuslc
        cp -r ${rsl4-cpio-src} ./libs/libcpio

        mkdir ./tools
        cp -r ${rsl4-common-src} ./tools/common
        cp -r ${rsl4-kbuild-src} ./tools/kbuild
        cp -r ${rsl4-elfloader-src} ./tools/elfloader

        # Now copy the custom configs and makefile for rsl4
        cp $src/Makefile .
        cp $src/Kbuild .
        cp $src/Kconfig .
        cp -r $src/configs .

        # permissions are read-only from the nix store
        chmod -R +w ./*

        # Get rid of absolute paths that break building on nix
        substituteInPlace ./tools/common/project-arm.mk --replace /bin/bash ${pkgs.bash}/bin/bash
        substituteInPlace ./kernel/tools/xmllint.sh --replace /bin/bash ${pkgs.bash}/bin/bash
        substituteInPlace ./kernel/tools/cpp_gen.sh --replace /bin/bash ${pkgs.bash}/bin/bash
        substituteInPlace ./kernel/tools/changed.sh --replace /bin/bash ${pkgs.bash}/bin/bash

        # Do the build
        make bbb_defconfig
        make

        # Copy outputs
        mkdir -p $out
        cp -r ./build $out
      '';
    };

    ##### Sources #####

    rsl4-rust-src = srcOnly {
      inherit stdenv;
      name = "rsl4-rust-src";
      src = rustc.src;
    };

    rsl4-sel4-src = srcOnly {
      inherit stdenv;
      name = "rsl4-seL4-src";
      src = fetchgit {
        url = "https://github.com/seL4/seL4";
        rev = sel4-rev;
        sha256 = "0jnmw3mbiakwp02pqn6w849hpppl276sd9h3vva6wa19443vdxxj";
      };
    };

    rsl4-common-src = srcOnly {
      inherit stdenv;
      name = "rsl4-common-src";
      src = fetchgit {
        url = "https://github.com/seL4/common-tool";
        rev = sel4-common-rev;
        sha256 = "1fr472m9fg8f0saz7ck3g90b0jl70g828chp4wk4x3x9kjf4hrqh";
      };
    };

    rsl4-kbuild-src = srcOnly {
      inherit stdenv;
      name = "rsl4-kbuild-src";
      src = fetchgit {
        url = "https://github.com/seL4/kbuild-tool";
        rev = sel4-kbuild-rev;
        sha256 = "1flx537fbxj2xmj6j8icm9x3bxsj9hsml89ywgwjfrwg11arwv04";
      };
    };

    rsl4-elfloader-src = srcOnly {
      inherit stdenv;
      name = "rsl4-elfloader-src";
      src = fetchgit {
        url = "https://github.com/seL4/elfloader-tool";
        rev = sel4-elfloader-rev;
        sha256 = "03ndqf5r4sk9f0fqmbv4s01ahny9gr3fsv4cidy3i60rifn7sxyz";
      };
    };

    rsl4-musl-src = srcOnly {
      inherit stdenv;
      name = "rsl4-musl-src";
      src = fetchgit {
        url = "https://github.com/seL4/musllibc";
        rev = sel4-musl-rev;
        sha256 = "11qgyxkrbld2v1awwn5m5kcmxy4nspqxwqfyx6nrxqxkfkikjpxs";
      };
    };

    rsl4-cpio-src = srcOnly {
      inherit stdenv;
      name = "rsl4-cpio-src";
      src = fetchgit {
        url = "https://github.com/seL4/libcpio";
        rev = sel4-cpio-rev;
        sha256 = "0v15fpbkf0py98b97qlqd6qy1rn5vywdp7wa577gvwdng1v0qri7";
      };
    };
  };
in
  self

