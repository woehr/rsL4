{ pkgs ? import <nixpkgs> {}
}:
let
  stdenv = pkgs.stdenv;

  callPackage = pkgs.lib.callPackageWith (pkgs // self);
  foldSpace = pkgs.lib.concatStringsSep " ";

  inherit (pkgs) fetchgit fetchurl srcOnly;
  inherit (stdenv) mkDerivation;
  inherit (stdenv.lib) overrideDerivation;

  ############ Modifiable parametres ############

  # Build toolchain
  armToolchain = pkgs.gcc-arm-embedded;

  # rsl4 parametres
  rsl4Target = "arm-unknown-linux-gnueabi";
  rsl4Cpu = "cortex-a8";

  # Rust version
  rustc = pkgs.rustcAlpha2;

  # Kernel build parametres

  # Version of source code to use which are git commit hashes
  # master as of 2015/02/27 for seL4 repos
  sel4Rev          = "b62b20f24dc4411db84c69fdbc0f9fdf9d02655a";
  sel4commonRev    = "39faa87bd8fbce1d9c2d1718eda8677c939a1262";
  sel4kbuildRev    = "820f7efb4fbceeb1d0223f48f34dacfe8378cfdb";
  sel4elfloaderRev = "4d3cb13da092f83de13e9a27e18ca9755474e88d";
  # Make sure to be on sel4 branch for the musllibc repo
  sel4muslRev      = "6aa6001a2413db18f056daa14801e64c0c089e6f";
  sel4cpioRev      = "fb50b5f4e0eedf1da877989a7a22b38ee3e77f4d";

  ########## End Modifiable parametres ##########

  # Rust compiler parametres for arm cortex-a8
  rsl4Cc = "${armToolchain}/bin/arm-none-eabi-gcc";
  rsl4Ar = "${armToolchain}/bin/arm-none-eabi-ar";
  rsl4RustFlags = foldSpace [ "--verbose"
                              "--target ${rsl4Target}"
                              "-A dead_code"
                              "-A non_camel_case_types"
                              "-A non_snake_case"
                              "-A non_upper_case_globals"
                              "-C target-cpu=${rsl4Cpu}"
                              "-C ar=${rsl4Ar}"
                              "-C linker=${rsl4Cc}"
                              "-C no-stack-check"
                            ];

  # Helper functions
  rsl4Build = name: src: cmds: mkDerivation {
    inherit name;
    inherit src;
    builder = pkgs.writeScript "builder.sh" (''
      source $stdenv/setup
      mkdir -p $out
    '' + cmds);
  };

  self = pkgs // rec {

    rsl4-core = rsl4Build "rsl4-core" "${rsl4-rust-src}/src/libcore" ''
      ${rustc}/bin/rustc ${rsl4RustFlags} --out-dir $out $src/lib.rs
    '';

    rsl4-compiler-rt = rsl4Build "rsl4-compiler-rt" ./compiler-rt ''
      ${rsl4Cc} -c $src/compiler-rt.s -o compiler-rt.o
      ${rsl4Ar} rcs $out/libcompiler-rt.a compiler-rt.o
    '';

    rsl4-morestack = rsl4Build "rsl4-morestack" "${rsl4-rust-src}/src/rt/arch/arm" ''
      ${rsl4Cc} -c $src/morestack.S -o morestack.o
      ${rsl4Ar} rcs $out/libmorestack.a morestack.o
    '';

    rsl4-runtime = rsl4Build "rsl4-runtime" ./rsL4-runtime ''
      ${rsl4Cc} -c $src/rrt.S -o $out/rsL4-runtime.o
    '';

    rsl4-sel4 = mkDerivation {
      name = "rsl4-sel4";
      src = ./sel4-build-files;
      buildInputs = [
        pkgs.which
        pkgs.python2Packages.tempita
        armToolchain
        pkgs.libxml2                  # for xmllint
        pkgs.clang                    # for clang-format
        pkgs.moreutils                # for sponge
      ];
      
      builder = pkgs.writeScript "builder.sh" ''
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
        rev = sel4Rev;
        sha256 = "0jnmw3mbiakwp02pqn6w849hpppl276sd9h3vva6wa19443vdxxj";
      };
    };

    rsl4-common-src = srcOnly {
      inherit stdenv;
      name = "rsl4-common-src";
      src = fetchgit {
        url = "https://github.com/seL4/common-tool";
        rev = sel4commonRev;
        sha256 = "1fr472m9fg8f0saz7ck3g90b0jl70g828chp4wk4x3x9kjf4hrqh";
      };
    };

    rsl4-kbuild-src = srcOnly {
      inherit stdenv;
      name = "rsl4-kbuild-src";
      src = fetchgit {
        url = "https://github.com/seL4/kbuild-tool";
        rev = sel4kbuildRev;
        sha256 = "1flx537fbxj2xmj6j8icm9x3bxsj9hsml89ywgwjfrwg11arwv04";
      };
    };

    rsl4-elfloader-src = srcOnly {
      inherit stdenv;
      name = "rsl4-elfloader-src";
      src = fetchgit {
        url = "https://github.com/seL4/elfloader-tool";
        rev = sel4elfloaderRev;
        sha256 = "03ndqf5r4sk9f0fqmbv4s01ahny9gr3fsv4cidy3i60rifn7sxyz";
      };
    };

    rsl4-musl-src = srcOnly {
      inherit stdenv;
      name = "rsl4-musl-src";
      src = fetchgit {
        url = "https://github.com/seL4/musllibc";
        rev = sel4muslRev;
        sha256 = "11qgyxkrbld2v1awwn5m5kcmxy4nspqxwqfyx6nrxqxkfkikjpxs";
      };
    };

    rsl4-cpio-src = srcOnly {
      inherit stdenv;
      name = "rsl4-cpio-src";
      src = fetchgit {
        url = "https://github.com/seL4/libcpio";
        rev = sel4cpioRev;
        sha256 = "0v15fpbkf0py98b97qlqd6qy1rn5vywdp7wa577gvwdng1v0qri7";
      };
    };
  };
in
  self

