{ pkgs ? import <nixpkgs> {}
}:
let
  inherit (pkgs) lib fetchgit fetchurl srcOnly stdenv writeScript writeText;
  inherit (stdenv) mkDerivation;
  inherit (stdenv.lib) overrideDerivation;


  callPackage = pkgs.lib.callPackageWith (pkgs // self);
  foldSpace = pkgs.lib.concatStringsSep " ";

  ############ Modifiable parametres ############
  # It would make more sense for these parameters to be function arguments or
  # some kind of other option in the future.

  # Rust version
  rustcVer = pkgs.rustcAlpha2;
  cargo = pkgs.cargoSnapshot;

  # Kernel build parametres
  sel4-platform = "am335x";

  # Version of source code to use, which are git commit hashes
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
  rsl4-arch = "arm";
  rsl4-cpu = if sel4-platform == "am335x"
               then "cortex-a8"
               else throw "Unsupported sel4-platform";

  # Can also be determined from the sel4-platform
  # Name must match what rustc configuration expects
  rsl4-target = "arm-unknown-linux-gnueabihf";

  # The target toolchain for kernel and rsl4 compilation
  rsl4-toolchain = pkgs.gcc-armv7l-unknown-linux-gnueabihf;
  tool-prefix = rsl4-toolchain.prefix + "-";

  rsl4-cc = "${rsl4-toolchain}/bin/${tool-prefix}gcc";
  rsl4-ar = "${rsl4-toolchain}/bin/${tool-prefix}ar";
  rsl4-objcopy = "${rsl4-toolchain}/bin/${tool-prefix}objcopy";

  rsl4-rust-flags = foldSpace [
    "--verbose"
    "--target ${rsl4-target}"
    "-A dead_code"
    "-A non_camel_case_types"
    "-A non_snake_case"
    "-A non_upper_case_globals"

    # Entry point, executable location, stack space for initial task
    "-C link-args=-Wl,-nostdlib,-errt_start,-T${self.rsl4-runtime}/link.lds"

    # location of compiler-rt 
    #"-L ${self.rsl4-runtime}"
    # location of arm libs
    #"-L ${self.rustc-with-arm}/lib/rustlib/${rsl4-target}/lib"
    "-C linker=${tool-prefix}gcc"
    "-C target-cpu=${rsl4-cpu}"
    #"-C relocation-model=static"
    "-C no-stack-check"
    "-C no-redzone"

    "-Z print-link-args"
  ];

  self = rec {

    rsl4-boot = mkDerivation {
      name = "rsl4-boot";
      srcs = [ "${rsl4-elfloader-src}/gen_boot_image.sh"
               "${rsl4-elfloader-src}/src/archive.bin.lds"
               "${rsl4-elfloader-src}/src/arch-arm/linker.lds"
               
               "${rsl4-sel4}/build/kernel/kernel.elf"
               "${rsl4-sel4}/build/arm/${sel4-platform}/elfloader/elfloader.o"
               "${rsl4-init}/rsl4-init"
             ];
      buildInputs = [ rsl4-toolchain rsl4-cpio-strip pkgs.cpio pkgs.which ];
      builder = writeScript "builder.sh" ''
        source $stdenv/setup
        mkdir -p $out
        cp $srcs .
        substituteInPlace ./gen_boot_image.sh --replace /bin/bash $SHELL
        PLAT=${sel4-platform} TOOLPREFIX=${tool-prefix} ./gen_boot_image.sh ./kernel.elf ./rsl4-init $out/rsl4-boot.elf
        ${rsl4-objcopy} -O binary $out/rsl4-boot.elf $out/rsl4-boot.bin
      '';
    };

    rsl4-init = mkDerivation {
      name = "rsl4-init";
      src = ./rsl4-init;
      buildInputs = [ rustc-with-arm rsl4-toolchain ];
      builder = writeScript "builder.sh" ''
        source $stdenv/setup
        mkdir -p $out
        rustc --crate-type bin ${rsl4-rust-flags} -L ${rsl4-librsl4} --emit asm,llvm-ir,link $src/main.rs --out-dir $out
      '';
    };

    rsl4-librsl4 = mkDerivation {
      name = "rsl4-librsl4";
      src = ./librsl4;
      buildInputs = [ rustc-with-arm rsl4-toolchain ];
      builder = writeScript "builder.sh" ''
        source $stdenv/setup
        mkdir -p $out
        cp -r $src/* .
        chmod -R +w ./src
        mkdir -p ./src/types/generated
        cp ${rsl4-generated}/generated.rs ./src/types/generated/mod.rs
        rustc --crate-type rlib --emit asm,llvm-ir,link ${rsl4-rust-flags} --out-dir $out ./src/lib.rs
      '';
    };

    # Generates types based on kernel configuration parameters (except it
    # doesn't do that yet ... TODO).
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
        rsL4-genTool $srcs $out/generated.rs
      '';
    };

    # Uses a precompiled binary hack until there is an easy way to build cargo
    # packages in nix
    rsl4-gen-tool = mkDerivation {
      name = "rsl4-gen-tool";
#      src = ./rsl4-gen-tool;
      src = ./bin; # EWWWWW.
#      buildInputs = [ cargo rustc ];
      builder = builtins.toFile "builder.sh" ''
        source $stdenv/setup
        mkdir -p $out/bin
        cp $src/rsL4-genTool $out/bin
        #cp -r $src/* .
        #cargo build --verbose
      '';
    };

    # Sleeze our runtime into compiler-rt for linking. Kudos to @neykov.
    # https://github.com/neykov/armboot/blob/871cd89a324f81aaa9ad188373d4a13504dc9c10/Makefile
    rsl4-runtime = mkDerivation {
      name = "rsl4-runtime";
      src = ./rsl4-runtime;
      # This builder extracts all objects from libcompiler-rt.a and recombines
      # them with our own runtime object (so we don't need to worry about extra
      # linker flags). It also copies the linker script into the output.
      builder = writeScript "builder.sh" ''
        source $stdenv/setup
        mkdir -p $out
        ${rsl4-cc} -c $src/rrt.S -o ./rsl4-runtime.o
        cp ${rustc-with-arm}/lib/rustlib/arm-unknown-linux-gnueabihf/lib/libcompiler-rt.a $out
        chmod 700 $out/libcompiler-rt.a
        ${rsl4-ar} r $out/libcompiler-rt.a ./rsl4-runtime.o
        cp $src/link.lds $out
      '';
    };

    ##### seL4 derivations ####

    # seL4 provides a cpio-strip utility to make cpio files (completely?)
    # deterministic.
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

    # Builds the kernel and bootloader
    # Maybe refactor this into different build phases in the future
    rsl4-sel4 = mkDerivation {
      name = "rsl4-sel4";
      src = ./sel4-build-files;
      buildInputs = [
        rsl4-toolchain
        pkgs.which
        pkgs.python2Packages.tempita
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

        # Now copy the custom build files for rsl4
        cp $src/Makefile .
        cp $src/Kbuild .
        cp $src/Kconfig .

        # and the generated config
        mkdir ./configs
        cp ${rsl4-sel4-config} ./configs/${rsl4-sel4-config.name}

        # permissions are read-only from the nix store
        chmod -R +w ./*

        # Get rid of absolute paths that break building on nix
        substituteInPlace ./tools/common/project-arm.mk --replace /bin/bash $SHELL
        substituteInPlace ./kernel/tools/xmllint.sh --replace /bin/bash $SHELL
        substituteInPlace ./kernel/tools/cpp_gen.sh --replace /bin/bash $SHELL
        substituteInPlace ./kernel/tools/changed.sh --replace /bin/bash $SHELL

        # Do the build
        make ${rsl4-sel4-config.name}
        make

        # Copy outputs
        mkdir -p $out
        cp -r ./build $out
      '';
    };

    # seL4 config. Doesn't do anything fancy except set the correct toolchian
    # prefix currently. In the future, generate an appropriate config file from
    # the options given by the user
    rsl4-sel4-config = import ./nix/rsl4_defconfig.nix {
      inherit pkgs;
      inherit tool-prefix;
    };

    ##### Sources #####

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

    # Build our target rust version with the arm target for arm libs
    rustc-with-arm = overrideDerivation rustcVer (origAttrs: {
      name = "rustc-with-arm";
      src = rustc-src-armv7-compiler;
      buildInputs = origAttrs.buildInputs ++ [ rsl4-toolchain ];
      configureFlags = origAttrs.configureFlags ++ [
        "--target=${rsl4-target}"
      ];
      # arm libraries fail
      doCheck = false;
      # remove information from arm libraries causing linker errors
      dontStrip = true;
    });

    rustc-src-armv7-compiler = overrideDerivation
      (srcOnly {
        inherit stdenv;
        name = "rsl4-rustc-src";
        src = rustcVer.src;
      })
      (origAttr: {
        name = "rustc-src-armv7-compiler";
        patchPhase = ''
          substituteInPlace ./mk/cfg/arm-unknown-linux-gnueabihf.mk \
            --replace \
            CROSS_PREFIX_arm-unknown-linux-gnueabihf=arm-linux-gnueabihf- \
            CROSS_PREFIX_arm-unknown-linux-gnueabihf=${tool-prefix}
        '';
      });
  };
in
  self
