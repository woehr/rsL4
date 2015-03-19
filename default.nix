{ pkgs ? import <nixpkgs> {}
}:
let
  stdenv = pkgs.stdenv;

  callPackage = pkgs.lib.callPackageWith (pkgs // self);
  foldSpace = pkgs.lib.concatStringsSep " ";

  inherit (pkgs) lib fetchgit fetchurl srcOnly writeScript writeText;
  inherit (stdenv) mkDerivation;
  inherit (stdenv.lib) overrideDerivation;

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
  rsl4-target = "arm-unknown-linux-gnueabi";

  # The target toolchain for kernel and rsl4 compilation
  rsl4-toolchain = self.arm-unknown-linux-gnueabi;
  tool-prefix = "arm-linux-gnueabi-";

  rsl4-cc = "${rsl4-toolchain}/bin/${tool-prefix}gcc";
  rsl4-ar = "${rsl4-toolchain}/bin/${tool-prefix}ar";
  rsl4-objcopy = "${rsl4-toolchain}/bin/${tool-prefix}objcopy";

  rsl4-rust-flags = foldSpace [
    "--verbose"
    "--target arm-unknown-linux-gnueabi"
    "-A dead_code"
    "-A non_camel_case_types"
    "-A non_snake_case"
    "-A non_upper_case_globals"

    # Entry point, executable location, stack space for initial task
    "-C link-args=-Wl,-nostdlib,-errt_start,-T${self.rsl4-runtime}/link.lds"

    # location of compiler-rt 
    #"-L ${self.rsl4-runtime}"
    # location of arm libs
    "-L ${self.rustc-with-arm}/lib/rustlib/arm-unknown-linux-gnueabi/lib"
    "-C linker=${tool-prefix}gcc"
    "-C target-cpu=${rsl4-cpu}"
    "-C relocation-model=static"
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
               "${rsl4-init}/rsl4-init.elf"
             ];
      buildInputs = [ rsl4-toolchain rsl4-cpio-strip pkgs.cpio pkgs.which ];
      builder = writeScript "builder.sh" ''
        source $stdenv/setup
        mkdir -p $out
        cp $srcs .
        substituteInPlace ./gen_boot_image.sh --replace /bin/bash $SHELL
        PLAT=${sel4-platform} TOOLPREFIX=${tool-prefix} ./gen_boot_image.sh ./kernel.elf ./rsl4-init.elf $out/rsl4-boot.elf
        ${rsl4-objcopy} -O binary $out/rsl4-boot.elf $out/rsl4-boot.bin
      '';
    };

    rsl4-init = mkDerivation {
      name = "rsl4-init";
      src = ./rsl4-init;
      buildInputs = [ self.rustc-with-arm ];
      builder = writeScript "builder.sh" ''
        source $stdenv/setup
        mkdir -p $out
        rustc --crate-type bin ${rsl4-rust-flags} -L ${rsl4-librsl4} --emit asm,llvm-ir,link --out-dir $out $src/main.rs -o rsl4-init.elf
      '';
    };

    rsl4-librsl4 = mkDerivation {
      name = "rsl4-librsl4";
      src = ./librsl4;
      buildInputs = [ self.rustc-with-arm ];
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
        cp ${rustc-with-arm}/lib/rustlib/arm-unknown-linux-gnueabi/lib/libcompiler-rt.a $out
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

        # Now copy the custom configs and makefile for rsl4
        cp $src/Makefile .
        cp $src/Kbuild .
        cp $src/Kconfig .
        cp -r $src/configs .

        # permissions are read-only from the nix store
        chmod -R +w ./*

        # Get rid of absolute paths that break building on nix
        substituteInPlace ./tools/common/project-arm.mk --replace /bin/bash $SHELL
        substituteInPlace ./kernel/tools/xmllint.sh --replace /bin/bash $SHELL
        substituteInPlace ./kernel/tools/cpp_gen.sh --replace /bin/bash $SHELL
        substituteInPlace ./kernel/tools/changed.sh --replace /bin/bash $SHELL

        # Do the build
        make bbb_defconfig
        make

        # Copy outputs
        mkdir -p $out
        cp -r ./build $out
      '';
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
    rustc-with-arm = overrideDerivation rustcVer (old: {
      name = "rsl4-rustc-with-arm-unknown-linux-gnueabi";
      propagatedBuildInputs = old.propagatedBuildInputs ++ [ arm-unknown-linux-gnueabi ];
      configureFlags = old.configureFlags ++ [
        "--target=${rsl4-target}"
      ];
      doCheck = false;
    });

    arm-unknown-linux-gnueabi = mkDerivation {
      name = "arm-unknown-linux-gnueabi";
      srcs = [
        (fetchurl {
          urls = [
            http://ftp.kernel.org/pub/linux/kernel/v3.x/linux-3.15.4.tar.xz
            http://ftp.kernel.org/pub/linux/kernel/v3.x/longterm/v3.15/linux-3.15.4.tar.xz
            http://ftp.kernel.org/pub/linux/kernel/v3.x/longterm/linux-3.15.4.tar.xz
          ];
          sha256 = "1y8khv9vrd2sr0gi03x40zl32lbjh35kayl9kdkhdqfwp9wxflgy";
        })
        (fetchurl {
          urls = [
            ftp://ftp.sunet.se/pub/gnu/gmp/gmp-5.1.3.tar.xz
            ftp://ftp.gnu.org/gnu/gmp/gmp-5.1.3.tar.xz
            http://ftp.sunet.se/pub/gnu/gmp/gmp-5.1.3.tar.xz
            http://ftp.gnu.org/gnu/gmp/gmp-5.1.3.tar.xz
          ];
          sha256 = "0wbhn3wih61vjcs94q531fipfvvzqfq2v4qr03rl3xaggyiyvqny";
        })
        (fetchurl {
          urls = [
            http://www.mpfr.org/mpfr-current/mpfr-3.1.2.tar.xz
            http://www.mpfr.org/mpfr-3.1.2/mpfr-3.1.2.tar.xz
          ];
          sha256 = "0fs501qi8l523gs3cpy4jjcnvwxggyfbklcys80wq236xx3hz79r";
        })
        (fetchurl {
          urls = [
            ftp://ftp.linux.student.kuleuven.be/pub/people/skimo/isl/isl-0.12.2.tar.bz2
            http://mirrors.kernel.org/sources.redhat.com/gcc/infrastructure/isl-0.12.2.tar.bz2
          ];
          sha256 = "1d0zs64yw6fzs6b7kxq6nh9kvas16h8b43agwh30118jjzpdpczl";
        })
        (fetchurl {
          urls = [
            http://www.bastoul.net/cloog/pages/download/cloog-0.18.1.tar.gz
            ftp://gcc.gnu.org/pub/gcc/infrastructure/cloog-0.18.1.tar.gz
          ];
          sha256 = "15j19gb078vmbpvwcx143h6cn98456jfvjw4zsa5z1qlvm70ll02";
        })
        (fetchurl {
          urls = [
            http://www.multiprecision.org/mpc/download/mpc-1.0.2.tar.gz
          ];
          sha256 = "1264h3ivldw5idph63x35dqqdzqqbxrm5vlir0xyx727i96zaqdm";
        })
        (fetchurl {
          urls = [
            http://www.mr511.de/software/libelf-0.8.13.tar.gz
          ];
          sha256 = "0vf7s9dwk2xkmhb79aigqm0x0yfbw1j0b9ksm51207qwr179n6jr";
        })
        (fetchurl {
          urls = [
            ftp://ftp.gnu.org/gnu/binutils/binutils-2.22.tar.bz2
            ftp://ftp.kernel.org/pub/linux/devel/binutils/binutils-2.22.tar.bz2
            http://ftp.gnu.org/gnu/binutils/binutils-2.22.tar.bz2
            http://ftp.kernel.org/pub/linux/devel/binutils/binutils-2.22.tar.bz2
            ftp://gcc.gnu.org/pub/binutils/releases/binutils-2.22.tar.bz2
            ftp://gcc.gnu.org/pub/binutils/snapshots/binutils-2.22.tar.bz2
          ];
          sha256 = "1a9w66v5dwvbnawshjwqcgz7km6kw6ihkzp6sswv9ycc3knzhykc";
        })
        (fetchurl {
          urls = [
            ftp://ftp.gnu.org/gnu/gcc/gcc-4.9.1.tar.bz2
            ftp://ftp.gnu.org/gnu/gcc/gcc-4.9.1/gcc-4.9.1.tar.bz2
            ftp://ftp.gnu.org/gnu/gcc/releases/gcc-4.9.1/gcc-4.9.1.tar.bz2
            http://ftp.gnu.org/gnu/gcc/gcc-4.9.1.tar.bz2
            http://ftp.gnu.org/gnu/gcc/gcc-4.9.1/gcc-4.9.1.tar.bz2
            http://ftp.gnu.org/gnu/gcc/releases/gcc-4.9.1/gcc-4.9.1.tar.bz2
            ftp://ftp.irisa.fr/pub/mirrors/gcc.gnu.org/gcc/releases/gcc-4.9.1/gcc-4.9.1.tar.bz2
            ftp://ftp.uvsq.fr/pub/gcc/snapshots/4.9.1/gcc-4.9.1.tar.bz2
            http://launchpad.net/gcc-linaro/4.9.1/4.9.1/+download/gcc-4.9.1.tar.bz2
          ];
          sha256 = "0zki3ngi0gsidnmsp88mjl2868cc7cm5wm1vwqw6znja28d7hd6k";
        })
        (fetchurl {
          urls = [
             ftp://ftp.gnu.org/gnu/glibc/glibc-2.19.tar.xz
             http://ftp.gnu.org/gnu/glibc/glibc-2.19.tar.xz
             ftp://gcc.gnu.org/pub/glibc/releases/glibc-2.19.tar.xz
             ftp://gcc.gnu.org/pub/glibc/snapshots/glibc-2.19.tar.xz
          ];
          sha256 = "18m2dssd6ja5arxmdxinc90xvpqcsnqjfwmjl2as07j0i3srff9d";
        })
        (fetchurl {
          urls = [
            http://dmalloc.com/releases/dmalloc-5.5.2.tgz
          ];
          sha256 = "12j23s8rxpwcxxaqdmszms7jgw9ny36zpnv7pnrhr594xip5rgnk";
        })
        (fetchurl {
          urls = [
            http://downloads.sourceforge.net/project/duma/duma/2.5.15/duma_2_5_15.tar.gz
          ];
          sha256 = "05sffd9y6ndx2vi9wdbhgl8sf85jsj5gpnyspp8kl2g3ai47kbxs";
        })
        (fetchurl {
          urls = [
            ftp://ftp.gnu.org/pub/gnu/gdb/gdb-7.8.tar.xz
            http://ftp.gnu.org/pub/gnu/gdb/gdb-7.8.tar.xz
            ftp://sources.redhat.com/pub/gdb/releases/gdb-7.8.tar.xz
            ftp://sources.redhat.com/pub/gdb/old-releases/gdb-7.8.tar.xz
            http://launchpad.net/gdb-linaro/7.8/7.8/+download/gdb-7.8.tar.xz
          ];
          sha256 = "0xdqxjj77q60k19hn85msnbv9nchnpzi0lp7z7hm97zpfkhspi29";
        })
        (fetchurl {
          urls = [
            ftp://ftp.gnu.org/pub/gnu/ncurses/ncurses-5.9.tar.gz
            http://ftp.gnu.org/pub/gnu/ncurses/ncurses-5.9.tar.gz
            ftp://invisible-island.net/ncurses/ncurses-5.9.tar.gz
          ];
          sha256 = "0fsn7xis81za62afan0vvm38bvgzg5wfmv1m86flqcj0nj7jjilh";
        })
        (fetchurl {
          urls = [
            http://downloads.sourceforge.net/project/expat/expat/2.1.0/expat-2.1.0.tar.gz
          ];
          sha256 = "11pblz61zyxh68s5pdcbhc30ha1b2vfjd83aiwfg4vc15x3hadw2";
        })
        (fetchurl {
          urls = [
            ftp://ftp.de.debian.org/debian/pool/main/l/ltrace/ltrace_0.7.3.orig.tar.bz2
            http://ftp.de.debian.org/debian/pool/main/l/ltrace/ltrace_0.7.3.orig.tar.bz2
          ];
          sha256 = "00wmbdghqbz6x95m1mcdd3wd46l6hgcr4wggdp049dbifh3qqvqf";
        })
        (fetchurl {
          urls = [
            http://downloads.sourceforge.net/project/strace/strace/4.8/strace-4.8.tar.xz
          ];
          sha256 = "1y6pw4aj4rw5470lqks1ml0n8jh5xbhwr5c3gb00bj570wgjk4pl";
        })
      ];
      buildInputs = [
        crosstool-ng
        pkgs.cloog # gcc
        pkgs.perl  # kernel headers
        pkgs.expat # cross-gdb
        pkgs.file  # cleanup (executable stripping step)
      ];
      builder = writeScript "builder.sh" ''
        source $stdenv/setup
        mkdir -p $out

        # Where ct-ng will put stuff
        mkdir -p ./dummy-home
        # Where tarballs go so ct-ng doesn't fetch them from the internet
        mkdir -p ./.build/tarballs
        # Where extracted sources go so we can patch what we need to
        mkdir -p ./.build/src

        pushd ./.build/src

        # Copy in sources
        for s in $srcs; do
          tarName=$(basename $s | cut -d- -f2-)
          cp $s ./$tarName
          unpackFile ./$tarName
          mv ./$tarName ../tarballs/$tarName
        done

        # Mark source directories as extracted
        for d in ./*; do
          touch ./.$(basename $d).extracted
        done

        # Done in .build/src
        popd

        # Do our patching (perhaps this should be applied to all Makefiles
        # and configure scripts?)
        substituteInPlace ./.build/src/linux-3.15.4/Makefile --replace /bin/pwd pwd
        substituteInPlace ./.build/src/glibc-2.19/configure --replace /bin/pwd pwd

        ct-ng arm-unknown-linux-gnueabi

        # Install to the correct output directory
        substituteInPlace ./.config --replace CT_PREFIX_DIR=\"$\{HOME\}/x-tools/$\{CT_TARGET\}\" CT_PREFIX_DIR=\"$out\"

        # This option makes the install directory read-only but nix will take
        # care of that for us
        substituteInPlace ./.config --replace CT_INSTALL_DIR_RO=y CT_INSTALL_DIR_RO=n

        # This is unnecessary because we get a clean environment with nix
        substituteInPlace ./.config --replace CT_RM_RF_PREFIX_DIR=y CT_RM_RF_PREFIX_DIR=n

        # I want executables name arm-linux-gnueabi so nothing extra is needed
        # to compile rustc targetting arm
        substituteInPlace ./.config --replace CT_TARGET_ALIAS=\"\" CT_TARGET_ALIAS=\"arm-linux-gnueabi\"

        # Use a location in the build directory in place of home
        substituteInPlace ./.config --replace $\{HOME\} $\{CT_TOP_DIR\}/dummy-home

        ct-ng build
      '';
    };

    crosstool-ng = let crosstool-ng-version = "1.20.0"; in mkDerivation {
      name = "crosstool-ng";
      src = fetchurl {
        url = "http://crosstool-ng.org/download/crosstool-ng/crosstool-ng-${crosstool-ng-version}.tar.bz2";
        sha256 = "0r1lqwqgw90q3a3gpr1a29zvn84r5d9id17byrid5nxmld8x5cdz";
      };

      buildInputs = [ pkgs.makeWrapper ];

      # I'm assuming anything that needs ct-ng will also need it's dependencies
      # in order to do anything meaningful since ct-ng is just a bunch of scripts
      propagatedBuildInputs = with pkgs; [
        which gperf bison flex texinfo wget libtool automake ncurses
      ];

      # Clearing CC was required otherwise the target compiler was misidentified
      # when building the ncurses (dependency of native gdb)
      # CXX cleared simply because CC was also cleared, although clearing it may
      # not be required
      postInstall = ''
        wrapProgram $out/bin/ct-ng \
        --prefix PATH : ${crosstool-ng-gcc-wrapper-with-triple}/bin \
        --set CC "" \
        --set CXX ""
      '';
    };

    crosstool-ng-gcc-wrapper-with-triple = mkDerivation {
      name = "gcc-with-triple-fakey";
      srcs = [];
      builder = writeScript "builder.sh" ''
        source $stdenv/setup
        mkdir -p $out/bin
        ln -s $NIX_CC/bin/gcc $out/bin/$(basename $(find ${pkgs.gcc.cc.outPath}/bin -name \*-gcc))
        ln -s $NIX_CC/bin/g++ $out/bin/$(basename $(find ${pkgs.gcc.cc.outPath}/bin -name \*-g++))
      '';
    };
  };
in
  self

