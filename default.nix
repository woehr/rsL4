{ pkgs ? import <nixpkgs> {}
}:
let
  stdenv = pkgs.stdenv;

  callPackage = pkgs.lib.callPackageWith (pkgs // self);
  foldSpace = pkgs.lib.concatStringsSep " ";

  inherit (pkgs) fetchgit fetchurl srcOnly;
  inherit (stdenv) mkDerivation;
  inherit (stdenv.lib) overrideDerivation;

  rustRev = "522d09dfecbeca1595f25ac58c6d0178bbd21d7d";
  # Rather than bothering with importing rustc/common.nix
  rustPlatform = "linux-x86_64";

  rsl4Cc = "${pkgs.gcc-arm-embedded}/bin/arm-none-eabi-gcc";
  rsl4Ar = "${pkgs.gcc-arm-embedded}/bin/arm-none-eabi-ar";
  rsl4Target = "arm-unknown-linux-gnueabi";
  rsl4Cpu = "cortex-a8";
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

  self = pkgs // rec {
    
    rsl4-rust-src = srcOnly {
      inherit stdenv;
      name = "rsl4-rust-src";
      src = fetchgit {
        url = "https://github.com/rust-lang/rust";
        rev = rustRev;
        sha256 = "10vljjggs9h86xj8bcvvrkmz73pbkr7xqz9zk4i48kxk0cnbmd9g";
      };
    };
 
    rsl4-rustc = overrideDerivation pkgs.rustcMaster (rustcOld:
      let
        shortVersion = "1.0.0-dev";
        version = "${shortVersion}-g${revShort}";
        rev = rustRev;
        revShort = builtins.substring 0 7 rev;
        snapshotDate = "2015-02-17";
        snapshotRev = "f1bb6c2";
        snapshotHash = "acec86045632f4f3f085c072ba696f889906dffe";
        snapshotName = "rust-stage0-${snapshotDate}-${snapshotRev}-${rustPlatform}-${snapshotHash}.tar.bz2";
      in {
        inherit version;
        name = "rustc-${version}";
        src = "${rsl4-rust-src}";
        snapshot = overrideDerivation pkgs.rustcMaster.snapshot (snapshotOld : {
          src = fetchurl {
            url = "http://static.rust-lang.org/stage0-snapshots/${snapshotName}";
            sha1 = snapshotHash;
          };
        });
        patches = ["${rustc-patch-paths}" "${rustc-patch-stage}" "${rustc-patch-grsec}"];
      }
    );

    rsl4Build = name: src: cmds: mkDerivation {
      inherit name;
      inherit src;
      builder = pkgs.writeScript "builder.sh" (''
        source $stdenv/setup
        mkdir -p $out
      '' + cmds);
    };

    rsl4-core = rsl4Build "rsl4-core" "${rsl4-rust-src}/src/libcore" ''
      ${rsl4-rustc}/bin/rustc ${rsl4RustFlags} --out-dir $out $src/lib.rs
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

    rustc-patch-paths = builtins.toFile "paths.patch" ''
      diff --git a/src/librustc_back/archive.rs b/src/librustc_back/archive.rs
      index 7ea192b..cd5d01b 100644
      --- a/src/librustc_back/archive.rs
      +++ b/src/librustc_back/archive.rs
      @@ -57,7 +57,7 @@ fn run_ar(handler: &ErrorHandler, maybe_ar_prog: &Option<String>,
                 paths: &[&Path]) -> ProcessOutput {
           let ar = match *maybe_ar_prog {
               Some(ref ar) => &ar[..],
      -        None => "ar"
      +        None => "@arPath@"
           };
           let mut cmd = Command::new(ar);
       
      diff --git a/src/librustc_trans/back/link.rs b/src/librustc_trans/back/link.rs
      index dacf620..815ce60 100644
      --- a/src/librustc_trans/back/link.rs
      +++ b/src/librustc_trans/back/link.rs
      @@ -348,8 +348,8 @@ pub fn mangle_internal_name_by_path_and_seq(path: PathElems, flav: &str) -> Stri
       
       pub fn get_cc_prog(sess: &Session) -> String {
           match sess.opts.cg.linker {
      -        Some(ref linker) => return linker.to_string(),
      -        None => sess.target.target.options.linker.clone(),
      +        Some(ref linker) => linker.to_string(),
      +        None => "@ccPath@".to_string(),
           }
       }
    '';
    rustc-patch-stage = builtins.toFile "stage.patch" ''
      diff --git a/src/etc/local_stage0.sh b/src/etc/local_stage0.sh
      index ca59b1c..65ee7bf 100755
      --- a/src/etc/local_stage0.sh
      +++ b/src/etc/local_stage0.sh
      @@ -50,11 +50,6 @@if [-z $TARG_DIR ]; then
       fi
       
       cp ''${PREFIX}/bin/rustc''${BIN_SUF} ''${TARG_DIR}/stage0/bin/
      -cp ''${PREFIX}/''${LIB_DIR}/''${RUSTLIBDIR}/''${TARG_DIR}/''${LIB_DIR}/* ''${TARG_DIR}/stage0/''${LIB_DIR}/
      -cp ''${PREFIX}/''${LIB_DIR}/''${LIB_PREFIX}extra*''${LIB_SUF} ''${TARG_DIR}/stage0/''${LIB_DIR}/
      -cp ''${PREFIX}/''${LIB_DIR}/''${LIB_PREFIX}rust*''${LIB_SUF} ''${TARG_DIR}/stage0/''${LIB_DIR}/
      -cp ''${PREFIX}/''${LIB_DIR}/''${LIB_PREFIX}std*''${LIB_SUF} ''${TARG_DIR}/stage0/''${LIB_DIR}/
      -cp ''${PREFIX}/''${LIB_DIR}/''${LIB_PREFIX}syntax*''${LIB_SUF} ''${TARG_DIR}/stage0/''${LIB_DIR}/
       
       # do not fail if one of the above fails, as all we need is a working rustc!
       exit 0
    '';
    rustc-patch-grsec = builtins.toFile "grsec.patch" ''
      diff --git a/src/test/run-make/relocation-model/Makefile b/src/test/run-make/relocation-model/Makefile
      index 2fcdd32..2d9ddb0 100644
      --- a/src/test/run-make/relocation-model/Makefile
      +++ b/src/test/run-make/relocation-model/Makefile
      @@ -5,9 +5,11 @@ all:
      ''\t$(call RUN,foo)
      
      ''\t$(RUSTC) -C relocation-model=default foo.rs
      +''\tpaxctl -czexm $(TMPDIR)/foo
      ''\t$(call RUN,foo)
      
      ''\t$(RUSTC) -C relocation-model=static foo.rs
      +''\tpaxctl -czexm $(TMPDIR)/foo
      ''\t$(call RUN,foo)
      
      ''\t$(RUSTC) -C relocation-model=default --crate-type=dylib foo.rs
    '';
  };
in
  self

