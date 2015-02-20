import Development.Shake
import Development.Shake.Command
import Development.Shake.FilePath
import Development.Shake.Util

import Data.List (intercalate)
import System.Directory (canonicalizePath)

data CrateType = Bin | Rlib

buildOut :: String
buildOut = "_build/outputs"

sel4Build :: String
sel4Build = "_build/seL4"

sel4Out :: String
sel4Out = buildOut </> "seL4"

-- TODO: Use the PLAT environment variable for outputs where it makes sense
genBootDir:: String
genBootDir = sel4Build </> "stage/arm/am335x/common/elfloader"

pyOut :: String
pyOut = "_build/venv"

getEnvOrFail :: String -> Action String
getEnvOrFail var = getEnv var >>= \mVal ->
  case mVal of
    Just(v) -> return v
    Nothing -> error $ "Environment variable {" ++ var ++ "} is undefined."

needDirRec :: String -> Action ()
needDirRec dir =
  getDirectoryFiles dir ["//*"] >>= (return . map (\f -> dir </> f)) >>= need

cargoBuild :: String -> Action ()
cargoBuild dir = cmd (Cwd dir) "cargo" "build"

cargoRun :: String -> [String] -> Action ()
cargoRun dir args = cmd (Cwd dir) (["cargo", "run", "--"] ++ args)

compileOpts :: Action [String]
compileOpts = do
  tar <- getEnvOrFail "TARGET"
  cpu <- getEnvOrFail "PROCESSOR"
  tc  <- getEnvOrFail "TOOLCHAIN"
  return
    [ "--verbose"
    , "--target",   tar
    , "-A",         "dead_code"
    , "-A",         "non_camel_case_types"
    , "-A",         "non_snake_case"
    , "-A",         "non_upper_case_globals"
    , "-C",         ("target-cpu=" ++ cpu)
    , "-C",         ("ar=" ++ tc ++ "ar")
    -- Linker is gcc because linker options are passed as -Wl,opt which ld doesn't recognize
    , "-C",         ("linker=" ++ tc ++ "gcc")
--    , "-C",         "lto"
    , "-C",         "no-stack-check"
    , "-L",         buildOut
    , "--out-dir",  buildOut
    ]

-- Takes a directory containing a rust lib and a list of rust dependencies and builds the lib
compileRust :: CrateType -> String -> [String] -> Action ()
compileRust ty dir deps = do
  defaultOpts <- compileOpts
  libFiles <- getDirectoryFiles dir ["//*.rs"] >>= (return . map (\f -> dir </> f))
  let depFiles = map (\l -> buildOut </> l) deps
  need $ libFiles ++ depFiles
  let (fn, extraOpts) = case ty of
                             Rlib -> ("lib.rs" , [ "--crate-type", "rlib"
                                                 , "--emit",       "llvm-ir,asm,link"
                                                 ])
                             Bin  -> ("main.rs", [ "--crate-type", "bin"
                                                 , "--emit",       "llvm-ir,asm,obj"
                                                 ])
  cmd "rustc" (defaultOpts ++ extraOpts) (dir </> fn)

-- seL4 inputs
sel4BuildFileDir :: String
sel4BuildFileDir = "seL4-build-files"

-- Outputs from the seL4 build process that are used as inputs elsewhere
kernelElf :: String
kernelElf              = sel4Build </> "build/kernel/kernel.elf"

bootloaderGenScript, bootloaderArchiveLds, bootloaderLds, bootloaderObj :: String
bootloaderGenScript    = genBootDir </> "gen_boot_image.sh"
bootloaderArchiveLds   = genBootDir </> "archive.bin.lds"
bootloaderLds          = genBootDir </> "linker.lds"
bootloaderObj          = genBootDir </> "elfloader.o"

xmlInterfaceSyscall, xmlInterfaceInvoc, xmlInterfaceInvocArch :: String
xmlInterfaceSyscall    = sel4Build </> "libs/libsel4/include/api/syscall.xml"
xmlInterfaceInvoc      = sel4Build </> "libs/libsel4/include/interfaces/sel4.xml"
xmlInterfaceInvocArch  = sel4Build </> "libs/libsel4/arch_include/arm/interfaces/sel4arch.xml"

libcpioSrc, stripCpioSrc :: String
libcpioSrc     = sel4Build </> "libs/libcpio/src/cpio.c"
stripCpioSrc   = sel4Build </> "tools/common/cpio-strip.c"

-- Files produced by the seL4 build script in the seL4 build directory
sel4Products :: [String]
sel4Products =
  [ kernelElf
  , bootloaderGenScript
  , bootloaderArchiveLds
  , bootloaderLds
  , libcpioSrc
  , stripCpioSrc
  , xmlInterfaceSyscall
  , xmlInterfaceInvoc
  , xmlInterfaceInvocArch
  ]

sel4OutputPath :: FilePath -> FilePath
sel4OutputPath = (sel4Out </>) . takeFileName

-- The actual outputs of the seL4 build. These exist out of the sel4Build directory
-- because shake creates directories for them to live in which interferes with the seL4
-- build process
sel4Outputs :: [String]
sel4Outputs = map sel4OutputPath sel4Products

-- gen tool output binary
target_rsL4_genTool :: String
target_rsL4_genTool = "rsL4-genTool"

-- gen tool input directory
project_rsL4_genTool :: String
project_rsL4_genTool = "rsL4-genTool"

-- generated lib source file
target_rsL4_generated_rs :: String
target_rsL4_generated_rs = "librsl4_generated.rs"

-- librsl4 binary
target_librsl4 :: String
target_librsl4 = "librsl4.rlib"

-- librsl4 source
project_librsl4 :: String
project_librsl4 = "librsl4"

main :: IO ()
main = shakeArgs shakeOptions{shakeFiles="_build/"} $ do
  want [buildOut </> "rsL4-boot.bin"]

  phony "clean" $ do
    putNormal "Cleaning files in _build"
    removeFilesAfter "_build" ["//*"]

  buildOut </> "rsL4-boot.bin" *> \out -> do
    tc <- getEnvOrFail "TOOLCHAIN"
    need $
      sel4Outputs ++
        [ buildOut </> "cpio-strip"
        , buildOut </> "rsL4-init.elf"
        ]
    let genImageOut = out -<.> "elf"
    -- Add build outputs to path so gen_boot_image.sh can find cpio-strip
    canonPath <- liftIO $ canonicalizePath buildOut
    opt <- addPath [canonPath] []

    () <- cmd opt bootloaderGenScript kernelElf (buildOut </> "rsL4-init.elf") genImageOut
    cmd (tc ++ "objcopy") "-O" "binary" genImageOut out

  buildOut </> "rsL4-init.elf" *> \out -> do
    need [ buildOut </> "rsL4-runtime.o"
         , buildOut </> "rsL4-init.o"
         , buildOut </> "librsl4.rlib"
         --, buildOut </> "libmorestack.a"
         --, buildOut </> "libcompiler-rt.a"
         ]
    tc <- getEnvOrFail "TOOLCHAIN"
    cmd [ (tc ++ "gcc")
        , "-nostdlib"
        , "-Wl,--as-needed,--gc-section,-errt_start,--script=rsL4-runtime/link.lds"
--        , "-pie"
        , (buildOut </> "rsL4-runtime.o")
        , (buildOut </> "rsL4-init.o")
        , (buildOut </> "librsl4.rlib")
        , (buildOut </> "libcore.rlib")
        , "-o",  out
        ]

  buildOut </> "rsL4-init.o" *> \out -> do
    compileRust Bin "rsL4-init" ["libmorestack.a", "libcompiler-rt.a", "libcore.rlib", "librsl4.rlib"]

  buildOut </> target_librsl4 *> \_-> do
    needDirRec project_librsl4
    need [ buildOut </> target_rsL4_generated_rs
         ]

    -- I do the build of librsl4 this way for several reasons. First, we don't know what the files in the
    -- librsl4 project will be ahead of time and I don't want to have to modify the build script when that
    -- project changes. I haven't tried reading the files from the directory and creating a target that way
    -- but it may be possible. Second, I didn't want to copy the generated file into the source tree, but
    -- instead for everything to be in the build directory.
    -- There is probably a better way, but this works for now.
    let generatedDir = buildOut </> project_librsl4 </> "src" </> "types" </> "generated"
    () <- cmd "rm" "-rf" (buildOut </> project_librsl4)
    () <- cmd "cp" "-R" project_librsl4 buildOut
    () <- cmd "mkdir" "-p" generatedDir
    () <- cmd "cp" "-T" (buildOut </> target_rsL4_generated_rs) (generatedDir </> "mod.rs")
    compileRust Rlib (buildOut </> project_librsl4 </> "src") []

  buildOut </> target_rsL4_generated_rs *> \out -> do
    need [ project_rsL4_genTool </> "target" </> target_rsL4_genTool
         , sel4OutputPath xmlInterfaceSyscall
         , sel4OutputPath xmlInterfaceInvoc
         , sel4OutputPath xmlInterfaceInvocArch
         ]
    -- cargoRun executes cargo in the cargo project's directory so we need to make sure these are
    -- relative to that directory
    let args = map (".." </>)
         [ sel4OutputPath xmlInterfaceSyscall
         , sel4OutputPath xmlInterfaceInvoc
         , sel4OutputPath xmlInterfaceInvocArch
         , out
         ]
    cargoRun project_rsL4_genTool args

  project_rsL4_genTool </> "target" </> target_rsL4_genTool *> \_ -> do
    -- TODO: Do an out-of-tree cargo build so the outputs aren't in the same directory
    --       then we can just needDirRec on project_rsL4_genTool
    needDirRec $ project_rsL4_genTool </> "src"
    need [ project_rsL4_genTool </> "Cargo.lock"
         , project_rsL4_genTool </> "Cargo.toml"
         ]
    cargoBuild project_rsL4_genTool

  buildOut </> "cpio-strip" *> \out -> do
    -- We don't strictly need sel4Outputs but rather sources that are fetched as part of the seL4 build step
    need sel4Outputs
    cmd "gcc" "-W" "-Wall" "-Wextra" "-std=gnu1x" ("-I" ++ (sel4Build </> "libs/libcpio/include")) libcpioSrc stripCpioSrc "-o" out

  -- TODO: The manner in which seL4 is built should be improved.
  --       Currently building seL4 doesn't depend on any of the fetched sources, only the build inputs
  sel4Outputs &%> \_ -> do
    needDirRec sel4BuildFileDir
    need [pyOut </> "bin/activate", buildOut </> "repo"]
    () <- cmd (sel4BuildFileDir </> "build.sh") sel4BuildFileDir (buildOut </> "repo") pyOut sel4Build
    mapM_ (uncurry copyFile') (zip sel4Products sel4Outputs)

  pyOut </> "bin/activate" *> \_ -> do
    pyVer <- getEnvOrFail "PYTHON_EXE"
    cmd "virtualenv" ("--python=" ++ pyVer) pyOut

  buildOut </> "repo" *> \out -> do
    cmd "curl" "https://storage.googleapis.com/git-repo-downloads/repo" "-o" out

  buildOut </> "rsL4-runtime.o" *> \out -> do
    let inp = "rsL4-runtime/rrt.S"
    need [inp]
    tc  <- getEnvOrFail "TOOLCHAIN"
    cmd (tc ++ "gcc") "-c" inp "-o" out

  buildOut </> "libmorestack.a" *> \out -> do
    let inp = "rust-src/src/rt/arch/arm/morestack.S"
    need [inp]
    tc <- getEnvOrFail "TOOLCHAIN"
    () <- cmd (tc ++ "gcc") "-c" inp "-o" (out -<.> "o")
    cmd (tc ++ "ar") "rcs" out (out -<.> "o")

  -- A dummy more stack library so we don't have to invoke the linker manually
  -- TODO: Build the real libcompiler-rt.a when it becomes required
  buildOut </> "libcompiler-rt.a" *> \out -> do
    let inp = "compiler-rt/compiler-rt.s"
    need [inp]
    tc <- getEnvOrFail "TOOLCHAIN"
    () <- cmd (tc ++ "gcc") "-c" inp "-o" (out -<.> "o")
    cmd (tc ++ "ar") "rcs" out (out -<.> "o")

  buildOut </> "libcore.rlib" *> \_ -> do
    compileRust Rlib "rust-src/src/libcore" []

