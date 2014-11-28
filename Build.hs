import Development.Shake
import Development.Shake.Command
import Development.Shake.FilePath
import Development.Shake.Util

data CrateType = Bin | Rlib | StaticLib

buildOut :: String
buildOut = "_build/outputs"

sel4Out :: String
sel4Out = "_build/seL4"

-- TODO: Use the PLAT environment variable for outputs where it makes sense
genBootDir:: String
genBootDir = sel4Out </> "stage/arm/am335x/common/elfloader"

pyOut :: String
pyOut = "_build/venv"

getEnvOrFail :: String -> Action String
getEnvOrFail var = getEnv var >>= \mVal ->
  case mVal of
    Just(v) -> return v
    Nothing -> error $ "Environment variable {" ++ var ++ "} is undefined."

compileOpts :: Action [String]
compileOpts = do
  tar <- getEnvOrFail "TARGET"
  cpu <- getEnvOrFail "PROCESSOR"
  tc  <- getEnvOrFail "TOOLCHAIN"
  return
    [ "--target",   tar
    , "-C",         ("target-cpu=" ++ cpu)
    , "-C",         ("ar=" ++ tc ++ "ar")
    -- Linker is gcc because linker options are passed as -Wl,opt which ld doesn't recognize
    , "-C",         ("linker=" ++ tc ++ "gcc")
--    , "-C",         "lto"
    , "-C",         "no-stack-check"
    , "-L",         buildOut
    , "--emit",     "ir,asm,link"
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
        Bin       -> ( "main.rs",
                     [ "--crate-type", "bin"
                     , "-C",           "link-args=-nostdlib"
                     ])
        Rlib      -> ("lib.rs",  ["--crate-type", "rlib"])
        StaticLib -> ("lib.rs",  ["--crate-type", "staticlib"])
  cmd "rustc" (defaultOpts ++ extraOpts)
      (dir </> fn)

sel4Outputs :: [String]
sel4Outputs =
  [ sel4Out </> "build/kernel/kernel.elf"
  ]

genBootOutputs =
  [ genBootDir </> "gen_boot_image.sh"
  , genBootDir </> "archive.bin.lds"
  , genBootDir </> "linker.lds"
  , genBootDir </> "elfloader.o"
  ]

main :: IO ()
main = shakeArgs shakeOptions{shakeFiles="_build/"} $ do
  want [buildOut </> "rsL4-boot.bin"]

  phony "clean" $ do
    putNormal "Cleaning files in _build"
    removeFilesAfter "_build" ["//*"]

-- TODO: Make this depend also on cpio-strip being built correctly
  buildOut </> "rsL4-boot.bin" *> \out -> do
    tc <- getEnvOrFail "TOOLCHAIN"
    need $
      sel4Outputs ++ genBootOutputs ++
        [ buildOut </> "rsL4-init.elf"
        ]
    let genImageOut = out -<.> "elf"
    () <- cmd (genBootDir </> "gen_boot_image.sh") (sel4Out </> "build/kernel/kernel.elf") (buildOut </> "rsL4-init.elf") genImageOut
    cmd (tc ++ "objcopy") "-O" "binary" genImageOut out

  (sel4Outputs ++ genBootOutputs) &%> \_ -> do
    let buildFileDir = "seL4-build-files"
    buildFiles <- getDirectoryFiles buildFileDir ["//*"] >>= (return . map (\f -> buildFileDir </> f))
    need $ buildFiles ++ [pyOut </> "bin/activate", buildOut </> "repo"]
    cmd (buildFileDir </> "build.sh") buildFileDir (buildOut </> "repo") pyOut sel4Out

  pyOut </> "bin/activate" *> \_ -> do
    pyVer <- getEnvOrFail "PYTHON_EXE"
    cmd "virtualenv" ("--python=" ++ pyVer) pyOut

  buildOut </> "repo" *> \out -> do
    cmd "curl" "https://storage.googleapis.com/git-repo-downloads/repo" "-o" out

  buildOut </> "rsL4-init.elf" *> \out -> do
    compileRust Bin "rsL4-init"
      [ "libmorestack.a"
      , "libcompiler-rt.a"
      , "libcore.rlib"
      ]
    copyFile' (out -<.> "") out

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

