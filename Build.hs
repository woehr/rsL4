import Development.Shake
import Development.Shake.Command
import Development.Shake.FilePath
import Development.Shake.Util

data CrateType = Bin | Rlib

buildOut :: String
buildOut = "_build/outputs"

sel4Out :: String
sel4Out = "_build/seL4"

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
  let compileMe = case ty of
        Bin  -> "main.rs"
        Rlib -> "lib.rs"
  cmd "rustc" defaultOpts
      (dir </> compileMe)

main :: IO ()
main = shakeArgs shakeOptions{shakeFiles="_build/"} $ do
--  want [buildOut </> "rsL4-boot.bin"]
  want [buildOut </> "rsL4-init.elf"]

  phony "clean" $ do
    putNormal "Cleaning files in _build"
    removeFilesAfter "_build" ["//*"]

  buildOut </> "rsL4-boot.bin" *> \out -> do
    tc <- getEnvOrFail "TOOLCHAIN"
    need
      [ buildOut </> "kernel.elf"
      , buildOut </> "gen_boot_image.sh"
      , buildOut </> "archive.bin.lsd"
      , buildOut </> "linker.lds"
      , buildOut </> "elfloader.o"

      , buildOut </> "rsL4-init.elf"
      ]
    let genImageOut = out -<.> "elf"
    () <- cmd (buildOut </> "gen_boot_image.sh") (buildOut </> "kernel.elf") (buildOut </> "rsL4-init.elf") genImageOut
    cmd (tc ++ "objcopy") "-O" "binary" genImageOut out

  buildOut </> "kernel.elf" *> \out -> do
    need [pyOut </> "bin/activate", buildOut </> "repo"]
    let buildFileDir = "seL4-build-files"
    buildFiles <- getDirectoryFiles buildFileDir ["//*"]
    need $ map (\f -> buildFileDir </> f) buildFiles
    cmd (buildFileDir </> "build.sh") buildFileDir (buildOut </> "repo") pyOut sel4Out buildOut

  pyOut </> "bin/activate" *> \out -> do
    pyVer <- getEnvOrFail "PYTHON_EXE"
    cmd "virtualenv" ("--python=" ++ pyVer) pyOut

  buildOut </> "repo" *> \out -> do
    cmd "curl" "https://storage.googleapis.com/git-repo-downloads/repo" "-o" out

  buildOut </> "rsL4-init.elf" *> \out -> do
    compileRust Bin "rsL4-init" ["libcore.rlib"]

  buildOut </> "libcore.rlib" *> \out -> do
    compileRust Rlib "rust-src/src/libcore" []

