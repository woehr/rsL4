import Development.Shake
import Development.Shake.Command
import Development.Shake.FilePath
import Development.Shake.Util

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
  pro <- getEnvOrFail "PROCESSOR"
  return ["--target", tar, "-C", ("target-cpu=" ++ pro), "--emit", "ir,asm,link"]

-- Takes a directory containing a rust lib and a list of rust dependencies and builds the lib
compileRlib :: String -> [String] -> Action ()
compileRlib dir deps = do
  co <- compileOpts
  libFiles <- getDirectoryFiles dir ["//*.rs"]
  need $ map (\f -> dir </> f) libFiles
  need $ map (\l -> "_build" </> l) deps
  cmd "rustc" co "--out-dir" buildOut "-L" buildOut (dir </> "lib.rs")

main :: IO ()
main = shakeArgs shakeOptions{shakeFiles="_build/"} $ do
  want [buildOut </> "rsL4-boot.bin"]

  phony "clean" $ do
    putNormal "Cleaning files in _build"
    removeFilesAfter "_build" ["//*"]

  buildOut </> "rsL4-boot.bin" *> \out -> do
    tc <- getEnvOrFail "TOOLCHAIN"
    need
      [ buildDir </> "kernel.elf"
      , buildDir </> "gen_boot_image.sh"
      , buildDir </> "archive.bin.lsd"
      , buildDir </> "linker.lds"
      , buildDir </> "elfloader.o"

      , buildDir </> "rsL4-init.elf"
      ]
    let genImageOutput = out -<.> "elf"
    () <- cmd (buildDir </> "gen_boot_image.sh") (buildDir </> "kernel.elf") (buildDir </> "rsL4-init.elf") genImageOut
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
    initFiles <- getDirectoryFiles "rsL4-init" ["//*"]
    need
      [ buildOut </> "libcore.rlib"
      ] ++ initFiles
    cmd "rustc" "rsL4-init/src/main.rs"

  buildOut </> "libcore.rlib" *> \_ -> do
    compileRlib "rust-src/src/libcore" []

