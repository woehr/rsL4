import Development.Shake
import Development.Shake.Command
import Development.Shake.FilePath
import Development.Shake.Util

getEnvOrFail :: String -> Action String
getEnvOrFail var = getEnv var >>= \mVal ->
  case mVal of
    Just(v) -> return v
    Nothing -> error $ "Environment variable {" ++ var ++ "} is undefined."

compileOpts :: Action [String]
compileOpts = do
  tar <- getEnvOrFail "TARGET"
  pro <- getEnvOrFail "PROCESSOR"
  return ["--target", tar, "-C", ("target-cpu=" ++ pro)]

-- Takes a directory containing a rust lib and a list of rust dependencies and builds the lib in
-- dir.
compileRlib :: String -> [String] -> Action ()
compileRlib dir deps = do
  co <- compileOpts
  libFiles <- getDirectoryFiles dir ["//*.rs"]
  need $ map (\f -> dir </> f) libFiles
  need $ map (\l -> "_build" </> l) deps
  cmd "rustc" co "--out-dir" "_build" "-L" "_build" (dir </> "lib.rs")

main :: IO ()
main = shakeArgs shakeOptions{shakeFiles="_build/"} $ do
  want ["_build/rsL4-boot.bin"]

  phony "clean" $ do
    putNormal "Cleaning files in _build"
    removeFilesAfter "_build" ["//*"]

  "_build/rsL4-boot.bin" *> \out -> do
    let inp = out -<.> "elf"
    need [inp]
    tc <- getEnvOrFail "TOOLCHAIN"
    cmd (tc ++ "objcopy") "-O binary" inp out

  "_build/rsL4-boot.elf" *> \out -> do
    let linkScript = "rsL4-boot/am335x.ld"
    let linkObjs   =
          [ "_build/rsL4-boot-start.o"
          , "_build/librsL4-boot.rlib"
          , "_build/libcore.rlib"
          ]
    need $ linkObjs ++ [linkScript]
    tc <- getEnvOrFail "TOOLCHAIN"
    cmd (tc ++ "ld") "-T" linkScript "-o" out linkObjs

  "_build/rsL4-boot-start.o" *> \out -> do
    let inp = "rsL4-boot/start.s"
    need [inp]
    tc <- getEnvOrFail "TOOLCHAIN"
    cmd (tc ++ "as") "-o" out inp

  "_build/librsL4-boot.rlib" *> \_ -> do
    compileRlib "librsL4-boot" ["libcore.rlib"]

  "_build/libcore.rlib" *> \_ -> do
    compileRlib "rust-src/src/libcore" []

