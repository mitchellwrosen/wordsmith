{-# language LambdaCase #-}
{-# language NoImplicitPrelude #-}
{-# language ScopedTypeVariables #-}

import Mitchell

import Development.Shake
import Environment (getEnvironment)
import Exception
import List (String)
import Process (executeFile, runProcess, runProcess_, shell)
import System.Posix.Signals (sigUSR1)

main :: IO ()
main =
  shakeArgs shakeOptions $ do
    phony "run" $ do
      need ["wordsmith.cabal"]
      runAfter $
        runProcess (shell "cabal new-run exe:wordsmith") >>= \case
          ExitFailure code
            | code == fromIntegral (-sigUSR1) -> do
                env <- getEnvironment
                executeFile "./Shakefile" False ["run"] (Just env)
            | otherwise ->
                throwIO (ExitFailure code)
          ExitSuccess ->
            pure ()

    phony "dev" $ do
      need ["wordsmith.cabal"]
      runAfter $
        runProcess_ (shell
          "ghcid -c 'cabal new-repl exe:wordsmith' --restart cabal.project --restart wordsmith.cabal -T 'sendSIGUSR1' -W")

    want ["bin/wordsmith"]

    "bin/wordsmith" %> \out -> do
      srcfiles <- getDirectoryFiles "" ["src//*.hs"]
      appfiles <- getDirectoryFiles "" ["app/*.hs"]
      need ("cabal.project" : "wordsmith.cabal" : appfiles ++ srcfiles)
      cmd_ "cabal new-build"
      Stdout binary <- cmd "cabal-plan list-bin wordsmith"
      cmd_ "cp" (binary :: String) out

    "wordsmith.cabal" %> \out -> do
      need ["wordsmith.cabal.dhall"]
      cmd_
        (Stdin "./wordsmith.cabal.dhall")
        (FileStdout out)
        "dhall-to-text"
