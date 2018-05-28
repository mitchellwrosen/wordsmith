import Development.Shake
import System.Process (callCommand)

main :: IO ()
main =
  shakeArgs shakeOptions $ do
    phony "run" $ do
      need ["wordsmith.cabal"]
      runAfter (callCommand "cabal new-run exe:wordsmith")

    phony "dev" $ do
      need ["wordsmith.cabal"]
      runAfter $
        callCommand
          "ghcid -c 'cabal new-repl lib:wordsmith' --restart cabal.project --restart wordsmith.cabal"

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
        (FileStdin "wordsmith.cabal.dhall")
        (FileStdout out)
        "dhall-to-text"
