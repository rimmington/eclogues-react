module Main (main) where

import Control.Monad ((<=<))
import Paths_eclogues_react (getDataFileName)
-- import Language.Haskell.HLint (hlint)
import System.Exit (ExitCode (..), exitFailure, exitSuccess)
import System.Process (spawnProcess, waitForProcess)

paths :: [String]
paths =
    [ "app"
    , "test"
    ]

-- Can't use the hlint lib because of https://github.com/ghcjs/ghcjs-boot/issues/38
hlint :: [String] -> IO [()]
hlint = (pure . toFakeHints) <=< waitForProcess <=< spawnProcess "hlint"
  where
    toFakeHints ExitSuccess     = []
    toFakeHints (ExitFailure _) = [()]

arguments :: IO [String]
arguments = pure $ "--hint=HLint.hints" : paths
-- arguments = go <$> getDataFileName "HLint.hints"
--   where
--     go p = ("--hint=" ++ p) : paths

main :: IO ()
main = do
    hints <- hlint =<< arguments
    if null hints then exitSuccess else exitFailure
