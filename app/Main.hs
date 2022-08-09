module Main where

import Server (runAppDevel)
import System.Environment (getArgs, getProgName)
import System.Exit (exitFailure)
import qualified System.IO as IO

-- | The 'main' function gathers the required environment information and
-- initializes the application.
main :: IO ()
main = do
  args <- getArgs
  progName <- getProgName
  case args of
    [configPath] -> runAppDevel configPath
    _ -> do
      IO.hPutStrLn IO.stderr $ "Usage: " ++ progName ++ " <conf>"
      exitFailure
