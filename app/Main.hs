module Main where

import Server (runAppDevel)
import System.Environment (getProgName)

--import System.Exit (exitFailure)
import System.IO qualified as IO

{- | The 'main' function gathers the required environment information and
 initializes the application.
-}
main :: IO ()
main = do
    args <- getArgs
    progName <- getProgName
    case args of
        [configPath] -> runAppDevel configPath
        _ -> do
            IO.hPutStrLn stderr $ "Usage: " ++ progName ++ " <conf>"
            exitFailure
