module Utils.Help where

import System.Environment

printHelp :: IO ()
printHelp = do
  progName <- getProgName
  putStrLn $ "Usage: " ++ progName ++ " [-h | --help | -v | --version] <greeting>"
