import Utils.Help
import Utils.Version

import System.Exit
import System.Environment


mainAct :: [String] -> IO ()
mainAct [] = do
  putStrLn "Needs a greeting"
  printHelp
  exitFailure
mainAct args = do
  let greeting = unwords args
  name <- lookupEnv "USER"
  putStrLn $ maybe "No user to greet!" (\name -> greeting ++ " " ++ name) name

main :: IO ()
main = do
  args <- getArgs
  if elem "-h" args || elem "--help" args then
    printHelp >> exitSuccess
    else if elem "-v" args || elem "--version" args then 
           printVersion >> exitSuccess
    else
           mainAct args
