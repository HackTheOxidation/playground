module Interactions (
  act,
  Interactions.getLine,
  Interactions.putStr,
  Interactions.putStrLn,
  strlen,
) where

act :: IO (Char, Char)
act = do
  x <- getChar
  _ <- getChar
  y <- getChar
  return (x,y)

getLine :: IO String
getLine = do
  x <- getChar
  if x == '\n' then
    return []
  else
   do xs <- Interactions.getLine;
       return (x:xs)

putStr :: String -> IO ()
putStr [] = return ()
putStr (x:xs) = do putChar x
                   Interactions.putStr xs

putStrLn :: String -> IO ()
putStrLn xs = do Interactions.putStr xs
                 putChar '\n'

strlen :: IO ()
strlen = do Interactions.putStr "Enter a string: "
            xs <- Interactions.getLine
            Interactions.putStr "The string has "
            Interactions.putStr (show (length xs))
            Interactions.putStrLn " characters"
