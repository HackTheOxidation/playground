import System.IO
import Data.Char
import Data.List

-- 1: Redefine putStr using list comprehension

{-
Original implementation:

putStr :: String -> IO ()
putStr [] = return ()
putStr (x:xs) = do putChar x
                   Interactions.putStr xs
-}

putStr :: String -> IO ()
putStr xs = sequence_ [putChar x | x <- xs]

-- 2: Using recursion define a version of putBoard that displays nim boards of any size
{-
Original implementation:

putBoard :: Board -> IO ()
putBoard [a,b,c,d,e] = do putRow 1 a
                          putRow 2 b
                          putRow 3 c
                          putRow 4 d
                          putRow 5 e

-}

type Board = [Int]

putRow :: Int -> Int -> IO ()
putRow row num = do System.IO.putStr (show row)
                    System.IO.putStr ": "
                    System.IO.putStrLn (concat (replicate num "* "))

putBoardN :: Board -> Int -> IO ()
putBoardN _ 0 = return ()
putBoardN [] _ = return ()
putBoardN b n = do putRow (b !! n) n

putBoard :: Board -> IO ()
putBoard [] = return ()
putBoard b = do putBoardN b (length b)
                putBoard (tail b)

-- 3: Redefine putBoard using list comprehension and sequence_

putBoard2 :: Board -> IO ()
putBoard2 b = sequence_ [putRow row num | row <- b, num <- [1..(length b)]]

-- 4: Define adder :: IO ()
readAcc :: Int -> Int -> IO Int
readAcc acc n = do input <- System.IO.getLine
                   let x = read input :: Int
                   if n == 0
                     then return (x+acc)
                     else
                     readAcc (x+acc) (n-1)

adder :: IO ()
adder = do System.IO.putStr "How many numbers? "
           input <- System.IO.getLine
           let n = read input :: Int
           total <- readAcc 0 (n-1)
           putStrLn ("The total is " ++ show total)
           
-- 5: Redefine adder :: IO () using sequence
adder2 :: IO ()
adder2 = do System.IO.putStr "How many numbers? "
            input <- System.IO.getLine
            let n = read input :: Int
            ns <- sequence [System.IO.getLine | _ <- [1..n]]
            let total = sum (map read ns)
            putStrLn ("The total is " ++ show total)

-- 6: Define readLine :: IO String using getCh with support for delete key
getLine :: IO String
getLine = do x <- getChar
             if x == '\n' then
               return []
               else
               do xs <- Main.getLine
                  return (x:xs)

readLine :: IO String
readLine = do x <- getChar
              let action | x == '\n' = return []
                         | x == '\DEL' = do xs <- Main.readLine
                                            return xs
                         | otherwise = do xs <- Main.readLine
                                          return (x:xs)
              action
