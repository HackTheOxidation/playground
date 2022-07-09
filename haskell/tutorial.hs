-- Single-line comment

{-
	Multi-line comment
-}

-- Documentation
-- http://learnyouahaskell.com/
-- https://haskell.org

-- Imports
import Data.List

-- Types
-- Bool, Int, Integer, Float, Double, Char, [Char], (Tuples)

-- :: Bool
trueAndFalse = False

trueOrFalse = True

notTrue = False

boolFive = 5 > 4

-- :: Int - Bounded
maxInt = maxBound :: Int

-- :: Integer - Unbounded
numFive :: Integer
numFive = 5

numFive' = 5.0 :: Float

-- :: Float
myFloat :: Float
myFloat = 1.0 + 2.5

-- :: Double
myDouble = 1.55555555555 + 0.00000000001

-- :: Char
singleChar = 'a'

myName :: String
myName = "Tomas"

myName' = ['T', 'o', 'm', 'a', 's']

-- Math
addNum = 3 + 6

subNum = 3 - 6

multNum = 3 * 6

divNum = 3 / 6

modNum = mod 9 2 -- prefix

modNum' = 9 `mod` 2 -- infix

addNeg = 4 + (-4) -- wrap negatives

-- pi, exp, log, sin, cos, tan, asin, acos, atan

truncDouble = truncate myDouble

roundDouble = round myDouble

ceilDouble = ceiling myDouble

floorDouble = floor myDouble

squareFive = sqrt numFive'

-- LISTS
numList = [1, 2, 3, 4, 5]

rangeList = [1 .. 5]

alphaList = ['a' .. 'z']

evenNums = [2, 4 .. 20]

oddNums = [1, 3 .. 20]

oddAlpha = ['a', 'c' .. 'z']

sumNumList = sum [1 .. 5]

sumNumList' = sum numList

prodNumList = product numList

elemList = 5 `elem` numList -- prefix, infix possible

fibNumbers = [0, 1, 1, 2, 3, 5, 8]

moreFibs = [13, 21, 34, 55, 89, 144, 233]

combineFibs = fibNumbers ++ moreFibs

maxFib = maximum combineFibs

minFib = minimum combineFibs

twoLists = [fibNumbers, moreFibs]

myZip = zipWith (+) [1, 2, 3, 4, 5] [6, 7, 8, 9, 10]

zipFibs = zipWith (+) fibNumbers moreFibs

infOdds = [1, 3 ..] -- infinite list

takeOdds = take 20 infOdds

infFives = repeat 5

takeFives = take 20 infFives

cycleFibs = cycle combineFibs

takeCycle = take 50 $ cycle [1, 2, 3, 4, 5, 6, 7, 8, 9, 10]

dropFibs = drop 5 combineFibs

filterFibs = filter (> 5) combineFibs

whileFibs = takeWhile (<= 88) combineFibs

mapList = map (* 2) [1 .. 10]

unordList = [545, 2, 34, 87, 3, 897, 56, 13]

sortList = sort unordList

multiList = product [2, 3, 4, 5]

-- foldr f e ([])
minusList = foldl (-) 1 [2, 3, 4, 5]

sumFold = sum [1 .. 100]

consList = [1, 2, 3, 4, 5]

infFibs = 0 : 1 : zipWith (+) infFibs (tail infFibs)

-- [0,1] ([1]) -> [0, 1, 1, 2] [1, 1, 2]

listFunc = [x * y | x <- [1 .. 5], y <- [1 .. 5]] -- Piping: |, such that

listFunc' = [x * y | x <- [1 .. 5], y <- [1 .. 5], x * y `mod` 3 == 0]

-- TUPLES
myTuple = ("John Doe", 1)

getName = fst myTuple

getId = snd myTuple

empNames = ["John Doe", "Jane Doe", "Mary Jane", "Ben Dover"]

empID = [1 .. 4]

empTuple = zip empNames empID

add a b = a + b

multiply :: Num a => a -> a -> a
multiply a b = a * b

func :: Num a => a -> a -> a -> a
func a b = add (multiply a b)

sayHello :: String
sayHello = "Hello, World!"

nonUppercase :: String -> String
nonUppercase s = [c | c <- s, c `elem` ['a' .. 'z']]

{-
main = do
  putStrLn "Enter your name: "
  name <- getLine
  putStrLn ("Hello, " ++ name ++ "!")
-}

main :: IO ()
main = do
  let s = nonUppercase "Tomas Is The Best!"
  print s
