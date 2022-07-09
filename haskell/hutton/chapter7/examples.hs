{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
import Data.Char
import Data.List

-- Higher Order Functions
twice :: (a -> a) -> a -> a
twice f x = f (f x)

-- Prelude higher order functions
map1 :: (a -> b) -> [a] -> [b]
map1 f xs = [f x | x <- xs]

map2 :: (a -> b) -> [a] -> [b]
map2 f [] = []
map2 f (x:xs) = f x : map f xs

filter1 :: (a -> Bool) -> [a] -> [a]
filter1 p xs = [x | x <- xs, p x]

filter2 :: (a -> Bool) -> [a] -> [a]
filter2 p [] = []
filter2 p (x:xs) | p x = x : filter p xs
                 | otherwise = filter p xs

-- Example of higher order function
sumsqreven :: [Int] -> Int
sumsqreven ns = sum (map (^2) (filter even ns))

-- Prelude functions implemented with recursion
sum1 :: Num a => [a] -> a
sum1 xs = foldr (+) 0 xs

product1 :: Num a => [a] -> a
product1 xs = foldr (*) 1 xs

or1 :: [Bool] -> Bool
or1 xs = foldr (||) False xs

and1 :: [Bool] -> Bool
and1 xs = foldr (&&) True xs

-- Prelude functions implemented with foldr (fold right)
sum2 :: Num a => [a] -> a
sum2 = sum
-- Explicitly: sum xs = foldr (+) 0 xs

product2 :: Num a => [a] -> a
product2 = product

or2 :: [Bool] -> Bool
or2 = or

and2 :: [Bool] -> Bool
and2 = and

-- Recursive implementation of Prelude function: foldr
foldr1 :: (a -> b -> b) -> b -> [a] -> b
foldr1 _ v [] = v
foldr1 f v (x:xs) = f x (foldr f v xs)

-- More Prelude functions implemented with recursion
length1 :: [a] -> Int
length1 [] = 0
length1 (__:xs) = 1 + length xs

reverse1 :: [a] -> [a]
reverse1 [] = []
reverse1 (x:xs) = reverse xs ++ [x]

-- The snoc function (reverse cons)
snoc :: a -> [a] -> [a]
snoc x xs = xs ++ [x]

reverse2 :: [a] -> [a]
reverse2 [] = []
reverse2 (x:xs) = snoc x (reverse xs)

-- More Prelude functions implemented with foldr
length2 :: [a] -> Int
length2 = foldr (\_ n -> 1+n) 0

reverse3 :: [a] -> [a]
reverse3 = foldr snoc []


-- Recursive functions on list that associate to the left
sum3 :: Num a => [a] -> a
sum3 = sum' 0
         where
            sum' v [] = v
            sum' v (x:xs) = sum' (v+x) xs


-- Recursive implementation of Prelude function: foldl
foldl1 :: (a -> b -> a) -> a -> [b] -> a
foldl1 f v [] = v
foldl1 f v (x:xs) = foldl f (f v x) xs

-- The composition operator : '.'
odd2 :: Integer -> Bool
odd2 = odd

twice2 :: (b -> b) -> b -> b
twice2 f = f . f

sumsqreven2 :: [Integer] -> Integer
sumsqreven2 = sum . map (^2) . filter even

id1 :: a -> a
id1 x = x

compose1 :: [a -> a] -> (a -> a)
compose1 = foldr (.) id1

-- Binary string transmitter
type Bit = Int

bin2int1 :: [Bit] -> Int
bin2int1 bits = sum [w*b | (w,b) <- zip weights bits]
                         where weights = iterate (*2) 1

bin2int :: [Bit] -> Int
bin2int = foldr (\x y -> x + 2*y) 0

int2bin :: Int -> [Bit]
int2bin 0 = []
int2bin n = mod n 2 : int2bin (div n 2)

make8 :: [Bit] -> [Bit]
make8 bits = take 8 (bits ++ repeat 0)

encode :: String -> [Bit]
encode = concatMap (make8 . int2bin . ord)

chop8 :: [Bit] -> [[Bit]]
chop8 [] = []
chop8 bits = take 8 bits : chop8 (drop 8 bits)

decode :: [Bit] -> String
decode = map (chr . bin2int) . chop8

transmit :: String -> String
transmit = decode . channel . encode

channel :: [Bit] -> [Bit]
channel = id

-- Voting algorithms
-- First past the post
votes :: [String]
votes = ["Red", "Blue", "Green", "Blue", "Blue", "Red"]

count :: Eq a => a -> [a] -> Int
count x = length . filter (== x)

rmdups :: Eq a => [a] -> [a]
rmdups [] = []
rmdups (x:xs) = x : filter (/= x) (rmdups xs)

result :: Ord a => [a] -> [(Int,a)]
result vs = sort [(count v vs, v) | v <- rmdups vs]

winner :: Ord a => [a] -> a
winner = snd . last . result

-- Alternative vote
ballots :: [[String]]
ballots = [["Red", "Green"],
            ["Blue"],
            ["Green", "Red", "Blue"],
            ["Blue", "Green", "Red"],
            ["Green"]]

rmempty :: Eq a => [[a]] -> [[a]]
rmempty = filter (/= [])

elim :: Eq a => a -> [[a]] -> [[a]]
elim x = map (filter (/= x))

rank  :: Ord a => [[a]] -> [a]
rank = map snd . result . map head

winner' :: Ord a => [[a]] -> a
winner' bs = case rank (rmempty bs) of
                 [c] -> c
                 (c:cs) -> winner' (elim c bs)
