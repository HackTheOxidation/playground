{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
-- Exercises Chapter 6
--

-- 1
fac :: Int -> Int
fac n
  | n > 0 = n * fac (n -1)
  | n <= 0 = 1

-- 2
sumDown :: Int -> Int
sumDown 0 = 0
sumDown n = n + sumDown (n -1)

-- 3
(^) :: Int -> Int -> Int
m ^ 0 = 1
m ^ n = m * (m Main.^ (n -1))

-- 4
euclid :: Int -> Int -> Int
euclid n m
  | n == m = n
  | n > m = euclid (n - m) m
  | n < m = euclid n (m - n)

-- 6
-- 6a
and :: [Bool] -> Bool
and (x : xs)
  | null xs = x
  | otherwise = x && Main.and xs

-- 6b
concat :: [[a]] -> [a]
concat [[]] = []
concat (x : xs)
  | null x = []
  | null xs = x
  | otherwise = x ++ Main.concat xs

-- 6c
replicate :: Int -> a -> [a]
replicate n x
  | n > 0 = x : Main.replicate (n -1) x
  | n <= 0 = []

-- 6d
(!!) :: [a] -> Int -> a
(x : xs) !! 0 = x
(x : xs) !! n = xs Main.!! (n -1)

-- 6e
elem :: Eq a => a -> [a] -> Bool
elem _ [] = False
elem x (y : ys)
  | x /= y && ys == [] = False
  | x == y = True
  | otherwise = Main.elem x ys

-- 7
merge :: Ord a => [a] -> [a] -> [a]
merge [] [] = []
merge xs [] = xs
merge [] ys = ys
merge (x : xs) (y : ys)
  | x < y = x : Main.merge xs (y : ys)
  | x > y = y : Main.merge (x : xs) ys
  | x == y = [x] ++ [y] ++ Main.merge xs ys

-- 8
halve :: [a] -> ([a], [a])
halve [] = ([], [])
halve xs = splitAt (div (length xs) 2) xs

msort :: Ord a => [a] -> [a]
msort xs
  | length xs <= 1 = xs
  | otherwise = merge (msort h1) (msort h2)
  where
    (h1, h2) = halve xs

-- 9a
sum :: Num a => [a] -> a
sum = foldr (+) 0

-- 9b
take :: Int -> [a] -> [a]
take _ [] = []
take 0 _ = []
take n (x : xs) = x : Main.take (n -1) xs

-- 9c
last :: [a] -> a
last (x : xs) = if null xs then x else Main.last xs
