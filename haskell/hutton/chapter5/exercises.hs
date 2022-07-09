-- Exercises chapter 5

-- 1
hundredsquares = sum [x^2 | x <- [1..100]]

-- 2
grid :: Int -> Int -> [(Int,Int)]
grid m n = [(x,y) | x <- [0..m], y <- [0..n]]

-- 3
square :: Int -> [(Int,Int)]
square n = [(x,x') | (x,x') <- grid n n, x /= x']

-- 4
replicate :: Int -> a -> [a]
replicate n x = [x | _ <- [1..n]]

-- 5
pyths :: Int -> [(Int,Int,Int)]
pyths n = [(x,y,z) | x <- [1..n], y <- [1..n], z <- [1..n], x^2 + y^2 == z^2]

-- 6
factors :: Int -> [Int]
factors n = [x | x <- [1..n], mod n x == 0]

perfects :: Int -> [Int]
perfects n = [x | x <- [1..n], sum (tail (reverse(factors x))) == x]

-- 7
singleGen = zip [x | x <- [1,2]] [y | y <- [3,4]]

-- 8
find :: Eq a => a -> [(a,b)] -> [b]
find k t = [v | (k',v) <- t, k == k']

positions :: Eq a => a -> [a] -> [Int]
positions x xs = find x (zip xs [0..])

-- 9
scalarproduct :: [Int] -> [Int] -> Int
scalarproduct xs ys = sum [x*y | (x,y) <- zip xs ys]

