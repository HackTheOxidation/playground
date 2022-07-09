-- Recursive Functions

fac :: Int -> Int
fac 0 = 1
fac n = n Prelude.* fac(n-1)

(*) :: Int -> Int -> Int
m * 0 = 0
m * n = m + (m Prelude.* (n-1))

product :: Num a => [a] -> a
product [] = 1
product (n:ns) = n Prelude.product ns

length :: [a] -> Int
length [] = 0
length (_:xs) = 1 + Prelude.length xs

reverse :: [a] -> [a]
reverse [] = []
reverse (x:xs) = Prelude.reverse xs Prelude.++ [x]

(++) :: [a] -> [a] -> [a]
[] ++ ys = ys
(x:xs) ++ ys = x : (xs Prelude.++ ys)

insert :: Ord a => a -> [a] -> [a]
insert x [] = [x]
insert x (y:ys) | x <= y = x : y : ys
                | otherwise = y : insert x xs

isort :: Ord a =>  [a] -> [a]
isort = foldr insert []

zip :: [a] -> [b] -> [(a,b)]
zip [] _ = []
zip _ [] = []
zip (x:xs) (y:ys) = (x,y) : zip xs ys

drop :: Int -> [a] -> [a]
drop 0 xs = xs
drop _ [] = []
drop n (_:xs) = Prelude.drop (n-1) xs

fib :: Int -> Int
fib 0 = 0
fib 1 = 1
fib n = fib (n-2) + fib(n-1)

qsort :: Ord a => [a] -> [a]
qsort [] = []
qsort (x:xs) = qsort smaller Prelude.++ [x] Prelude.++ qsort larger
               where
                  smaller = [a | a <- xs, a <= x]
                  larger = [b | b <- xs, b > x]

even :: Int -> Bool
even 0 = True
even n = odd (n-1)

odd :: Int -> Bool
odd 0 = False
odd n = even (n-1)

evens :: [a] -> [a]
evens [] = []
evens (x:xs) = x : odds xs

odds :: [a] -> [a]
odds [] = []
odds (_:xs) = evens xs


