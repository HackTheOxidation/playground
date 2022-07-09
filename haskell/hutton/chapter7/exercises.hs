-- Exercises: Chapter 7 - Higher order functions
import Data.Char
import Data.List

-- 1 [f x | x <- xs, p x]
listcomp :: (a -> b) -> (a -> Bool) -> [a] -> [b]
listcomp f p xs = map f (filter p xs)


-- 2a
all :: (a -> Bool) -> [a] -> Bool
all p = and . map p

-- 2b
any :: (a -> Bool) -> [a] -> Bool
any p = or . map p

-- 2c
takeWhile :: (a -> Bool) -> [a] -> [a]
takeWhile _ [] = []
takeWhile p (x:xs) | p x = x : Main.takeWhile p xs
                   | otherwise = []

-- 2d
dropWhile :: (a -> Bool) -> [a] -> [a]
dropWhile _ [] = []
dropWhile p (x:xs) | p x = Main.dropWhile p xs
                   | otherwise = x:xs


-- 3
map1 :: (a -> b) -> [a] -> [b]
map1 = map

filter1 p = foldr (\x xs -> if p x then x:xs else xs) []


-- 4
--dec2int = foldl (\x y -> 10*x + y) 0


-- 5
curry :: ((a,b) -> c) -> a -> b -> c
curry f x y = f (x,y)

uncurry :: (a -> b -> c) -> ((a,b) -> c)
uncurry f (x,y) = f x y


-- 6
type Bit = Int

unfold :: (t -> Bool) -> (t -> a) -> (t -> t) -> t -> [a]
unfold p h t x | p x = []
               | otherwise = h x : unfold p h t (t x)

int2bin' :: Int -> [Bit]
int2bin' = unfold (== 0) (`mod` 2) (`div` 2)

chop8' :: [Bit] -> [[Bit]]
chop8' = unfold (== []) (take 8) (drop 8)

map2 :: Eq a => (a -> b) -> [a] -> [b]
map2 f = unfold (== []) (f . head) tail

--iterate2 ::
iterate2 :: (Eq a, Num a) => (a -> a) -> a -> [a]
iterate2 f = unfold (== 0) f f


-- 7
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

addparity :: [Bit] -> [Bit]
addparity bits = make8 bits ++ [mod (sum (make8 bits)) 2]

encode :: String -> [Bit]
encode = concatMap (addparity . int2bin . ord)

chop9 :: [Bit] -> [[Bit]]
chop9 [] = []
chop9 bits = take 9 bits : chop9 (drop 9 bits)

checkparity :: [[Bit]] -> [[Bit]]
checkparity [] = []
checkparity bits | mod (sum (take 8 (head bits))) 2 == last (head bits) = take 8 (head bits) : checkparity (tail bits)
                 | otherwise = error "Parity error"

decode :: [Bit] -> String
decode = map (chr . bin2int) . checkparity . chop9

transmit :: String -> String
transmit = decode . channel . encode

channel :: [Bit] -> [Bit]
channel = id


-- 8
-- Test-channel produces error
channel' :: [Bit] -> [Bit]
channel' = tail

-- 9
altMap :: (a -> b) -> (a -> b) -> [a] -> [b]
altMap _ _ [] = []
altMap f g (x:xs) = f x : altMap g f xs


-- 0
luhnDouble :: Int -> Int
luhnDouble x = if res > 9 then res - 9 else res
                        where res = x*2

luhn :: [Int] -> Bool
luhn xs = sum (altMap id luhnDouble (reverse xs)) == 0
