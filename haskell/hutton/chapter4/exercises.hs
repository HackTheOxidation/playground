{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
-- Section 4.8 - Exercises

-- 1
halve :: [a] -> ([a],[a])
halve as = splitAt h as
                where h = div (length as) 2

-- 2a
third1 :: [a] -> a
third1 as = head (tail (tail as))

-- 2b
third2 :: [a] -> a
third2 as = as !! 2

-- 2c
third3 :: [a] -> a
third3 (_:_:a:_) = a


-- 3a
safetail1 :: [a] -> [a]
safetail1 xs = if not (null xs) then tail xs else []

-- 3b
safetail2 :: [a] -> [a]
safetail2 xs | null xs = []
             | otherwise = tail xs

-- 3c
safetail3 :: [a] -> [a]
safetail3 [] = []
safetail3 xs = tail xs

-- 4
{-
 - (||) :: Bool -> Bool -> Bool
 - True || True = True
 - True || False = True
 - False || True = True
 - False || False = False
 -}

-- 5
{-
 - (&&) a b = if a == True then
 -                         (if b == True then True else False) else False
 -
 -}

-- 6
{-
 - (&&) a b = if a == True then b else False
 -}

-- 7
{-
 - mult :: Int -> Int -> Int -> Int
 - mult = \x -> (\y -> (\z -> x*y*z)
 -}

-- 8
luhnDouble :: Int -> Int
luhnDouble x = if res > 9 then res - 9 else res
                        where res = x*2

luhn :: Int -> Int -> Int -> Int -> Bool
luhn a b c d = mod (luhnDouble a + b + luhnDouble c + d) 10 == 0
