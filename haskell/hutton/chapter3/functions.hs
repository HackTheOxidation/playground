-- Functions and examples from chapter 3

-- Types
--
-- Lists:
-- [1,2,3,4,5] :: Int
--
-- Tuples:
-- ("Yes",True,'a') :: (String,Bool,Char)

-- Functions
add :: (Int,Int) -> Int
add (x,y) = x+y

zeroto :: Int -> [Int]
zeroto n = [0..n]

-- Curried Functions
add' :: Int -> (Int -> Int)
add' x y = x+y

mult :: Int -> (Int -> (Int -> Int))
mult x y z = x*y*z


