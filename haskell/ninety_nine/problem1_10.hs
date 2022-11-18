
-- Problem 1
myLast :: Eq a => [a] -> a
myLast [] = error "No last element for an empty list."
myLast (x:xs) | null xs = x
              | otherwise = myLast xs

-- Problem 2
myButLast :: Eq a => [a] -> a
myButLast [] = error "No second last element for an empty list."
myButLast (x:xs) | length xs == 1 = x
                 | otherwise = myButLast xs

-- Problem 3
elementAt :: [a] -> Int -> a
elementAt (x:xs) i | i == 0 = x
                   | otherwise = elementAt xs (i - 1)

-- Problem 4
myLength :: [a] -> Int
--myLength [] = 0
--myLength (x:xs) = 1 + myLength xs
myLength = foldr (\x -> (+) 1) 0

-- Problem 5
myReverse :: [a] -> [a]
myReverse [] = []
myReverse (x:xs) = myReverse xs ++ [x]

-- Problem 6
isPalindrome :: Eq a => [a] -> Bool
isPalindrome xs = xs == reverse xs

-- Problem 7
data NestedList a = Elem a | List [NestedList a]
flatten :: NestedList a -> [a]
flatten (Elem x) = [x]
flatten (List []) = []
flatten (List (x:xs)) = flatten x ++ flatten (List xs)

-- Problem 8
compress :: Eq a => [a] -> [a]
compress [] = []
compress xs = compress' xs []
  where
    compress' [] ys = reverse ys
    compress' (x:xs) [] = compress' xs [x]
    compress' (x:xs) ys | last ys == x = compress' xs ys
                        | otherwise = compress' xs (x:ys)
