{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
-- Exercise 1
data Nat = Zero | Succ Nat

add :: Nat -> Nat -> Nat
add Zero n = n
add (Succ m) n = Succ (add m n)

mult :: Nat -> Nat -> Nat
mult Zero n = Zero
mult m (Succ n) = add m (mult m n)

-- Exercise 2
{-
data Tree a = Leaf a | Node (Tree a) a (Tree a)

occurs :: Ord a => a -> Tree a -> Bool
occurs x (Leaf y) = x == y
occurs x (Node l y r) = case compare x y of
                            LT -> occurs x l
                            EQ -> True
                            GT -> occurs x r

-- Exercise 3
leaves :: Tree a -> Int
leaves (Leaf x) = 1
leaves (Node l x r) = leaves l + leaves r

balanced :: Tree a -> Bool
balanced (Leaf x) = True
balanced (Node l x r) = abs lendif <= 1
                        where
                            lendif = leaves l - leaves r
-}

-- Exercise 4
data Tree a = Leaf a | Node (Tree a) (Tree a)
halve xs = splitAt (length xs `div` 2) xs

balance [x] = Leaf x
balance xs = Node (balance ys) (balance zs)
             where (ys,zs) = halve xs

-- Exercise 5
data Expr = Val Int | Add Expr Expr

folde :: (Int -> a) -> (a -> a -> a) -> Expr -> a
folde f _ (Val x) = f x
folde f g (Add e1 e2) = g (folde f g e1) (folde f g e2)

-- Exercise 6
eval :: Expr -> Int
eval = folde (0 +) (+)

size :: Expr -> Int
size (Val _) = 1
size (Add e1 e2) = folde (const 1) (+) (Add e1 e2)

-- Exercise 7
data Maybe a = Nothing | Just a

instance Eq a => Eq (Main.Maybe a) where
    Main.Nothing == Main.Just _ = False
    Main.Just b == Main.Just c = b == c

{-
instance Eq a => Eq [a] where
    (x:xs) == (y:ys) | length x:xs /= length y:ys = False
                     | otherwise = x == y && xs == ys
-}

-- Exercise 8
-- See exercise8.hs

-- Exercise 9
-- See exercise9.hs
