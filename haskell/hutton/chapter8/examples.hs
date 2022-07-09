-- Chapter 8 - Types and Classes

-- Type declaration (Synonyms for existing types)
type String = [Char]

type Pos = (Int,Int)

type Trans = Pos -> Pos

-- Parameterized type declaration
type Pair a = (a,a)

type Assoc k v = [(k,v)]

-- Using new types in functions
find :: Prelude.Eq k => k -> Assoc k v -> v
find k t = head [v | (k',v) <- t, k Prelude.== k']


-- Data declarations (Completely new types)

-- data Bool = False | True

data Move = North | South | East | West deriving Show

-- Using new data types in functions
move :: Move -> Pos -> Pos
move North (x,y) = (x,y+1)
move South (x,y) = (x,y-1)
move East (x,y) = (x+1,y)
move West (x,y) = (x-1,y)

moves :: [Move] -> Pos -> Pos
moves ms p = foldl (flip move) p ms

rev :: Move -> Move
rev North = South
rev South = North
rev East = West
rev West = East

-- Data declarations with parameterized constructors
data Shape = Circle Float | Rect Float Float

-- Using the Shape data type in functions
square :: Float -> Shape
square n = Rect n n

area :: Shape -> Float
area (Circle r) = pi * r^2
area (Rect x y) = x * y

-- Constructor functions as data
-- data Maybe a = Nothing | Just a
safediv :: Int -> Int -> Maybe Int
safediv _ 0 = Nothing
safediv m n = Just (m `div` n)

safehead :: [a] -> Maybe a
safehead [] = Nothing
safehead xs = Just (head xs)

-- Newtype declarations
--newtype Nat = N Int

-- Recursive types
data Nat = Zero | Succ Nat

nat2int :: Nat -> Int
nat2int Zero = 0
nat2int (Succ n) = 1 + nat2int n

int2nat :: Int -> Nat
int2nat 0 = Zero
int2nat n = Succ (int2nat (n-1))

-- adding natural numbers with conversion
add' :: Nat -> Nat -> Nat
add' m n = int2nat (nat2int m + nat2int n)

-- adding natural numbers with recursion
add :: Nat -> Nat -> Nat
add Zero n = n
add (Succ m) n = Succ (add m n)

data List a = Nil | Cons a (List a)

len :: List a -> Int
len Nil = 0
len (Cons _ xs) = 1 + len xs

-- Trees
data Tree a = Leaf a | Node (Tree a) a (Tree a)

t :: Tree Int
t = Node (Node (Leaf 1) 3 (Leaf 4)) 5 (Node (Leaf 6) 7 (Leaf 9))

occurs' :: Prelude.Eq a => a -> Tree a -> Bool
occurs' x (Leaf y) = x Prelude.== y
occurs' x (Node l y r) = x Prelude.== y || occurs' x l || occurs' x r

flatten :: Tree a -> [a]
flatten (Leaf x) = [x]
flatten (Node l x r) = flatten l ++ [x] ++ flatten r

occurs :: Ord a => a -> Tree a -> Bool
occurs x (Leaf y) = x Prelude.== y
occurs x (Node l y r) | x Prelude.== y = True
                      | x < y = occurs x l
                      | otherwise = occurs x r

data Tree1 a = Leaf1 a | Node1 (Tree1 a) (Tree1 a)

data Tree2 a = Leaf2 | Node2 (Tree2 a) a (Tree2 a)

data Tree3 a b = Leaf3 a | Node3 (Tree3 a b) b (Tree3 a b)

data Tree4 a = Node4 a [Tree4 a]


-- Class and instance declarations
class Eq a where
  (==) :: a -> a -> Bool
  (/=) :: a -> a -> Bool

  x /= y = not (x Main.== y)
