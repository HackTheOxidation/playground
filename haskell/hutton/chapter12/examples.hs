-- Functors
-- Some simple functions
inc :: [Int] -> [Int]
inc [] = []
inc (n:ns) = n+1 : inc ns

sqr :: [Int] -> [Int]
sqr [] = []
sqr (n:ns) = n^2 : sqr ns

-- Abstracting away a common pattern
inc2 :: [Int] -> [Int]
inc2 = map (+1)

sqr2 :: [Int] -> [Int]
sqr2 = map (^2)

-- Mapping functions to parameterised types with functors
-- Defining fmap for lists
instance Functor [] where
  fmap = map

-- Defining fmap for the Maybe monad
instance Functor Maybe where
  fmap _ Nothing = Nothing
  fmap g (Just x) = Just (g x)

-- User-defined types can be made into functors as well
data Tree a = Leaf a | Node (Tree a) (Tree a)
            deriving Show

instance Functor Tree where
  fmap g (Leaf x) = Leaf (g x)
  fmap g (Node l r) = Node (fmap g l) (fmap g r)

instance Functor IO where
  fmap g mx = do {x <- mx; return (g x)}

-- Generalising inc with a Functor
inc3 :: Functor f => f Int -> f Int
inc3 = fmap (+1)

-- Applicatives
fmap0 :: Functor f => a -> f a
fmap0 = pure

fmap1 :: Functor f => (a -> b) -> f a -> f b
fmap1 g x = pure g <*> x

-- and so on...

-- From the standard prelude
instance Applicative Maybe where
  -- pure :: a -> Maybe a
  pure = Just

  -- (<*>) :: Maybe (a -> b) -> Maybe a -> Maybe b
  Nothing <*> _ = Nothing
  (Just g) <*> mx = fmap g mx

instance Applicative [] where
  -- pure :: a -> [a]
  pure x = [x]

  -- (<*>) :: [a -> b] -> [a] -> [b]
  gs <*> xs = [g x | g <- gs, x <- xs]


-- Using list comprehension
prods :: [Int] -> [Int] -> [Int]
prods xs ys = [x*y | x <- xs, y <- ys]

-- Using Applicatives
prods2 :: [Int] -> [Int] -> [Int]
prods2 xs ys = pure (*) <*> xs <*> ys


-- Also from the standard prelude
instance Applicative IO where
  -- pure :: a -> IO a
  pure = return

  -- (<*>) :: IO (a -> b) -> IO a -> IO b
  mg <*> mx = do {g <- mg; x <- mx; return (g x)}


-- An example using the IO Monad
getChars :: Int -> IO String
getChars 0 = return []
getChars n = pure (:) <*> getChar <*> getChars (n - 1)

-- Effectful programming
sequenceA :: Applicative f => [f a] -> f [a]
sequenceA [] = pure []
sequenceA (x:xs) = pure (:) <*> x <*> sequenceA xs

getChars :: Int -> IO String
getChars n = sequenceA (replicate n getChar)

-- Monads
data Expr = Val Int | Div Expr Expr

eval :: Expr -> Int
eval (Val n) = n
eval (Div x y) = eval x `div` eval y

-- Handle the Division by Zero case with safediv
safediv :: Int -> Int -> Maybe Int
safediv _ 0 = Nothing
safediv n m = Just (n `div` m)

-- Problem: this is verbose and doesn't fit the applicative pattern
eval :: Expr -> Maybe Int
eval (Val n) = Just n
eval (Div x y) = case eval x of
                   Nothing -> Nothing
                   Just n -> case eval y of
                               Nothing -> Nothing
                               Just m -> safediv n m

-- Define the bind operators as a solution
(>>=) :: Maybe a -> (a -> Maybe b) -> Maybe b
mx >>= f = case mx of
             Nothing -> Nothing
             Just x -> f x

-- Use the bind operator to redefine eval
eval :: Expr -> Maybe Int
eval (Val n) = Just n
eval (Div x  y) = eval x >>= \n ->
                               eval y >>= \m ->
                                            safediv n m
