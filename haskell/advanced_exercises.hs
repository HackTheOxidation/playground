import Test.QuickCheck
import Data.List hiding (insert)

data Tree a = Leaf | Node (Tree a) a (Tree a) deriving Show

-- Exercise #1
inv_tup_tree :: Tree (Integer, Integer)
inv_tup_tree = asc (0, 0)
  where
    asc (l, r) = Node (asc (l + 1, r)) (l, r) (asc (l, r + 1)) 

cut :: Integer -> Tree a -> Tree a
cut 0 _ = Leaf
cut i Leaf = Leaf
cut i (Node l n r) = Node (cut (i - 1) l) n (cut (i - 1) r)

-- Exercise #2
insert :: Ord a => a -> Tree a -> Tree a
insert n Leaf = Node Leaf n Leaf
insert n (Node l v r) = if n < v then
                          Node (insert n l) v r
                          else
                          Node l v (insert n r)

inorder :: Tree a -> [a]
inorder Leaf = []
inorder (Node l v r) = (inorder l) ++ [v] ++ (inorder r)

prop_IIS xs = sort xs === xs'
  where
    types = xs :: [Int]
    xs' = inorder $ foldr insert Leaf xs

main :: IO ()
main = do
    putStrLn "Linker is fixed"
