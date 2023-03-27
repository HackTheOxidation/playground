module LazyQueue 
( LazyQueue
, empty
, len
, isEmpty
, insert
, inserts
, remove
) where

type LazyQueue a = ([a],[a])

empty :: LazyQueue a
empty = ([],[])

len :: LazyQueue a -> Int
len lq = length (fst lq) + length (snd lq)

isEmpty :: LazyQueue a -> Bool
isEmpty lq = null (fst lq) && null (snd lq)

rot :: [a] -> [a] -> [a] -> [a]
rot l r a | null l = head r : a
          | not (null l) = head l : rot (tail l) (tail r) (head r : a)

makeq :: LazyQueue a -> LazyQueue a
makeq lq | length (snd lq) <= length (fst lq) = lq
         | length (snd lq) == length (fst lq) + 1 = (uncurry rot lq [], [])

insert :: a -> LazyQueue a -> LazyQueue a
insert e lq = makeq (fst lq, e : snd lq)

inserts :: [a] -> LazyQueue a -> LazyQueue a
inserts [] lq = lq
inserts (x:xs) lq = inserts xs (insert x lq)

remove :: LazyQueue a -> (a, LazyQueue a)
remove lq = (head (fst lq), makeq (tail (fst lq), snd lq))
