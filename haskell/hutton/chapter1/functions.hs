double x = x + x

sum [1..n]

sum' = foldr (+) 0

qsort [] = []
qsort (x : xs) = qsort smaller ++ [x] ++ qsort larger
  where
    smaller = [a | a <- xs, a <= x]
    larger = [b | b <- xs, b > x]

seqn [] = return []
seqn (act : acts) = do
  x <- act
  xs <- seqn acts
  return (x : xs)

product = foldr (*) 1

reverse_qsort [] = []
reverse_qsort (x : xs) = reverse_qsort larger ++ [x] ++ reverse_qsort
  where
    smaller = [a | a <- xs, a <= x]
    larger = [b | b <- xs, b > x]
