product1 [] = 1
product1 (x:xs) = x * product1 xs

revqsort [] = []
revqsort (x:xs) = revqsort larger ++ [x] ++ revqsort smaller
                  where
                     smaller = [a | a <- xs, a <= x]
                     larger  = [b | b <- xs, b > x]

brokenqsort [] = []
brokenqsort (x:xs) = brokenqsort smaller ++ [x] ++ brokenqsort larger
                     where
                        smaller = [a | a <- xs, a < x]
                        larger  = [b | b <- xs, b > x]

