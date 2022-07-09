-- 2.7 Exercises

-- 3
n = a `div` length xs
        where
                a = 10
                xs = [1,2,3,4,5]

-- 4
last1 [] = []
last1 xs = drop (length xs - 1) xs

last2 xs = last xs

last3 xs = xs !! (length xs - 1)

-- 5
init1 [] = []
init1 xs = take (length xs - 1) xs

init2 [] = []
init2 xs = init xs
