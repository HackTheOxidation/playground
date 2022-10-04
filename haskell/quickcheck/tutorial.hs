import Test.QuickCheck

prop1 a b = (a+b) == (b+a)
-- test with: quickCheck prop1

prop2 xs = (length $ tail xs) == ((length xs) - 1)
-- test with: quickcheck prop2

prop3 xs = not (null xs) ==> (length $ tail xs) == ((length xs) - 1)
-- test with: quickcheck prop3

prop4 xs = not (null xs) ==> (length $ tail xs) == ((length xs) - 1)
-- test with: quickcheck prop4
