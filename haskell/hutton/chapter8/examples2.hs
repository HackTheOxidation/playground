-- Chapter 8

-- Class and Instance declarations

class Eq a where
  (==), (/=) :: a -> a -> Bool

  x /= y = not (x Main.== y)

instance Main.Eq Bool where
  False == False = True
  True == True = True
  _ == _ = False


class Main.Eq a => Ord a where
  (<), (<=), (>), (>=) :: a -> a -> Bool
  min, max :: a -> a -> a

  min x y | x Main.<= y = x
          | otherwise = y

  max x y | x Main.<= y = y
          | otherwise = x

instance Main.Ord Bool where
  False < True = True
  _ < _ = False

  b <= c = (b Main.< c) || (b Main.== c)
  b > c = c Main.< b
  b >= c = c Main.<= b

data Bool = False | True
        deriving (Eq, Ord, Show, Read)

data Shape = Circle Float | Rect Float Float

data Maybe a = Nothing | Just a
