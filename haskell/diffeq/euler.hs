{- Implementation of Euler's Method for use with differential equations. -}

-- Example function
f1 :: Fractional a => Real a => a -> a -> a
f1 x y = x + (y / 5.0)


-- Implementation of Euler's Method
euler :: Real a => a -> a -> (a -> a -> a) -> a -> [(a, a)]
euler x0 y0 f h = (x1, y1) : (euler x1 y1 f h)
  where
    x1 = x0 + h
    y1 = y0 + h * (f x0 y0)
