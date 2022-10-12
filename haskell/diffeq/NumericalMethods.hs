{- Implementation of Numerical approximation methods for use with differential equations. -}
module NumericalMethods (
  euler
  , improvedEuler
  , rungeKutta4
) where

-- Example function
f1 :: Fractional a => Real a => a -> a -> a
f1 x y = x + (y / 5.0)


-- Implementation of Euler's Method
euler :: Real a => a -> a -> (a -> a -> a) -> a -> [(a, a)]
euler x0 y0 f h = (x1, y1) : euler x1 y1 f h
  where
    x1 = x0 + h
    y1 = y0 + h * f x0 y0


-- Implementation of the Improved Euler's Method
improvedEuler :: Fractional a => Real a => a -> a -> (a -> a -> a) -> a -> [(a, a)]
improvedEuler x0 y0 f h = (x1, y1) : improvedEuler x1 y1 f h
    where
        y1 = y0 + h * ((k1 + k2) / 2)
        k2 = f x0 u
        u = y0 + h * k1
        k1 = f x0 y0
        x1 = x0 + h

-- Implementation of Runge-Kutta RK4 method
rungeKutta4 :: Fractional a => Real a => a -> a -> (a -> a -> a) -> a -> [(a, a)]
rungeKutta4 x0 y0 f h = (x1, y1) : rungeKutta4 x1 y1 f h
  where
    y1 = y0 + h * k
    k = (k1 + 2*k2 + 2*k3 + k4) / 6
    k4 = f x1 $ y0 + h * k3
    k3 = f (x0 + h / 2) $ y0 + h*k2 / 2
    k2 = f (x0 + h / 2) $ y0 + h*k1 / 2
    k1 = f x0 y0
    x1 = x0 + h
