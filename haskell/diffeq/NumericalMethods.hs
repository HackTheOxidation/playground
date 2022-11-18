{- Implementation of Numerical approximation methods for use with differential equations. -}
module Methods.NumericalMethods (
  euler
  , improvedEuler
  , rungeKutta4
  , StepSize
  , NumericalMethod
  , BivariateDiffEq
) where

type StepSize = Double
type BivariateDiffEq = Double -> Double -> Double
type NumericalMethod a = a -> a -> BivariateDiffEq -> StepSize -> [(a, a)]

-- Example function
f1 :: BivariateDiffEq
f1 x y = x + (y / 5.0)


-- Implementation of Euler's Method
euler :: NumericalMethod Double
euler x0 y0 f h = (x1, y1) : euler x1 y1 f h
  where
    x1 = x0 + h
    y1 = y0 + h * f x0 y0


-- Implementation of the Improved Euler's Method
improvedEuler :: NumericalMethod Double
improvedEuler x0 y0 f h = (x1, y1) : improvedEuler x1 y1 f h
    where
        y1 = y0 + h * ((k1 + k2) / 2)
        k2 = f x0 u
        u = y0 + h * k1
        k1 = f x0 y0
        x1 = x0 + h

-- Implementation of Runge-Kutta RK4 method
rungeKutta4 :: NumericalMethod Double
rungeKutta4 x0 y0 f h = (x1, y1) : rungeKutta4 x1 y1 f h
  where
    y1 = y0 + h * k
    k = (k1 + 2*k2 + 2*k3 + k4) / 6
    k4 = f x1 $ y0 + h * k3
    k3 = f (x0 + h / 2) $ y0 + h*k2 / 2
    k2 = f (x0 + h / 2) $ y0 + h*k1 / 2
    k1 = f x0 y0
    x1 = x0 + h
