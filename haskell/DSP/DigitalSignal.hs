module DigitalSignal
( Signal(..),
  evalDigital,
  evalAnalog,
  frequencyDigital
) where


data Signal = Digital Float Float Float | Analog Float Float

evalDigital :: Integral a => Signal -> [a] -> [Float]
evalDigital _ [] = []
evalDigital (Digital amp f0 ts) (n:ns) = (amp * sin (2 * pi * f0 * fromIntegral n * ts))
                                     : evalDigital (Digital amp f0 ts) ns
evalDigital (Analog _ _) (_:_) = []

evalAnalog :: Signal -> [Float] -> [Float]
evalAnalog _ [] = []
evalAnalog (Analog amp f0) (t:ts) = (amp * sin (2 * pi * f0 * t)) : evalAnalog (Analog amp f0) ts
evalAnalog Digital {} (_:_) = []

frequencyDigital :: Integral a => Signal -> [a] -> [Float]
frequencyDigital _ [] = []
frequencyDigital (Digital amp f0 ts) (m:ms) = (amp * sin (2 * pi * f0 * fromIntegral m)) : frequencyDigital (Digital amp f0 ts) ms
frequencyDigital (Analog _ _) (_:_) = []
