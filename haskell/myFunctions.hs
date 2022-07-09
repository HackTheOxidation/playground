import Data.List

averageProbs :: ([Double], [Double]) -> Double
averageProbs probs = sum [x*y | x <- fst probs, y <- snd probs]

varianceProbs :: ([Double], [Double]) -> Double
varianceProbs probs = sum [(x - averageProbs probs) * (x - averageProbs probs) * y | x <- fst probs, y <- snd probs]

average :: Floating a => Integral b => [a] -> b -> a
average xs n = sum xs / fromIntegral n

