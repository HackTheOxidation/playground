import Control.Concurrent

f :: Int -> Int -> IO ()
f a b = do
  let x = a + b
  putStrLn $! show x

main :: IO ()
main = do
  forkIO $ f 1 2
  putStrLn "Hello from main thread"
