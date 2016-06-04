import Data.List (intercalate)


pascal :: Int -> [Int]
pascal 0 = [1]
pascal n = zipWith (+) (appendZero prev) (prependZero prev)
  where
    prev = pascal $ n - 1
    appendZero = (0 :)
    prependZero = (++ [0])

main :: IO ()
main =
      readLn >>= \n -> 
      let ans = (pascal `map` [0..n-1])
      in putStr $ unlines $ toStr `map` ans
      where
        toStr = intercalate " " . map show

--(>>=) :: Monad m => m a -> (a -> m b) -> m b

--instance Monad Maybe where
--  return x    = Just x
--  Nothing >>= _ = Nothing
--  (Just x) >>= f = f x

main' :: IO ()
main' = do n <- getLine
           putStrLn n

--uncurry' :: a -> b -> c -> (a, b) -> c
