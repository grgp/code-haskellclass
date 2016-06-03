import System.IO 

getCh :: IO Char
getCh = do hSetEcho stdin False
           c <- getChar
           hSetEcho stdin True
           return c

sgetLine :: IO String
sgetLine = do n <- getCh
              if n == '\n' then return []
              else (do m <- sgetLine
                       return (n:m))

diff :: String -> String -> String
diff [] [] = []
diff xs [] = xs
diff [] ys = ys
diff (x:xs) (y:ys)
  | (x == y)    = x   : diff xs ys
  | otherwise   = '-' : diff xs ys

--guess :: String -> IO ()
--guess s = Nothing