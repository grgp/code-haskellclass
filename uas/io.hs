import System.IO 

theWord :: String
theWord = "haskell"

getCh :: IO Char
getCh = do hSetEcho stdin False
           c <- getChar
           hSetEcho stdin True
           putChar '-'
           return c

sgetLine :: IO String
sgetLine = do n <- getCh
              if n == '\n' then return []
              else (do m <- sgetLine
                       return (n:m))

diff :: String -> String -> String
diff [] ys      = []
diff (x:xs) ys  = (if elem x ys then x else '-') : diff xs ys

guess :: String -> IO ()
guess w
  | theWord == w  = putStrLn $ "You win!\n"
  | otherwise     = do putStrLn $ diff theWord w
                       hangman

hangman :: IO ()
hangman = do putStrLn "\nThink of a word:"
             word <- sgetLine
             putStrLn "\nTry to guess it:"
             guess word

--guess :: String -> IO ()
--guess s = Nothing