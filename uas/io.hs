import System.IO 

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
guess word = do putStr "> "
                w <- getLine
                if word == w then
                   putStrLn $ "\nYou win woooooooo!"
                else do
                   putStrLn $ (diff word w)
                   guess word

hangman :: IO ()
hangman = do putStrLn "\nThink of a word:"
             word <- sgetLine
             putStrLn "\nTry to guess it:"
             guess word

strlen :: IO ()
strlen = do str <- getLine
            putStrLn $ "The string has " ++ show(length str) ++ " characters"

--guess :: String -> IO ()
--guess s = Nothing