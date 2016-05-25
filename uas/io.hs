import System.IO 

getCh :: IO Char
getCh = do hSetEcho stdin False
           c <- getChar
           hSetEcho stdin True
           return c

sgetLine :: IO String
sgetLine = 