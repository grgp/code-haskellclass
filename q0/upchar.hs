import Data.Char
upperCh :: Char -> Char
upperCh x = 
  chr (ord x - 32)

offsetCh :: Int
offsetCh = ord 'A' - ord 'a'

upperChA :: Char -> Char
upperChA x = chr (ord x + offsetCh)

kindofLetter :: Char -> [Char]
kindofLetter x
  | 'a' <= x && x <= 'z' = "huruf kecil"
  | 'A' <= x && x <= 'Z' = "huruf besar"
  | otherwise = "bukan huruf"

