merge :: [Int] -> [Int] -> [Int]
merge x [] = x
merge [] y = y
merge (x:xs) (y:ys)
  | x <= y    = x:(merge xs (y:ys))
  | otherwise = y:(merge (x:xs) ys)

msort :: [Int] -> [Int]
msort [] = []
msort [a] = [a]
msort xs = merge (msort (take n xs)) (msort (drop n xs))
           where
           n = div (length xs) 2