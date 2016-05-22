merge :: [Int] -> [Int] -> [Int]
merge x [] = x
merge [] y = y
merge (x:xs) (y:ys)
  | x <= y    = x:(merge xs (y:ys))
  | otherwise = y:(merge (x:xs) ys)

msort :: [Int] -> [Int]
msort [] = []
msort [a] = [a]
msort x = merge 
          (msort ( take ((length x) `div` 2) x ))
          (msort ( drop ((length x) `div` 2) x ))

