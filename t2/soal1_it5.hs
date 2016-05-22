showAlign :: String -> String -> String
showAlign x y
  | length x <= length y  = transform x y
  | otherwise             = transform y x

transform :: String -> String -> String
transform [] [] = []
transform xs [] = []
transform [] (y:ys) = best (append (transform [] ys) '*')
transform (x:xs) (y:ys)
  | x == y      = best (
                  (append (transform xs ys) '+') ++
                  (append (transform (x:xs) ys) '*') ++
                  (append (transform xs ys) '-')
                  )
  | otherwise   = best (
                  append (transform (x:xs) ys) '*' ++
                  append (transform xs ys) '-'
                  )

append :: String -> Char -> [String]
append [] y     = [[y]]
append (x:xs) y = [[y] ++ x] ++ append xs y 
--append (x:xs) y = [([y] ++ x)] ++ append xs y 




-- get the best variation of the shorter sequence
best :: [String] -> String
best [x] = x
best (x:xs)
  | score x >= score b = x
  | otherwise          = b
    where
      b = best xs

-- calculate the sequence's score
score :: String -> Int
score [] = 0
score (x:xs)
  | x == '+'  = 1 + score xs
  | x == '-'  = (-1) + score xs
  | x == '*'  = (-2) + score xs


