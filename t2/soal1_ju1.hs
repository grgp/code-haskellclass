showAlign :: String -> String -> String
showAlign x y
  | length x <= length y  = makeSpace (transform x y)
  | otherwise             = (transform y x)

transform :: String -> String -> String
transform [] [] = []
transform (x:xs) [] = '*' : (transform xs [])
transform [] (y:ys) = '*' : (transform [] ys)
transform (x:xs) (y:ys)
  | x == y      = '+' : (transform xs ys)
  | otherwise   = best [
                  '*' : (transform (x:xs) ys),
                  '-' : (transform xs ys)
                  ]

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

makeSpace :: String -> String -> String
