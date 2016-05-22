--  TC
--  GATC
--  
--  if the first sequence is shorter then manipulate the spaces in the first one
--    add them until they fit
--  
--  TC__
--  GATC
--  
--  __TC
--  _T_C
--  _TC_
--  T_C_
--  TC__
--  
--  for each possible sequence, calculate the edit distance
--  get the best configuration from those (use tuples?)

--showAlign :: String -> String -> String
--showAlign x y = alignSeqs (getDiff x y)

getDiff :: String -> String -> (String, String)
getDiff x y
  | length x <= length y  = (x, y)
  | otherwise             = (y, x)

--alignSeqs :: (String, String) -> String
--alignSeqs = 

fillSpaces :: String -> Int -> String
fillSpaces x 0 = x
fillSpaces x n = fillSpaces x (n-1)

addSpace :: String -> Int -> [String]
addSpace w n = slice 0 n w : 

slice :: Int -> Int -> String -> String
slice from to xs = take (to - from + 1) (drop from xs)

compSequences :: String -> String -> (String, String, Int)
compSequences x y
  | length x <= length y = (x, y, (length y - length x))
  | otherwise = (y, x, (length x - length y))

-- get the best variation of the shorter sequence
best :: [String] -> String
best [x] = x
best (x:xs)
  | score x <= score b = x
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