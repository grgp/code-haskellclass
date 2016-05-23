guardsBetter :: Int -> Int
guardsBetter x
  | x<0 = -1
  | x>0 = 1
  | otherwise = 0

guardsBetter' :: Int -> Int
guardsBetter' y = if y>0 then 1
                  else if y<0 then -1
                    else 0