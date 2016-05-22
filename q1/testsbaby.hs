drop' :: Int -> [a] -> [a]
drop' 0 ys = ys
drop' _ [] = []
drop' x (y:ys)
    | x>0 = drop' (x-1) ys