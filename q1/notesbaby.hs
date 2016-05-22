--digits :: String -> String
--digits st = [ch | ch <- st, isDigit ch]
--
--firstDigit :: String -> Char
--firstDigit st
--    = case (digits st) of
--        [] -> '\0'
--        (x:_) −> x

iSort :: [Int] -> [Int]
iSort []        = []
iSort (x:xs)    = ins x (iSort xs)

ins :: Int -> [Int] -> [Int]
ins x []        = [x]
ins x (y:ys)
    | x <= y    = x:(y:ys)
    | otherwise = y : ins x ys

interleave :: [a] -> [a] -> [a]
interleave [] [] = []
interleave xs [] = xs
interleave [] ys = ys
interleave (x:xs) (y:ys) =
    x : y : interleave xs ys

zip :: [a] -> [b] -> [(a,b)]
zip [] [] = []
zip xs [] = []
zip [] ys = []
-- ternyata tiga diatas sama
zip (x:xs) (y:ys):
    (x,y) : zip xs ys
--zip _ _ = [] --taruhdiatas

drop :: Int -> [a] -> [a]
drop 0 ys = ys
drop _ [] = []
drop x (y:ys)
    | n>0 = drop (n-1) xs
drop ...