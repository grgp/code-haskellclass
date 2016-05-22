-- handout last
jum :: Int -> Int -> Int -> Int
jum x y z = x + y + z

uncurry' :: (a -> b -> c -> d) -> ((a, b, c) -> d)
uncurry' f (a, b, c) = f a b c

curry' :: ((a, b, c) -> d) -> (a -> b -> c -> d)
curry' f a b c = f (a, b, c) 

exists' :: (a -> Bool) -> [a] -> Bool
exists' p [] = False
exists' p (x:xs) = p x || exists' p xs

elem' :: Int -> [Int] -> Bool
elem' x ys = foldr(\y acc -> acc || (if y == x then True else False)) False ys

elem'' :: Int -> [Int] -> Bool
elem'' x ys = exists' (==x) ys

intersection :: Eq a => [a] -> [a] -> [a]
intersection [a] [] = []
intersection [] [b] = []
intersection [a] [b]
  | a == b    = [a]
  | otherwise = []
intersection (a:as) (b:bs)
  | exists' (==a) bs = a:(intersection as (b:bs))
  | otherwise = (intersection as (b:bs))

bubble' :: Ord a => [a] -> [a]
bubble' []     = []
bubble' [x]    = [x]
bubble' (x:y:zs)
  | (length zs == 0) && (x > y) = [y] ++ [x]
  | (length zs == 0) && (x > y) = [x] ++ [y]
  | (x > y)    = y:(bubble' ([x] ++ zs))
  | (x <= y)   = x:(bubble' ([y] ++ zs))
  | otherwise = []

f = (\ k ->
        if (k < 0) then "yey" else "no")

fg = (\ls -> [x+2 <- ls])