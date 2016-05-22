import Prelude hiding (foldrl)

map' :: (Int -> Int) -> [Int] -> [Int]
map' f xs     = [f x | x <- xs]

--cara lain idk tho
--map :: (Int -> Int) -> [Int] -> [Int]
--map' f [] = []
--map' f (x:xs) = f x : map f xs

sum' :: [Int] -> Int
sum' [] = 0
sum' (x:xs) = x + sum' xs

tailList :: [[Int]] -> [[Int]]
tailList xs = map tail xs

total :: [(Int, Int)] -> Int
total xs = sum' (map kali xs)
  where 
    kali :: (Int, Int) -> Int
    kali (a,b) = a*b

foldr1 :: (a -> a -> a) -> [a] -> a
foldr1 f [x] = x
foldr1 f (x:xs) = f x (foldr1 f xs)

reverse' :: [a] -> [a]
reverse' xs = foldr snoc' [] xs

snoc' :: a -> [a] -> [a]
snoc' x xs = xs ++ [x]

ff :: Int -> Int -> Int
ff x s
  | x > 0  = s + 1
  | x <= 0 = s

numPositive :: [Int] -> Int
numPositive xs = foldr ff 0 xs