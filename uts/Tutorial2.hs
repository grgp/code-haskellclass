headOr :: a -> [a] -> a
headOr x [] = x
headOr x (y:ys) = y 

sum' :: [Int] -> Int
sum' [] = 0
sum' (x:xs) = x + sum' xs

product' :: [Int] -> Int
product' [] = 1
product' (x:xs) = x * product' xs

length' :: [a] -> Int
length' [] = 0
length' (x:xs) = 1 + length xs