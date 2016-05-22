foldr1' :: (a -> a -> a) -> [a] -> a
foldr1' f [x] = x
foldr1' f (x:xs) = f x (foldr1' f xs)

mult :: Int -> Int -> Int
mult m n
  | m==0 = 0
  | m>0 = (mult (m-1) n) * n

composeList :: [(a -> a)] -> (a -> a)
composeList listFs = foldr (.) id listFs

twice :: (a -> a) -> (a -> a)
twice f = (f . f)

double :: Int -> Int
double x = x + x

triple :: Int -> Int
triple x = x + x + x

iter :: Int -> (a -> a) -> (a -> a)
iter n f
  | n > 0     = f . iter (n-1) f
  | otherwise = id

power2 :: Int -> Int
power2 n = (iter n double) 1

addNum :: Int -> (Int -> Int)
addNum n = addN 
           where addN m = n+m

addNum' :: Int -> (Int -> Int)
addNum' n = (\m -> m + n)

comp2 :: (a -> b) -> (b -> b -> c) -> (a -> a -> c)
comp2 f g = (\x y -> g (f x) (f y))

--mapLa :: (a -> a) -> [a] -> [a]
--mapLa f (x:xs) = (\x -> f x) : mapLa f xs

add :: Int -> Int -> Int
add x y = x + y

add' :: Int -> Int -> Int
add' x y = add y x

sum7 :: Int -> Int
sum7 = add 7

multiply :: Int -> Int -> Int
multiply x y = x*y

curry :: ((a,b) -> c) -> (a -> b -> c)
curry g x y = g (x,y)

--uncurry :: (a -> b -> c) -> (a,b) -> c
--uncurry g x y = g (x,y)

