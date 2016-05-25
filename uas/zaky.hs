import Prelude hiding (zipWith)
import Zakydef

-- fibonacci tidak berhingga [0, 1, 1, 2, 3, 5]
fibs :: [Int]
fibs = 0 : 1 : zipWith (+) fibs (tail fibs)

fib :: Int -> Int
fib n = fibs!!n

-- faktorial barisannya tidak hingga [1, 1, 2, 6, 24]
facs :: [Int]
facs = 1 : 1 : zipWith (*) (from 2) (tail facs)

-- intersum
intersum :: Num a => [a] -> [a]
intersum (x:y:xs) = [(x+y)] ++ intersum (y:xs)

merge :: Ord a => [a] -> [a] -> [a]
merge (x:xs) (y:ys) = [x, y] ++ merge xs ys

-- iterates 
iterates :: (a -> a) -> a -> [a]
iterates f x = x : (map f (iterates f x))

akar :: Double -> Double
akar d = (iterates (\x -> (d/x + x)/2) d)!!10

akar' :: Double -> Double
akar' d = help d
					where help n = (d/(help n) + (help n))/2

flip' :: (a -> b -> c) -> (b -> a -> c)
flip' f = (\x y -> f y x)
--flip' f = g 
--	where g x y = f y x

zip' :: [a] -> [b] -> [(a,b)]
zip' = zipWith (\x y -> (x,y)) 