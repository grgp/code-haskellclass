doubleMe x = x +x
doubleUs x y = doubleMe x*2 + doubleMe y*2

whatisthis x = (if x == 0 then 1 else 0) * 5

qs :: Ord a => [a] -> [a]
qs [] = []
qs (p:xs) = (qs lesser) ++ [p] ++ (qs greater)
  where
    lesser = filter (< p) xs
    greater = filter (>= p) xs 

sayMe :: (Integral a) => a -> String  
sayMe 1 = "One!"  

sayMe' :: Int -> String
sayMe' 1 = "One!"

testConstant :: Int -> Int
testConstant x = 5 * x

iMissParentheses :: Int -> Int
iMissParentheses x = 5

odd' :: [Int] -> [Int]
odd' [] = []
odd' (x:xs)  | mod x 2 /= 0 = x : odd' xs
  | otherwise = odd' xs

append' :: [a] -> [a] -> [a]
append' [] ys = ys
append' (x:xs) ys = x : append' xs ys