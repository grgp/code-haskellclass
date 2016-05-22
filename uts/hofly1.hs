maxCurr :: (Int -> Int -> Int)
maxCurr a b = (if a > b then a else b)

multThree :: (Num a) => a -> a -> a -> a  
multThree x y z = x * y * z  

compareTo50 = compare 50

compareWithHundred :: (Num a, Ord a) => a -> Ordering
compareWithHundred  = compare 100