averageThree :: Int -> Int -> Int -> Double
averageThree x y z = fromIntegral (x + y + z) / 3

howManyAboveAverage :: Int -> Int -> Int -> Int ->
howManyAboveAverage x y z
	| (averageThree x y z)