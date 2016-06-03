(-:) :: a -> (a -> b) -> b
(-:) x f = f x

type Birds = Int
type Pole = (Birds,Birds)

landLeft :: Birds -> Pole -> Maybe Pole
landLeft n (left, right)
	| abs (left+n - right) > 3 = Nothing
	| otherwise = Just (left+n, right)

landRight :: Birds -> Pole -> Maybe Pole
landRight n (left, right)
	| abs (right+n - left) > 3 = Nothing
	| otherwise = Just (left, right+n)

