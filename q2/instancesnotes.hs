Context

#1
member :: Eq a => a -> [a] -> Bool

#2
instance Visible a => Visible [a] where

#3
Class Eq a => Ord a where
	...
	(inheritance)

succ False True
pred True  False

[False ..] ~> [False, True]
[1.. 8]

[1, 2, 3] + [2, 3] ~> [3, 5]