data Either' a b = Left' a | Right' b
									deriving (Eq, Ord, Read, Show)

data Figure = Line {
								x :: Point,
								y :: Point
							} |
							Circle {
								r :: Point,
								t :: Float
						} deriving Show

data Point = Point Flo Flo
						 deriving Show

type Flo = Float