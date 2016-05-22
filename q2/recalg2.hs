data Either' a b = Left' a | Right' b
                  deriving (Eq,Ord,Read,Show)

data Maybe' a  = Nothing' | Just' a
               deriving (Eq,Ord,Read,Show)

errDiv :: Int -> Int -> Maybe' Int
errDiv n m
  | (m /= 0)  = Just' (n `div` m)
  | otherwise = Nothing'

mapMaybe' :: (a -> b) -> Maybe' a -> Maybe' b
mapMaybe' g Nothing' = Nothing'
mapMaybe' g (Just' x) = Just' (g x)

maybe' :: b -> (a -> b) -> Maybe' a -> b
maybe' n f Nothing' = n
maybe' n f (Just' x) = f x

data Edit = Change Char |
            Copy |
            Delete |
            Insert Char |
            Kill
            deriving (Eq,Show)

transform :: String -> String -> [Edit]
transform [] [] = []
transform xs [] = [Kill]
transform [] ys = map Insert ys
transform (x:xs) (y:ys)
  | x == y      = Copy : transform (x:xs) (y:ys)
  | otherwise   = best [ Delete   : transform xs (y:ys),
                         Insert y : transform (x:xs) ys,
                         Change y : transform xs ys ]

best :: [[Edit]] -> [Edit]
best [x] = x
best (x:xs)
  | cost x <= cost b = x
  | otherwise        = b
    where
      b = best xs

cost :: [Edit] -> Int
cost = length . filter (/=Copy)

data Vector = Vec Float Float

class Movable a where
  move :: Vector -> a -> a
  reflectX :: a -> a
  reflectY :: a -> a
  rotate180 :: a -> a
  rotate180 = reflectX . reflectY

data Point = Point Float Float
             deriving Show

instance Movable Point where
  move (Vec v1 v2) (Point c1 c2)
    = Point (c1+v1) (c2+v2)
  reflectX (Point c1 c2) = Point c1 (-c2)
  reflectY (Point c1 c2) = Point (-c1) c2
  rotate180 (Point c1 c2) = Point (-c1) (-c2)
