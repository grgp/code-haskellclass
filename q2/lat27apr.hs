data Expr = Lit Int |
            Add Expr Expr |
            Sub Expr Expr

data NTree = NilT |
             Node Int NTree NTree
             deriving (Show) 

reflect :: NTree -> NTree
reflect NilT = NilT
reflect (Node n left right) = Node n (reflect right) (reflect left)

max' :: NTree -> Int
max' NilT = 0
max' (Node n left right) = max n (max (max' left) (max' right))

data LockerState = Taken | Free deriving (Show, Eq)  
type Code = String  
type LockerMap = [(Int, LockerState, Code)]

lockerLookup :: Int -> LockerMap -> String
lockerLookup n [] = "gaada loker dengan kode itu"
lockerLookup n (x:xs)
    | (n == fstTripel x) && (sndTripel x == Free)  = (trdTripel x)
    | (n == fstTripel x) && (sndTripel x == Taken) = "udah keambil"
    | otherwise                                    = lockerLookup n xs

fstTripel :: (a, b, c) -> a
fstTripel (a, b, c) = a 

sndTripel :: (a, b, c) -> b
sndTripel (a, b, c) = b

trdTripel :: (a, b, c) -> c
trdTripel (a, b, c) = c
             
--testcase :: NTree
--testcase = Node 14 (Node 10 (Node 23 NilT NilT) (Node 81 NilT NilT)) (Node 6 NilT NilT)

sumTree :: NTree -> Int
sumTree NilT = 0
sumTree (Node n left right) = n + (sumTree left) + (sumTree right)

occurs :: NTree -> Int -> Int
occurs NilT x = 0
occurs (Node n left right) x
    | (n == x) = 1 + occurLR
    | otherwise = occurLR
    where occurLR = (occurs left x) + (occurs right x)

change :: NTree -> Int -> Int -> NTree
change NilT m n = NilT
change (Node x left right) m n
    | x == m = Node n (change left m n) (change right m n)
    | otherwise = Node x (change left m n) (change right m n)

treeInOrder :: NTree -> [Int]
treeInOrder NilT = []
treeInOrder (Node x left right) = (treeInOrder left) ++ [x] ++ (treeInOrder right)



--data List a = NilList | Cons a (List a)
--              deriving (Eq, Ord, Show, Read)

--length' NilList = 0
--length' (Cons a t) = 1 + length' t

--listcase :: List
--listcase = Cons 1 (Cons 2 (Cons 3 NilList))

data Vehicle = Bike |
               Motorbike Int |
               Car Bool |
               Lorry Int

wheels :: Vehicle -> Int
wheels Bike          = 2
wheels (Motorbike _) = 2
wheels (Car True)    = 3
wheels (Car _)       = 4
wheels (Lorry x)     = x

totalWheels :: [Vehicle] -> Int
totalWheels x = foldr (+) 0 (map wheels x)

--testVehicle :: [Vehicle]
--testVehicle = [Car False, Motorbike 411, Car True]

data Maybe' a = Nothing' | Just' a
               deriving (Eq, Ord, Read, Show)

mapMaybe' ::(a -> b) -> Maybe' a -> Maybe' b
mapMaybe' g Nothing'   = Nothing'
mapMaybe' g (Just' x)  = Just' (g x)


data Edit = Change Char |
            Copy |
            Delete |
            Insert Char |
            Kill
            deriving (Eq, Show) 

transform :: String -> String -> [Edit]
transform [] [] = []
transform xs [] = [Kill]
transform [] ys = map Insert ys
transform (x:xs) (y:ys)
    | x == y   = Copy : transform xs ys
    | otherwise = best [Delete   : transform xs (y:ys),
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
    move      :: Vector -> a -> a
    reflectX  :: a -> a
    reflectY  :: a -> a
    rotate180 :: a -> a
    rotate180 = reflectX . reflectY

data Point = Point Float Float
             deriving Show

instance Movable Point where
    move (Vec v1 v2) (Point c1 c2) = Point (c1 + v1) (c2 + v2)
    reflectX (Point c1 c2)   = Point c1 (-c2)
    reflectY (Point c1 c2)   = Point (-c1) c2
    rotate180 (Point c1 c2)  = Point (-c1) (-c2)

class Visible a where
  toString :: a -> String
  size     :: a -> Int
  compare'  :: a -> a -> Bool 

instance Visible Bool where
  compare' x y   = size x <= size y
  toString True  = "True"
  toString False = "False"
  size _         = 0

instance (Visible a, Visible b) => Visible (a, b) where
  compare' x y    = size x <= size y
  toString (a, b) = show (toString a, toString b)
  size _          = 2

instance (Visible a, Visible b, Visible c) => Visible (a, b, c) where
  compare' x y            =  size x <= size y
  toString (a, b, c) =  show (toString a, toString b, toString c)
  size _                  =  3

--instance Eq a, Eq b => Visible (a, b) where
--  func = 

--instance Visible Char where
--  toString ch = [ch]
--  size _      = 1