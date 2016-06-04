foo :: (Num a, Eq a) => a -> [a] -> [a]
foo x y = [x' | x' <- map (+3) y, x == x']

goo :: (Num a, Ord a) => a -> a -> [a] -> Bool
goo x y z = foo x z > foo y z

koo :: Ord a => a -> [a] -> [a]
koo x y = [x' | (x',i) <- zip y [1..z], x' < x]
          where z = length y

data Segitiga = Segitiga {
                  a :: Double,
                  t :: Double
                }

instance Eq Segitiga where
  (==) = eqTri
  --(Segitiga a t) == (Segitiga b u)  = (a * t) == (b * u)

eqTri :: Segitiga -> Segitiga -> Bool
eqTri (Segitiga a t) (Segitiga b u) = (a * t) == (b * u)

class Visible a where
  cetak :: a -> String

instance Visible a => Visible [a] where
  cetak []   = ""
  cetak (x:xs) = (cetak x) ++ ", " ++ (cetak xs)

instance Visible Segitiga where
  cetak (Segitiga a t) = "seg a:" ++ show(a) ++ " t:" ++ show(t)

--get 2 (filter even (ff 1 2))
--get 2 (filter even (1 : ff 2 (3)))
--get 2 (filter even (1 : 2 : ff 3 (5)))
--get 2 (filter even (1 : 2 : 3 : ff 5 (8)))
