-- dari slide
class Visible a where
  toString :: a -> String
  size     :: a -> Int
  compare'  :: a -> a -> Bool -- soal no [3]

-- soal no [4]
instance Visible Bool where
  toString True   = "True"
  toString False  = " False"
  size _          = 1
  compare' x y    = size x <= size y

instance (Visible a, Visible b) => Visible (a, b) where
  toString (a, b) = show (toString a, toString b)
  size _          = 1

instance (Visible a, Visible b, Visible c) => Visible (a,b,c) where
  toString (a,b,c) = show (toString a, toString b, toString c)
  size _           = 1

instance Visible Char where
  toString x = x : []
  size x = 1
  compare' x y = size x <= size y

--instance Eq (Maybe m) where
--  Just x == Just y = x == y
--  Nothing == Nothing = True
--  _ == _ = False

-- soal no [5]

--instance (Ord a, Ord b) => Ord (a,b) where
--  (x,y) < (w, z)  = (x < w)

instance Visible Bo