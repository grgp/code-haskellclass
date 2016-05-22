class Visible a where
  toString  :: a -> String
  size      :: a -> Int

instance Visible Char where
  toString cb = [cb]
  size _      = 1

instance Visible Bool where
  toString True   = "True"
  toString False  = "False"
  size _          = 1

class YesNo a where
  value     :: a -> Bool

instance YesNo Int where
  value 0   = False
  value _   = True

instance YesNo Bool where
  -- value x   = x
  value = id

val :: Int
val = 10

val2 :: Int
val2 = 0

val3 :: Char
val3 = 'a'

-- lookupFirst :: Eq a => [(a,b)] -> a -> [b]
-- lookupFirst pairList x = [ z | (y,z) <- pairList, y == x]

-- instance (Visible a, Visible b) => Visible (a, b) where
--  toString 

minInt :: Int
minInt = minBound

maxInt :: Int
maxInt = maxBound

minBool :: Bool
minBool = minBound

double x = x*2

instance (Show a, Show b) => Show (a -> b) where
  show _ = "*function*"

-- [1, 2, 3] + [2, 3] ~> [3, 5]

addList :: Num a => [a] -> [a] -> [a]
addList xs ys = map (\n -> fst n + snd n) (zip xs ys)

--instance (Show a, Show b) => Show (a -> b) where


instance (Num a, Num b) => Num (a, b) where
  (x, y) + (m, n) = (x+m, y+n)

instance (Num a) => Num [a] where
  (+) = zipWith (+)