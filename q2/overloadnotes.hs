-- polymorphic function
   -- nama fungsinya sama, tipe berbeda, definisi sama

-- overloaded function
   -- setiap tipe, fungsinya berbeda
      -- ex (==), (+)
      --    5 == 5, or [5,3,4] == [5,3,7]

elemGen isSama x []     = False
elemGen isSama x (y:ys) = isSama x y || elemGen isSama x ys

elemInt   = elemGen (==Int)
elemBool  = elemGen (==Bool)

elem :: (Eq a) => a -> [a] -> Bool

Value/Nilai -> Type -> TypeClass/Class
TypeClass kumpulan tipe yg punya sifat sama, yg didefinisikan dari operatornya
TypeClass sudah built in

Class Eq a where
  (==) :: a -> a -> Bool

Eq -> Basic Types (Int, Bool, Char, Float)
   |
   |-> list, tuple,...
       [Int], (Int, Bool)

Eq not applicable to Function
    incr == plus, doesn't make any sense

elem :: Eq a => a -> [a] -> Bool

instance Visible Char where
	toString ch = [ch]
	size _			= 1

instance Eq Bool where
	True  == True		= True
	False == False 	= False
	_			== _			= False

instance Visible a => Visible [a] where
	toString 	= concat . map toString
	size			= foldr (+) 1 . map size

instance Visible )) Int, Float, Double 	[V]
instance Visible )) String, Int 				[X]

YesNo
value :: a -> Bool

Int
Value 0 = False
Value   = True

Bool
Value False = False
Value True  = True

[Int]
Value [] = False
Value    = True

------

class Visible a where
	toString 	:: a -> String
	size 			:: a -> Int

instance Visible Char where
	toString cb = [cb]
	size _			= 1

instance Visible Bool where
	toString True 	= "True"
	toString False 	= "False"
	size _ 					= 1