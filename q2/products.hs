data Vehicle = Bike |
               Motorbike CC | -- or Motorbike Int |    -- CC
               Car Model | 
               Lorry Tires

type CC = Int
type Model = Bool
type Tires = Int

robin :: Vehicle
robin = Car True

listKendaraan :: [Vehicle]
listKendaraan = [Car True, Lorry 8, Lorry 10, Car False, Bike]

wheels :: Vehicle -> Int
wheels Bike = 2
wheels (Motorbike _) = 2
--wheels (Car True) = 3
--wheels (Car False) = 4
 -- or
wheels (Car t) = if t then 3 else 4
wheels (Lorry w) = w



data Temp = Cold | Hot     -- (Eq)
            deriving (Eq, Ord, Enum, Show, Read)

--data Shape = Circle Float |
--             Rectangle Float Float
--             deriving (Eq, Ord, Show, Read)

data Shape = Circle Float |
             Rectangle Float Float
             deriving Eq

instance Ord Shape where
  Circle x <= Rectangle v w = if (x*x*22/7) < (v*w) then True else False
  Circle x >= Rectangle v w = if (x*x*22/7) < (v*w) then True else False

--instance Eq Temp  where
--  Cold == Cold  = True
--  Hot  == Hot   = True
--  _    == _     = False

