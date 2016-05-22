data Temp = Cold | Temp
      ^				^      ^ ~ (nullary)
 nama type    |      - (pattern matching)
 					constructor

  kalau ada constructor baru, pisah dgn |

1) type Name = String
   type Umur = Int
   type Person = (Nama, Umur)

   a :: Person
   a = ("Jaem", 23)

   a :: People
   a = Person "auto" 20
        ^- constructor

-- soal
Product Types

Definisikan sebuah tipe bernama Vehicle dgn 4 constructor | Vehicle dapat beruba Bike, Motorbike, Car dan Lorry

--

Cold = Cold [X]

data Temp = Cold | Hot     (Eq)
instance Eq Temp  where
	Cold == Cold	= True
	Hot  == Hot		= True
	_		 == _ 		= False



