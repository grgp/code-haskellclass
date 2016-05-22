-- Nama = George Albert
-- NPM  = 1406569781
-- Kelas= Pemdek C

-- types, database, etc.
import Prelude hiding (lookup)

type Name       = String
type Price      = Int
type NumItem    = Int
type BarCode    = Int
type Database   = [(BarCode, Name, Price)]
type TillType   = [BarCode]
type CodeNumItem= [(BarCode, NumItem)]
type BillType   = [(Name, NumItem, Price)]

codeIndex :: Database
codeIndex = [(4719, "Fish Fingers", 121),
             (5743, "Nappies", 1010),
             (3814, "Orange Jelly", 56),
             (1111, "Hula Hoops", 21),
             (1112, "Hula Hoops (Giant)", 133),
             (1234, "Dry Sherry, 1lt", 540)]

lineLength :: Int
lineLength = 30

-- kode dari slide "Programming with Lists"
sort :: [Int] -> [Int]
sort [] = []
sort (x:xs) = sort [ y | y<-xs, y<=x ] ++ [x] ++
                   sort [ y | y<-xs, y>x]

-- function 1
formatPence :: Price -> String
formatPence x = show(x `div` 100) ++ "." ++ 
  if x `mod` 100 <= 10 
    then '0' : show(x `mod` 100)
  else show(x `mod` 100)

-- function 2
formatLine :: (Name, NumItem, Price) -> String
formatLine (x,y,z) = x ++ "....(" ++ show(y) ++ "," ++ formatPence(z) ++ ")" ++ "\n"

-- function 3
formatLines :: [(Name, NumItem, Price)] -> String
formatLines [] = []
formatLines (x:xs) = formatLine(x) ++ formatLines xs

-- function 4
makeTotal :: BillType -> Price
makeTotal [] = 0
makeTotal ((x,y,z):xs) = z + makeTotal xs

-- function 5
formatTotal :: Price -> String
formatTotal x = "\n..................." ++ formatPence(x)

-- function 6
formatBill :: BillType -> String
formatBill x =
  "Haskell Store\n" ++
  formatLines x ++
  formatTotal (makeTotal x)

-- function 7
look :: Database -> BarCode -> (Name, Price)
look [] b = ("Unknown Item", 0)
look ((x,y,z):ds) b
  | b == x    = (y,z)
  | otherwise = look ds b

-- function 8
lookup :: BarCode -> (Name, Price)
lookup x = look codeIndex x

-- function 9
makeCodeNumItem :: TillType -> CodeNumItem
makeCodeNumItem (x) = findPoints (sort (x)) 0 -- quicksort is defined above

findPoints :: [Int] -> Int -> CodeNumItem     -- find points of difference
findPoints [] k     = []
findPoints (x:xs) k
  | xs == []        = [(x, k+1)]
findPoints (x:y:zs) k
  | x /= y          = (x, k+1) : findPoints (y:zs) 0
  | x == y          = (findPoints (y:zs) (k+1))

-- function 10
makeBill :: CodeNumItem -> BillType
makeBill [] = []
makeBill ((x,y):zs) = includeNum y (lookup (x)) ++ makeBill zs

includeNum :: NumItem -> (Name, Price) -> BillType
includeNum y (n,p) = [(n,y,p)]

-- function 11
makeDiscount :: BillType -> Int
makeDiscount [] = 0
makeDiscount ((x,y,z):ms)
  | x == "Dry Sherry, 1lt" = (y `div` 2) * 100 + makeDiscount ms
  | otherwise = makeDiscount ms

--makeBill :: CodeNumItem -> BillType
--which takes a list of (BarCode, NumItem) into a list of (Name, NumItem, Price) triples.

--formatBill :: BillType -> String
--which takes a list of (Name, NumItem, Price) triples into a formatted bill, and

--produceBill :: TillType -> String
--which will combine the effets of makeCodeNumItem, makeBill and formatBill, thus. produceBill [=] formatBill . makeBill . makeCodeNumItem