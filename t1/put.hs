-- Nama = George Albert
-- NPM  = 1406569781
-- Kelas= Pemdek C

-- types, database, etc.
import Prelude hiding (lookup)
import Data.List

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
--sort :: [Int] -> [Int]
--sort [] = []
--sort (x:xs) = sort [ y | y<-xs, y<=x ] ++ [x] ++
                   --sort [ y | y<-xs, y>x]

-- function 1
formatPence :: Price -> String
formatPence x = show(x `div` 100) ++ "." ++ 
  if x `mod` 100 <= 10 
    then '0' : show(x `mod` 100)
  else show(x `mod` 100)

-- function 2
formatLine :: (Name, NumItem, Price) -> String
formatLine (x,y,z) = 
  x ++ 
  (replicate (calcDots (x,y,z)) '.') ++
  "(" ++ show(y) ++ "," ++ formatPence(z) ++ ")" ++ "\n"

calcDots :: (Name, NumItem, Price) -> Int
calcDots (x,y,z) = 
  lineLength - length x - length (show(y)) - length (formatPence(z)) - 4

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
formatTotal x = "\nTotal...................." ++ formatPence(x)

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
makeCodeNumItem (x) = iterateItems (sort (x)) 0 -- sort is defined above

iterateItems :: [Int] -> Int -> CodeNumItem     -- count by finding points 
iterateItems [] k     = []                      -- of difference in the list
iterateItems (x:xs) k
  | xs == []        = [(x, k+1)]
iterateItems (x:y:zs) k
  | x /= y          = (x, k+1) : iterateItems (y:zs) 0
  | x == y          = (iterateItems (y:zs) (k+1))

mCNI :: TillType -> CodeNumItem
mCNI []   = []
mCNI [x]  = [(x,1)]
mCNI (x:xs) = (head(sort(x:xs)), occur (head(sort(x:xs))) (sort(x:xs))) :  mCNI (tail(sort (x:xs)))
  -- | x == last xs  = sort ((x, (occur x (x:xs))) : mCNI (pantatHilang xs))
  -- | otherwise     = sort ((x, (occur x (x:xs))) : mCNI xs)

occur :: BarCode -> TillType -> Int
occur x [] = 0
occur x (y:ys)
  | x == y        = 1 + occur x ys
  | otherwise     = occur x ys

cF :: [(BarCode, NumItem)] -> [(BarCode, NumItem)]
cF [] = []
cF [(x,y)] = [(x,y)]
cF ((v,w):(x,y):[])
  | v == x    = take 1 ((v,w):(x,y):[])
  | otherwise = ((v,w):(x,y):[])
cF ((u,v):(w,x):ys)
  | u == w    = cF ((u,v) : take 2 ((u,v):(w,x):ys))
  | otherwise = (u,v) : cF ((w,x):ys)


pantatHilang :: TillType -> TillType
pantatHilang [] = []
pantatHilang [x] = []
pantatHilang (x:xs) = take (length (x:xs) - 1) (x:xs)

-- function 10
makeBill :: CodeNumItem -> BillType
makeBill [] = []
--makeBill ((x,y):zs) = includeNum y (lookup (x)) ++ makeBill zs

includeNum :: NumItem -> (Name, Price) -> BillType
includeNum y (n,p) = [(n,y,p)]

-- function 11
makeDiscount :: BillType -> Int
makeDiscount [] = 0
makeDiscount ((x,y,z):ms)
  | x == "Dry Sherry, 1lt" = (y `div` 2) * 100 + makeDiscount ms
  | otherwise = makeDiscount ms

-- function 12, asumsi discount punya type yang sama dengan price
formatDiscount :: Int -> String
formatDiscount x = "\nDiscount................." ++ formatPence x ++ "\n"

-- function 13
formatBill' :: BillType -> String
formatBill' x =
  "Haskell Store\n" ++
  formatLines x ++
  formatDiscount (makeDiscount x) ++
  formatTotal (makeTotal x)

-- without discount
produceBill :: TillType -> String
produceBill = formatBill . makeBill . makeCodeNumItem

-- with discount
produceBill' :: TillType -> String
produceBill' = formatBill' . makeBill . makeCodeNumItem