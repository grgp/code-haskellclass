module Store
  ( Store,
    initial,   -- Store
    value,     -- Store -> Var -> Int
    update     -- Store -> Var -> Int -> Store
  ) where

data Store = Sto [ (Int, Var) ]
type Var = Char

initial :: Store
initial = Sto []

value  :: Store -> Var -> Int
value (Sto []) v  = 0
value (Sto ((n,w):sto)) v
  | v==w          = n
  | otherwise     = value (Sto sto) v

update  :: Store -> Var -> Int -> Store
update (Sto sto) v n = Sto ((n,v):sto)

run :: Program -> State -> State

data AritExp = 
     Lit Int |
     Var String |
     Add AritExp AritExp |
     Mul AritExp AritExp |
     Sub AritExp AritExp

type State = Store

aEval :: AritExp -> State -> Int
aEval (Lit 1) _     = 1
aEval (Var v) s     = (value s v)
aEval (Add (Lit e1) (Lit e2)) _ = e1 + e2
aEval (Mul (Lit e1) (Lit e2)) _ = e1 * e2
aEval (Sub (Lit e1) (Lit e2)) _ = e1 - e2

data BoolExp = T |  -- True
               F |  -- False
               Equal AritExp AritExp | -- equality of arithmetic expressions
               Lt AritExp AritExp | -- less than, E1 < E2, of arithmetic exp
               Neg BoolExp | -- negation
               And BoolExp BoolExp | -- and
               Or BoolExp BoolExp -- or

bEval :: BoolExp -> State -> Bool
bEval T _ = True
bEval F _ = False
bEval (Equal )


fac :: Program
fac = Seq (Ass "x" (Lit 5))
          (Seq (Ass "y" (Lit 1))
              (While (Neg (Equal (Var "x") (Lit 0)))
                (Seq (Ass "y" (Mul (Var "x") (Var "y")))
                  (Ass "x" (Sub (Var "x") (Lit 1))))))