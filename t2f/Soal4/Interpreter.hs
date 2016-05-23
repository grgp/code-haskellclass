-- Soal 4
-- George Albert
-- 1406569781
-- Pemdek C

module Soal4.Interpreter
(
  run,
  Program
) where

import Soal4.Store

data AritExp = 
     Lit Int |
     Var String |
     Add AritExp AritExp |
     Mul AritExp AritExp |
     Sub AritExp AritExp
     deriving (Show)

type State = Store

aEval :: AritExp -> State -> Int
aEval (Lit x) _     = x
aEval (Var v) s     = (value s v)
aEval (Add (Lit e1) (Lit e2)) _ = e1 + e2
aEval (Add e1 e2) s = (aEval e1 s) + (aEval e2 s)
aEval (Mul (Lit e1) (Lit e2)) s = e1 * e2
aEval (Mul e1 e2) s = (aEval e1 s) * (aEval e2 s)
aEval (Sub (Lit e1) (Lit e2)) s = e1 - e2
aEval (Sub e1 e2) s = (aEval e1 s) - (aEval e2 s)

data BoolExp = T |  -- True
               F |  -- False
               Equal AritExp AritExp | -- equality of arithmetic expressions
               Lt AritExp AritExp | -- less than, E1 < E2, of arithmetic exp
               Neg BoolExp | -- negation
               And BoolExp BoolExp | -- and
               Or BoolExp BoolExp -- or
               deriving (Show)

bEval :: BoolExp -> State -> Bool
bEval T s = True
bEval F s = False
bEval (Equal e1 e2) s = aEval (Sub e1 e2) s == 0
bEval (Lt e1 e2) s    = aEval (Sub e1 e2) s < 0
bEval (Neg T) s       = False
bEval (Neg F) s       = True
bEval (Neg e1) s      = not (bEval e1 s)
bEval (And T T) s     = True
bEval (And F _) s     = False
bEval (And _ F) s     = False
bEval (And e1 e2) s   = (bEval e1 s) && (bEval e2 s)
bEval (Or T _) s      = True
bEval (Or _ T) s      = True
bEval (Or F F) s      = False
bEval (Or e1 e2) s    = (bEval e1 s) || (bEval e2 s)

data Program = Ass String AritExp | -- assignment sebuah variable
               Skip | -- statement kosong (do nothing)
               Seq Program Program | -- sequential statement composition
               If BoolExp Program Program | -- if b then p1 else p2
               While BoolExp Program -- while
               deriving (Show)

run :: Program -> State -> State
run (Ass str exp) s   = update s str (aEval exp s)
run (Skip)        s   = initial
run (Seq p1 p2)   s   = run p2 (run p1 s)
run (If b1 p1 p2) s
  | (bEval b1 s) = run p1 s
  | otherwise    = run p2 s
run (While b1 p1) s
  | (bEval b1 s) = run (While b1 p1) (run p1 s)
  | otherwise    = s

fac :: Program
fac = Seq (Ass "x" (Lit 5))
          (Seq (Ass "y" (Lit 1))
              (While (Neg (Equal (Var "x") (Lit 0)))
                (Seq (Ass "y" (Mul (Var "x") (Var "y")))
                  (Ass "x" (Sub (Var "x") (Lit 1))))))

 --prekondisi/initial state
s0 :: State
s0 = initial

-- kondisi akhir state
-- variable y berisi nilai 5!
s1 :: State
s1 = run fac s0

-- jawaban ada di variable "y"
y :: Int
y = value s1 "y"


