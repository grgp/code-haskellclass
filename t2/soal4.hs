run :: Program -> State -> State

fac :: Program
fac = Seq (Ass "x" (Lit 5))
          (Seq (Ass "y" (Lit 1))
              (While (Neg (Equal (Var "x") (Lit 0)))
                (Seq (Ass "y" (Mul (Var "x") (Var "y")))
                  (Ass "x" (Sub (Var "x") (Lit 1))))))

data AritExp = 
     Lit Int |
     Var String |
     Add Int Int |
     Mul Int Int |
     Sub Int Int

type State = Store

aEval :: AritExp -> State -> Int
aEval (Lit 1) _     = 1
aEval (Var v) s     = 
aEval (Add e1 e2) s =