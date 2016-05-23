module Soal3.CodeTable ( codeTable ) 
where
  -- Tree -> Table

import Soal3.Types

convert :: HCode -> Tree -> Table
convert cd (Leaf c n) = [(c,cd)]
convert cd (Node n t1 t2)
  = (convert (cd++[L]) t1) ++ (convert (cd++[R]) t2)

codeTable :: Tree -> Table
codeTable = convert[]