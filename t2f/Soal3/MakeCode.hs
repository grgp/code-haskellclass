-- Soal 3
-- George Albert
-- 1406569781
-- Pemdek C

module Soal3.MakeCode ( codes, codeTable ) where

import Soal3.Types
import Soal3.Frequency ( frequency )
import Soal3.MakeTree ( makeTree )
import Soal3.CodeTable ( codeTable )
import Soal2.PriorityQueue

codes :: [Char] -> Tree
codes = makeTree . frequency