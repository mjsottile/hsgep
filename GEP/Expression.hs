-- |
-- Expression related code.  These are helpers to turn linear sequences
-- into trees.
-- 
-- Author: mjsottile\@computer.org
-- 

module GEP.Expression (
	levelize
) where

import GEP.Types

levelize :: (Symbol -> Int) -> Sequence -> Int -> [Sequence]
levelize _     _  0 = []
levelize _     [] _ = []
levelize arity s  i =
    front : levelize arity back (sum $ map arity front)
    where
      (front,back) = splitAt i s