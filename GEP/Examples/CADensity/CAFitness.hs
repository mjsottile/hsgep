module GEP.Examples.CADensity.CAFitness(
  CA1D(..),
  nsteps,
  agreement
) where

import qualified Data.Vector as U
import Debug.Trace

data CA1D = CA1D (U.Vector Bool)
  deriving Show

idx :: Int -> Int -> Int
idx i w = i `mod` w

type CA7Rule = (Bool,Bool,Bool,Bool,Bool,Bool,Bool) -> Bool

onestep :: CA7Rule -> CA1D -> CA1D
onestep rule (CA1D state) = CA1D $ U.imap (\i _ -> site i) state
  where
    w = U.length state
    site i = rule (l3,l2,l1,c,r1,r2,r3)
      where l1 = state U.! (idx (i-1) w)
            l2 = state U.! (idx (i-2) w)
            l3 = state U.! (idx (i-3) w)
            c  = state U.! (idx i w)
            r1 = state U.! (idx (i+1) w)
            r2 = state U.! (idx (i+2) w)
            r3 = state U.! (idx (i+3) w)

nsteps :: CA7Rule -> CA1D -> Int -> CA1D
nsteps rule state n = last $ take n (iterate (onestep rule) state)

agreement :: CA1D -> Maybe Bool
agreement (CA1D state) =
  if count==0 then Just False
              else if count==w then Just True
                               else Nothing
  where 
    w = U.length state
    count = U.foldl (\c s -> if s then c+1 else c) 0 state


