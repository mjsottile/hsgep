-- |
-- Routines for selection after fitness evaluation.  Selection is the process
-- of taking some input population P, a set of fitness values such that
-- each p in P has a fitness score f(p,X) under some fitness test X, and
-- selecting which members of P participate in the creation of the next
-- population P'.
--
-- A common technique is roulette wheel selection.  In essence, this means that
-- we create a roulette wheel with one slot per individual where the width of
-- each slot is a function of the fitness of the individuals.  So, those
-- individuals with very good fitness will have wide slots and a correspondingly
-- high likelihood of selection, while poor fitness individuals will have tiny
-- slots and a low probability of being selected.
--
-- Fitness testing takes place outside this module.  This module is only
-- concerned with the selection process (ie: generating the roulette wheel).
--
-- Author: mjsottile\@computer.org
--
module GEP.Selection (
     generate_roulette_weights,
     roulette,
     selector,
     getBest
) where

import GEP.Types
import GEP.Rmonad
import List (sort)

{-|
  Given a set of pairs (f,i) where f is the fitness of the individual i,
  return the pair representing the individual with the best fitness.
  We may return nothing if an empty set is passed in to begin with, so
  the return type is a Maybe pair.
-}
getBest :: [(Double,Individual)]      -- ^ Fitness/Individual pairs
        -> Maybe (Double,Individual)  -- ^ Best pair, or Nothing if no such pair
getBest []          = Nothing
getBest individuals =
  let innerBest [] bi bf = Just (bf,bi)
      innerBest ((f,i):rest) bi bf = if f > bf 
                                     then 
                                         innerBest rest i f
                                     else 
                                         innerBest rest bi bf
      (firstB, firstI) = head individuals
  in
    innerBest (tail individuals) firstI firstB

weight_function :: Double -> Double -> Double
weight_function n e =
    1.0 / (n ** e)

{-|
  Given a list of indices and a list of data elements, create a new list
  of data elements composed of the elements listed in the index list.
  The output list may contain duplicates.
-}
selector :: [Int] -- ^ List of indices to select
         -> [a]   -- ^ List of elements 
         -> [a]   -- ^ List composed of elements selected from original set by indices provided
selector i x = reverse (innerSelect 0 (sort i) x [])

-- tail recursive version of inner select
innerSelect :: Int -> [Int] -> [a] -> [a] -> [a]
innerSelect _ [] _ l          = l
innerSelect _ _ [] l          = l
innerSelect n (i:is) (x:xs) l =
    if (i==n) 
    then innerSelect n is (x:xs) (x:l)
    else innerSelect (n+1) (i:is) xs l

{-|
  Generate n roulette weights with a generator exponent e.  A helper function
  weight_function is used to generate the actual weights.  For example,
  w = (k^e)^(-1) for k from 1 to n leads to a set of weights such that the
  size of the slots decreases exponentially as fitness decreases.  When e=1,
  this decrease is linear.  The list that is returned is the width of each slot
  such that the total of the weights adds to 1.0.
-}
generate_roulette_weights :: Double -> Double -> [Double]
generate_roulette_weights n e =
    map (\i -> i / sx) weights
    where
      weights = [weight_function x e | x <- [1..n]]
      sx = foldr (+) 0.0 weights 

{-|
  Given a set of roulette weights and a number of spins of the wheel, return
  a list of indices corresponding to the winning slot for each spin.  This
  is used to perform the actual selection after a set of roulette weights are
  generated.
-}
roulette :: [Double] -> Int -> GEPMonad [Int]
roulette _ 0       = do return []
roulette weights n =
  do val <- nextF 1.0
     rest <- roulette weights (n-1)
     return ([find_bin 0.0 0 val weights]++rest)
  where
    find_bin _   m _   []     = m
    find_bin tot m val (b:bs) =
        if (val > tot) && (val <= (tot+b)) then m
        else find_bin (tot+b) (m+1) val bs

