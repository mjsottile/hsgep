-- | This module contains code related to fitness evaluation.  The
--   main purpose of the code is to both evaluate fitnesses of individuals
--   and to sort individuals by fitness.  These are intended to all be
--   higher order functions that assume nothing about the purpose of the
--   individuals or the types of inputs being used for fitness testing.
--   The only assumption made currently is that the outputs for test cases
--   are floating point numbers.  That likely should change for general
--   purpose usage.
--
--   mjsottile\@computer.org
--
module GEP.Fitness (
  fitness_tester,
  fitness_filter,
  sortByFitness
) where

import GEP.Types

--
-- Sort a list of pairs by first element of each pair.  Disregard duplicates
-- pairs.
--
pairSort :: (Ord a) => [(a,b)] -> [(a,b)]
pairSort []           = []
pairSort ((f,i):rest) =
    lhs++((f,i):rhs)
    where
      lhs = [(ff,ii) | (ff,ii) <- rest, ff <  f]
      rhs = [(ff,ii) | (ff,ii) <- rest, ff >= f]

-- |
--  Fitness evaluator for generic individuals.  This needs to go away
--  and use a more general approach like evaluateFitness above.
-- 
fitness_tester :: a               -- ^ Expressed individual
               -> (a -> b -> Double -> Double -> Double) -- ^ Fitness function
               -> [b]             -- ^ List of symbol tables for test cases
               -> [Double]         -- ^ List of expected outputs for test cases
               -> Double           -- ^ Range of selection.  M in original
                                  --   GEP paper equations for fitness.
               -> Double           -- ^ Fitness value for given individual
fitness_tester who ffun inputDict outputs m = 
  foldr (+) 0.0 tests
  where 
    tests = map (\(x,y) -> ffun who x y m) 
                (zip inputDict outputs)

-- |
--  Given a list of fitness values and a corresponding list of individuals,
--  return a list of tuples pairing the fitness value with the individuals for
--  only those individuals that have a valid fitness value.  This means those
--  that are +/- infinity or NaN are removed.
--
fitness_filter :: [Double]              -- ^ Fitness values
               -> [Individual]         -- ^ Individuals
               -> [(Double,Individual)] -- ^ Paired fitness/individuals after 
                                       --   filtering
fitness_filter fitnesses pop =
    foldr (\(i,j) -> 
           \x -> if ((isNaN i) || (isInfinite i)) 
                 then x 
                 else ((i,j):x)
          ) [] (zip fitnesses pop)

-- |
--  Sort a set of individuals with fitness values by their fitness
--
sortByFitness :: [(Double,Individual)] -> [(Double,Individual)]
sortByFitness xs = reverse (pairSort xs)
