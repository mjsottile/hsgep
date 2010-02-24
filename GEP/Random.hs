{- |
    Randomized functions for GEP applications.  Attempting to
    isolate all code that needs to be run under the Rmonad here.
   
    Author: mjsottile\@computer.org
-}
module GEP.Random (
     randomSymbol,
     randomSymbolList,
     newIndividual,
     newPopulation,
     mutateSymbol,
     mutate
) where

import GEP.Types
import GEP.Params
import System.Random hiding (next)
import GEP.Rmonad

{-|
  Select a random symbol from the provided list.
-}
randomSymbol :: (RandomGen s) => [Symbol]        -- ^ List of symbols
                              -> Rmonad s Symbol -- ^ Selected symbol
randomSymbol syms =
  do index <- nextR (length syms)
     return (syms !! (index-1))

{-|
  Select a sequence of random symbols from the provided list.
-}
randomSymbolList :: (RandomGen s) => [Symbol]          -- ^ List of symbols
                                  -> Int               -- ^ Number to select
                                  -> Rmonad s [Symbol] -- ^ List of selected 
                                                       --   symbols
randomSymbolList _    0 = do return []
randomSymbolList syms n =
  do current <- randomSymbol syms
     rest <- randomSymbolList syms (n-1)
     return ([current]++rest)

-- | Generate a new individual given a genome specification.
newIndividual :: (RandomGen s) 
              => Genome              -- ^ Genome for individual
              -> Int                 -- ^ Number of genes to generate
              -> Rmonad s Individual
newIndividual _ 0 = do return []
newIndividual g n =
  do hI <- randomSymbolList (allsymbols g) head_len
     tI <- randomSymbolList (terminals g) tail_len
     otherGenes <- newIndividual g (n-1)
     return (hI++tI++otherGenes)
  where
     head_len = headLength g
     tail_len = tailLength g

-- |Create a population of fresh random individuals given a genome
-- |specification.
newPopulation :: (RandomGen s) 
              => Genome   -- ^ Genome of population
              -> Int      -- ^ Number of individuals to create
              -> Rmonad s [Individual]
newPopulation _ 0 = do return []
newPopulation g n =
  do p <- newPopulation g (n-1)
     i <- newIndividual g (numGenes g)
     return ([i]++p)

mutateSymbol :: (RandomGen s) => Genome -> Rates -> Symbol -> Float -> Bool -> Rmonad s Symbol
mutateSymbol g r _ p True | (p < (pMutate r)) = 
  do s <- randomSymbol (allsymbols g)
     return s

mutateSymbol g r _ p False | (p < (pMutate r)) =
  do s <- randomSymbol (terminals g)
     return s

mutateSymbol _ _ s _ _ | otherwise = 
  do return s 

mutateGene :: (RandomGen s) => Genome -> Rates -> [Symbol] -> Rmonad s [Symbol]
mutateGene_ _ [] = do return []
mutateGene g r (s:ss) =
  do prob <- nextF 1.0
     news <- mutateSymbol g r s prob ((length ss) >= (tailLength g))
     newss <- mutate g r ss
     return ([news]++newss)

mutate :: (RandomGen s) => Genome -> Rates -> [Symbol] -> Rmonad s [Symbol]
mutate g r s =
  do
    genes' <- mapM (\i -> mutateGene g r i) genes
    return $ genesToChrom genes'
  where
    genes = chromToGenes s (geneLength g)
    
