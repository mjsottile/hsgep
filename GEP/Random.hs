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
     mutate
) where

import GEP.Types
import GEP.Params
import GEP.Rmonad

{-|
  Select a random symbol from the provided list.
-}
randomSymbol :: [a]        -- ^ List of symbols
             -> GEPMonad a -- ^ Selected symbol
randomSymbol syms =
  do index <- nextR (length syms)
     return (syms !! (index-1))

{-|
  Select a sequence of random symbols from the provided list.
-}
randomSymbolList :: [a]          -- ^ List of symbols
                 -> Int               -- ^ Number to select
                 -> GEPMonad [a] -- ^ List of selected 
                                           --   symbols
randomSymbolList _    0 = do return []
randomSymbolList syms n =
  do current <- randomSymbol syms
     rest <- randomSymbolList syms (n-1)
     return ([current]++rest)

-- | Generate a new individual given a genome specification.
newIndividual :: Genome              -- ^ Genome for individual
              -> Int                 -- ^ Number of genes to generate
              -> GEPMonad Chromosome
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
newPopulation :: Genome   -- ^ Genome of population
              -> Int      -- ^ Number of individuals to create
              -> GEPMonad [Chromosome]
newPopulation _ 0 = do return []
newPopulation g n =
  do p <- newPopulation g (n-1)
     i <- newIndividual g (numGenes g)
     return ([i]++p)

-- | Mutate symbols in a gene. Symbols are chosen from terminals and allsymbols
--   for head and tail of the gene respectively.
mutateGene :: Genome -> Rates -> Gene -> GEPMonad Gene
mutateGene g r gene = do
    let (h, t) = splitAt (headLength g) gene
    hMutated <- mapM mutateHeadSymbol h
    tMutated <- mapM mutateTailSymbol t
    return $ hMutated ++ tMutated
    where
        mutateTailSymbol :: Symbol -> GEPMonad Symbol
        mutateTailSymbol s = mutateSymbol r s $ terminals g

        mutateHeadSymbol :: Symbol -> GEPMonad Symbol
        mutateHeadSymbol s = mutateSymbol r s $ allsymbols g

-- | Mutate single symbol with probability pMutate choosing from given symbol
--   list.
mutateSymbol :: Rates -> Symbol -> [Symbol] -> GEPMonad Symbol
mutateSymbol r s ss =
    nextF 1.0 >>= \prob ->
    if prob < pMutate r
    then randomSymbol ss
    else return s


mutate :: Genome -> Rates -> Chromosome -> GEPMonad Chromosome
mutate g r s =
  do
    genes' <- mapM (\i -> mutateGene g r i) genes
    return $ genesToChrom genes'
  where
    genes = chromToGenes s (geneLength g)
    
