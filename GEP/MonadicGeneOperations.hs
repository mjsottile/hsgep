{-|
   This module contains wrappers around the purely functional gene operations
   in "GEP.GeneOperations" in order to string the random number generation
   state through via the "GEP.Rmonad".  These helper functions are responsible
   for sampling the random number generator to determine the parameters for
   applying the genetic operators.

   The reasoning behind using a specialized Random monad instead of the
   system generator provided by IO is that this allows independent
   generators to be used should we support multiple threads of execution.
   Parallel random number generation requires distinct generators, not a
   shared one.

   Author: mjsottile\@computer.org
-}
module GEP.MonadicGeneOperations where

import System.Random
import GEP.Rmonad
import GEP.GeneOperations
import GEP.Types
import GEP.Params

{-|
   IS Transposition helper
-}
isTransposer :: (RandomGen s) => Genome ->
                                 SimParams ->
                                 Individual ->
                                 Rmonad s [Symbol]
isTransposer genome params who =
  do takelen   <- nextR (maxISLen params)
     takepos   <- nextR ((geneLength genome)-takelen)
     whichgene <- nextR (numGenes genome)
     putpos    <- nextR ((headLength genome)-1)
     return $ transposeIS who genome (whichgene-1) takepos takelen (putpos+1)

{-|
   RIS Transposition helper
-}
risTransposer :: (RandomGen s) => Genome -> 
                                  SimParams ->
                                  Individual ->
                                  Rmonad s [Symbol]
risTransposer genome params who =
  do takelen <- nextR (maxRISLen params)
     takepos <- nextR ((headLength genome)-1)
     genenum <- nextR (numGenes genome)
     return $ transposeRIS who genome genenum (takepos+1) takelen

{-|
   Gene transposition helper
-}
geneTransposer :: (RandomGen s) => Genome ->
                                   Individual ->
                                   Rmonad s [Symbol]
geneTransposer genome who =
  do whichGene <- nextR (numGenes genome)
     return $ transposeGene who genome whichGene

{-|
  One-point crossover helper.  Takes a genome, a pair of individuals,
  and selects the crossover point before generating the new pair of
  resulting individuals after crossover.
-}
x1PHelper :: (RandomGen s) => Genome ->
                              (Individual,Individual) ->
                              Rmonad s (Individual,Individual)
x1PHelper g pair =
  do xoverPos <- nextR (geneLength g)
     return $ crossover1pt pair xoverPos

{-|
  Two-point crossover helper.  Takes a genome, a pair of individuals,
  and selects the crossover points before generating the new pair of
  resulting individuals after crossover.
-}
x2PHelper :: (RandomGen s) => Genome ->
                              (Individual,Individual) ->
                              Rmonad s (Individual,Individual)
x2PHelper g pair =
  do xoverPos1 <- nextR (geneLength g)
     xoverPos2 <- nextRDifferent (geneLength g) xoverPos1
     return $ crossover2pt pair (min xoverPos1 xoverPos2)
                                (max xoverPos1 xoverPos2)
{-|
  Gene crossover helper.  Takes a genome, a pair of individuals, and
  selects the crossover gene before generating the new pair of
  individuals resulting after crossover.
-}
xGHelper :: (RandomGen s) => Genome ->
                             (Individual, Individual) ->
                             Rmonad s (Individual,Individual)
xGHelper g pair | (numGenes g) == 1 = return pair
xGHelper g pair | otherwise         = do
  xoverGene <- nextR (numGenes g)
  return $ crossoverGene pair xoverGene (geneLength g)