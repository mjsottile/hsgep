-- |
-- GEP parameters.  These are related to both population management,
-- selection, and rates of genetic operators.  The rates are a set of
-- probabilities of each operator being applied during each step of the
-- selection and reproduction phase.
-- 
-- Author: mjsottile\@computer.org
-- 

module GEP.Params (
  Rates(..),
  SimParams(..)
) where

-- | The SimParams structure reprents the parameters for a run of the GEP
--   algorithm.  This includes gross parameters unrelated to individuals
--   such as the population size, parameters related to selection, and
--   parameters related to specific genetic operators.
data SimParams = SimParams {
      popSize :: Int,             -- ^ Population size
      rouletteExponent :: Double,  -- ^ Exponent for defining the roulette
                                  --   wheel bin sizes
      maxFitness :: Double,        -- ^ Fitness of the ideal individual
      numGenerations :: Int,      -- ^ Number of generations to run the
                                  --   algorithm for
      selectionRange :: Double,    -- ^ Parameter m for fitness value
                                  --   computation from the GEP paper.
      maxISLen :: Int,            -- ^ Maximum length of an IS transpose seq.
      maxRISLen :: Int            -- ^ Maximum length of an RIS transpose seq.
} deriving Show

-- | The Rates structure is used to hold the probability of various events
--   occurring during the evolution of the GEP algorithm.  
data Rates = Rates {
      pMutate :: Double, -- ^ Probability of any single symbol being mutated
                        --   per individual
      pIS :: Double,     -- ^ Probability of an individual experiencing
                        --   insertion sequence transposition
      pRIS :: Double,    -- ^ Probability of an individual experiencing
                        --   root insertion sequence transposition
      pGT :: Double,     -- ^ Probability of an individual experiencing
                        --   gene transposition
      p1R :: Double,     -- ^ Probability of a 1pt recombination event
      p2R :: Double,     -- ^ Probability of a 2pt recombination event
      pGR :: Double      -- ^ Probability of a gene recombination event
} deriving Show
