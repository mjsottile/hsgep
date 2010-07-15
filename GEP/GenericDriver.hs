module GEP.GenericDriver where

import System.Random.Mersenne.Pure64
import GEP.TimeStep
import GEP.Rmonad
import GEP.Random
import GEP.Types
import GEP.Params
import GEP.Fitness

{-|
  Generic driver to be called from specific GEP program instances in their
  main routine.
-}
gepDriver :: SimParams  -- ^ Simulation parameters
          -> Rates      -- ^ Rates for genetic operators
          -> Genome     -- ^ Genome that individuals are drawn from
          -> TestDict b -- ^ Test dictionary for fitness testing
          -> TestOuts   -- ^ Expected test results for test dictionary
          -> FitnessFunction a b -- ^ Fitness testing function
          -> ExpressionFunction a        -- ^ String to ET expression function
          -> IO (Double, [Chromosome])         -- ^ Return best individual fitness and population
gepDriver params rs gnome testdict testouts fitness_evaluate expression_function = do
  -- create initial population
  (initialPopulation,rngState) <- return $ runRmonad 
                                           (newPopulation gnome 
                                                          (popSize params))
                                           (pureMT 1)

  -- Step 3: run the multistep iterator to evolve the population.  this
  --         is the core of the GEP process.  Pass same rngState returned
  --         when creating an initial population above when going back into
  --         the Rmonad
  ((best,pop),_) <- return $ runRmonad 
                                        (multiStep 
                                                initialPopulation 
                                                gnome 
                                                params
                                                rs
                                                expression_function
                                                fitness_evaluate 
                                                testdict 
                                                testouts 
                                                (numGenerations params)
                                                (maxFitness params) ) 
                                        rngState

  return (best,pop)
