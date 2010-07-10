module GEP.GenericDriver where

import System.Random.Mersenne.Pure64
import GEP.TimeStep
import GEP.Rmonad
import GEP.Random
import GEP.Types
import GEP.Params

-- | Fitness function type
type FitnessFunction a b = a -> b -> Double -> Double -> Double

-- | Function to express an individual into a list of ET structures
type ExpressionFunction a = Individual -> Genome -> a

-- | A test case maps a list of terminals to float values
type TestCase a = SymTable a
    
-- | A test dictionary is a set of test cases
type TestDict a = [TestCase a]

-- | The set of outputs expected for each entry in the test dictionary
type TestOuts = [Double]

{-|
  Generic driver to be called from specific GEP program instances in their
  main routine.
-}
gepDriver :: SimParams  -- ^ Simulation parameters
          -> Rates      -- ^ Rates for genetic operators
          -> Genome     -- ^ Genome that individuals are drawn from
          -> TestDict b -- ^ Test dictionary for fitness testing
          -> TestOuts   -- ^ Expected test results for test dictionary
          -> FitnessFunction a (TestCase b) -- ^ Fitness testing function
          -> ExpressionFunction a        -- ^ String to ET expression function
          -> IO (Double,[String])         -- ^ Return best individual fitness and population
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
