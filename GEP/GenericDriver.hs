module GEP.GenericDriver (
    Options(..)
  , handleCommandLine
  , gepDriver
) where

import System.Environment (getArgs)
import System.Console.GetOpt

import System.Random.Mersenne.Pure64
import GEP.TimeStep
import GEP.Rmonad
import GEP.Random
import GEP.Types
import GEP.Params
import GEP.Fitness

--
-- CMDLINE ARGS
--

data Flag = Params String
          | Fitness String
          | Dotfile String
          | Verbose
  deriving (Show, Eq)

--
-- command line options
--
data Options = Options {
  optParams  :: String,
  optFitness :: String,
  optDotfile :: Maybe String,
  optVerbose :: Bool
} 

options :: [OptDescr Flag]
options = [
  Option ['v'] ["verbose"]         (NoArg Verbose)           "Enable verbose output",
  Option ['d'] ["dot"]             (ReqArg Dotfile "FILE")   "Graphviz dotfile",
  Option ['i'] ["params"]          (ReqArg Params "FILE")    "Parameter file",
  Option ['f'] ["fitness"]         (ReqArg Fitness "FILE")   "Fitness data"
  ]

handleCommandLine :: String -> IO Options
handleCommandLine s = do
  flags <- commandLineToFlags s
  return Options { optParams = getParams flags,
                   optFitness = getFitness flags,
                   optDotfile = getDotfile flags,
                   optVerbose = isVerbose flags }

header :: String -> String
header s = "Usage: "++s++" [OPTION...]"

commandLineToFlags :: String -> IO [Flag]
commandLineToFlags s = do
  args <- getArgs
  let parsedArgs = getOpt RequireOrder options args
      (flags, _, _) = parsedArgs
  case parsedArgs of
    (_ , [],      [])   -> return flags
    (_ , nonOpts, [])   -> error $ "unrecognized arguments: " ++ unwords nonOpts
    (_ , _ ,      msgs) -> error $ concat msgs ++ usageInfo (header s) options

getDotfile :: [Flag] -> Maybe String
getDotfile []                   = Nothing
getDotfile ((Dotfile fname):_)  = Just fname
getDotfile (_:rest)             = getDotfile rest

getParams :: [Flag] -> String
getParams []                   = error "Parameter file required"
getParams ((Params fname):_)   = fname
getParams (_:rest)             = getParams rest

getFitness :: [Flag] -> String
getFitness []                   = error "Fitness data required"
getFitness ((Fitness fname):_)  = fname
getFitness (_:rest)             = getFitness rest

isVerbose :: [Flag] -> Bool
isVerbose []          = False
isVerbose (Verbose:_) = True
isVerbose (_:rest)    = isVerbose rest

--
-- DRIVERS
--

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
