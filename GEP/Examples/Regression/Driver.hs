{-# LANGUAGE DeriveDataTypeable #-}

-- |
--  Haskell gene expression programming, regression example
-- 
--  Author: mjsottile\@computer.org
--
module Main (
    main
) where

import GEP.GenericDriver
import GEP.Util.ConfigurationReader
import GEP.Examples.Regression.ArithmeticIndividual
import GEP.Examples.Regression.FitnessInput
-- NOTE: you can use the files in the maxima subdirectory next to this
--       example to integrate the maxima client/server for pretty printing
--       polynomials
--
-- import GEP.Examples.Regression.MaximaClient
import System (getArgs)
import System.Console.GetOpt

--
-- command line options
--
data Options = Options {
  optParams :: String,
  optFitness :: String,
  optDotfile :: Maybe String,
  optVerbose :: Bool
} 

options :: [OptDescr (Options -> IO Options)]
options = 
  [ Option ['i'] ["params"]  (ReqArg inputFile "FILE")   "Parameters"
  , Option ['f'] ["fitness"] (ReqArg fitnessFile "FILE") "Fitness tests"
  , Option ['d'] ["dot"]     (OptArg dotFile "FILE")     "Graphviz dotfile"
  , Option ['v'] ["verbose"] (NoArg  verbose)            "Verbose output"
  ]

checkOptions :: Options -> IO ()
checkOptions opts =
  case (optParams opts) of
    "" -> error ("Parameter file required.")
    _ -> case (optFitness opts) of
           "" -> error ("Fitness file required.")
           _  -> return ()

programOpts :: [String] -> IO Options
programOpts argv = do
  case getOpt Permute options argv of
    (actions, [], []) -> do opts <- foldl (>>=) (return defaultOptions) actions
                            checkOptions opts
                            return opts
    (_, nonOpts, []) -> error ( "unrecognized arguments: " ++ unwords nonOpts)
    (_, _, msgs) -> error (concat msgs ++ usageInfo header options)
  where header = "Usage: regression [OPTION...]"

inputFile :: String -> Options -> IO Options
inputFile arg opt = return opt { optParams = arg }

fitnessFile :: String -> Options -> IO Options
fitnessFile arg opt = return opt { optFitness = arg }

dotFile :: Maybe String -> Options -> IO Options
dotFile arg opt = return opt { optDotfile = arg }

verbose :: Options -> IO Options
verbose opt = return opt { optVerbose = True }

defaultOptions :: Options
defaultOptions = Options { 
  optParams = "", 
  optFitness = "", 
  optDotfile = Nothing,
  optVerbose = False
}

--
-- main
--
main :: IO ()
main = do
  args <- getArgs
  cmdOpts <- programOpts args
  
  -- read parameters
  (rs,gnome,params) <- readParameters (optParams cmdOpts)
  
  -- read fitness test data
  (testDict, ys) <- readFitnessInput (optFitness cmdOpts)

  -- call generic driver
  (best,pop) <- gepDriver params rs gnome testDict ys fitness_evaluate_absolute express_individual

  -- Express best individual
  let bestExpressed = express_individual (head pop) gnome
  
  -- Flatten best individual via infix walk
  let bestString = infixWalker bestExpressed

  -- report status
  putStrLn "-------------------------------------------------"
  putStrLn $ "DONE  : "++(show best)
  putStrLn $ "INFIX : "++bestString 

  -- send flattened individual to maxima for pretty printing
  -- and print lines that come back
  -- UNCOMMENT THE FOLLOWING TO USE MAXIMA

  -- putStrLn $ "MAXIMA OUTPUT :"
  -- maximaExpand bestString "qubu.net" 12777 >>= mapM_ putStrLn

  -- dump to dot file if one was specified
  case (optDotfile cmdOpts) of
    Nothing -> return ()
    Just s  -> dumpDotFile s bestExpressed
