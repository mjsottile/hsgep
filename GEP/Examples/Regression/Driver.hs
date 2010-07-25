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
import System.Environment (getArgs)
import Control.Monad (when)

--
-- sanity check arguments to see if we have enough
--
validateArgs :: [String] -> IO ()
validateArgs s =
    when (length s < 2) $
        error "Must specify config file and fitness test data file names."

--
-- main
--
main :: IO ()
main = do
  -- read in parameters from specified file
  args <- getArgs

  -- sanity check
  validateArgs args

  -- give args nice names
  let configFile = head args
  let fitnessFile = head (tail args)

  -- if optional third argument is present, assume it is dot file
  dotfile <- if length args == 3 then return $ Just $head (tail (tail args))
                                     else return $ Nothing
  
  -- read parameters
  (rs,gnome,params) <- readParameters configFile
  
  -- read fitness test data
  (testDict, ys) <- readFitnessInput fitnessFile

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
  dumpDotFile dotfile bestExpressed
