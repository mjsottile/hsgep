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

--
-- main
--
main :: IO ()
main = do
  cmdOpts <- handleCommandLine "HSGEP_Regression"
  
  -- read parameters
  p <- readParameters (optParams cmdOpts)
  let (rs,gnome,params) = case p of
                            Right r -> r
                            Left err -> error err
  
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
