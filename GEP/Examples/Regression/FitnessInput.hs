{-|

  Code to read input data files containing the test inputs and test outputs
  used to evaluate the fitness of individuals.

  Author: mjsottile\@computer.org

  NOTE: Parsec code for CSV files 

-}
module GEP.Examples.Regression.FitnessInput (
  readFitnessInput
) where

import Text.ParserCombinators.Parsec
import System.Exit

--
-- assume files have CSV format with a header row where each entry in the
-- header row names a variable.  note that currently we require these to
-- be single characters.  eventually we may automate the process of mapping
-- variables onto characters in the genome to allow more expressive names
-- to be associated with variables.
--

--  PARSEC STUFF

csvfile = many csvline

csvline = do
  entries <- (sepBy entry (char ','))
  newline
  return entries

-- entry accepts any string containing alphanum or periods, with spaces either
-- before or after the value.
entry = do
  many (char ' ')
  body <- many (noneOf ",\n")
  many (char ' ')
  return body

--  END PARSEC STUFF

type FitnessDict = [[(Char,Float)]]

dictify :: [String] -> [[String]] -> (FitnessDict, [Float])
dictify lbls values =
    (map (\j -> zip (init charLbls) j) (init floatValues),
     map last floatValues)
    where
      charLbls = map head lbls
      floatValues = map (\j -> map (\i -> (read i) :: Float) j) values

-- function that takes a filename and returns a dictionary
readFitnessInput :: String -> IO (FitnessDict,[Float])
readFitnessInput fname = do
  result <- parseFromFile csvfile fname
  case result of Left err -> do putStrLn "Bad regression fitness input!"
                                exitFailure
                 Right xs -> do return $ dictify (head xs) (tail xs)

{-
main :: IO ()
main = do
  x <- readFitnessInput "test.csv"
  print x
-}
