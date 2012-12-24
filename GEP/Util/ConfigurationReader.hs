-- |
-- Code to read configuration files.
--
-- Author: mjsottile\@computer.org
--

module GEP.Util.ConfigurationReader (
  readParameters
) where

import GEP.Params
import GEP.Types
import System.IO
import Data.Maybe

--
-- given a list of pairs mapping keys to values, lookup the various
-- parameters and populate the rates, genome, and simparams structures
--
extractParameters :: [(String,String)] -> (Rates,Genome,SimParams)
extractParameters config = (r,g,s)
    where
      s = SimParams { 
	    popSize          = fromJust (lookupInt "populationSize" config),
	    selectionRange   = fromJust (lookupDouble "selectionRange" config),
	    maxFitness       = fromJust (lookupDouble "maxFitness" config),
	    numGenerations   = fromJust (lookupInt "numGenerations" config),
	    maxISLen         = fromJust (lookupInt "maxISLen" config),
	    maxRISLen        = fromJust (lookupInt "maxRISLen" config),
	    rouletteExponent = fromJust (lookupDouble "rouletteExponent" config) 
	  }
      r = Rates { pMutate = fromJust (lookupDouble "rateMutate" config),
	          p1R     = fromJust (lookupDouble "rate1R" config),
	          p2R     = fromJust (lookupDouble "rate2R" config),
	          pGR     = fromJust (lookupDouble "rateGR" config),
	          pIS     = fromJust (lookupDouble "rateIS" config),
	          pRIS    = fromJust (lookupDouble "rateRIS" config),
	          pGT     = fromJust (lookupDouble "rateGT" config) 
	        }
      g = Genome { 
	    terminals     = fromJust (lookupString "genomeTerminals" config),
	    nonterminals  = fromJust (lookupString "genomeNonterminals" config),
	    geneConnector = fromJust (lookupChar "genomeGeneConnector" config),
	    maxArity      = fromJust (lookupInt "genomeMaxArity" config),
	    numGenes      = fromJust (lookupInt "genomeNumGenes" config),
	    headLength    = fromJust (lookupInt "genomeHeadLength" config)
	  }

--
-- function visible to the outside world.  passes in a string representing
-- the filename of the configuration, and passes back the rates,
-- genome, and simparams structures.  Expected to be called from within the
-- IO monad
--
readParameters :: String -> IO (Rates,Genome,SimParams)
readParameters filename = 
	do config <- readConfiguration filename
	   return $ extractParameters config

--
-- lookup helpers: float, int, char, and string versions
--

lookupDouble :: String -> [(String,String)] -> Maybe Double
lookupDouble k dict = 
	case (lookup k dict) of
		Nothing -> Nothing
		Just s  -> Just (read s)

lookupInt :: String -> [(String,String)] -> Maybe Int
lookupInt k dict =
	case (lookup k dict) of
		Nothing -> Nothing
		Just s  -> Just (read s)

lookupString :: String -> [(String,String)] -> Maybe String
lookupString k dict = lookup k dict

lookupChar :: String -> [(String,String)] -> Maybe Char
lookupChar k dict = 
	case (lookup k dict) of
		Nothing -> Nothing
		Just s  -> Just (head s)

--
-- given a string, remove whitespace
--
removeWhitespace :: String -> String
removeWhitespace s = filter (\x -> x /= ' ' && x /= '\t') s

--
-- split a line formatted as "KEY=VALUE", removing whitespace
--
splitLine :: String -> (String,String)
splitLine l = (front,back)
  where
    cleaned = removeWhitespace l
    front   = takeWhile (\i -> i /= '=') cleaned
    back    = drop (1 + length front) cleaned
--
-- read a file handle and return all of the lines in the file
--
fileToLines :: Handle -> IO [String]
fileToLines h = do eof <- hIsEOF h
                   (if eof
                    then return []
                    else do line <- hGetLine h
                            remainder <- fileToLines h
                            return $ (line:remainder))

--
-- given a filename, open the file, read the lines, and then split them
-- into key/value pairs assuming a "KEY=VALUE" format per line
--
readConfiguration :: String -> IO [(String,String)]
readConfiguration filename = do handle <- openFile filename ReadMode
                                fileLines <- fileToLines handle
                                return $ map splitLine fileLines
