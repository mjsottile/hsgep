-- |
-- Code to read configuration files.
--
-- Authors: mjsottile\@computer.org
--	  : andreawscotti@gmail.com	
--

module GEP.Util.ConfigurationReader (
  readParameters
) where

import GEP.Params
import GEP.Types
--import System.IO
import Data.Maybe

-- function visible to the outside world.  passes in a string representing
-- the filepath to the configuration file, and passes back the rates,
-- genome, and simparams structures.  Expected to be called from within the
-- IO monad.

-- The result is wrapped in a Either together with a String. If result
-- is Left x, then x is a human readable error message.

readParameters :: String -> IO (Either String (Rates,Genome,SimParams) )
readParameters path = do
  s <- readFile path
  let ls = linesWithFilter s
  case (checkConfig ls) of
    Left er -> return (Left er)			
    Right _ -> return $ Right $ extractParameters $ readStringsIntoDict ls

-- creates a dict out of a String list.
readStringsIntoDict :: [String] -> [(String,String)]
readStringsIntoDict s = makeDict $ (map clean s)

extractParameters :: [(String,String)] -> (Rates,Genome,SimParams)
extractParameters config = (r,g,s)
  where
    s = SimParams { 
      popSize          = fromJust (lookupInt    "populationSize"   config),
      selectionRange   = fromJust (lookupDouble "selectionRange"   config),
      maxFitness       = fromJust (lookupDouble "maxFitness"       config),
      numGenerations   = fromJust (lookupInt    "numGenerations"   config),
      maxISLen         = fromJust (lookupInt    "maxISLen"         config),
      maxRISLen        = fromJust (lookupInt    "maxRISLen"        config),
      rouletteExponent = fromJust (lookupDouble "rouletteExponent" config) 
    }

    r = Rates     {           
	             pMutate = fromJust (lookupDouble "rateMutate" config),
	             p1R     = fromJust (lookupDouble "rate1R"     config),
	             p2R     = fromJust (lookupDouble "rate2R"     config),
	             pGR     = fromJust (lookupDouble "rateGR"     config),
	             pIS     = fromJust (lookupDouble "rateIS"     config),
	             pRIS    = fromJust (lookupDouble "rateRIS"    config),
	             pGT     = fromJust (lookupDouble "rateGT"     config) 
	  }

    g = Genome    { 
	       terminals     = fromJust (lookupString "genomeTerminals"     config),
	       nonterminals  = fromJust (lookupString "genomeNonterminals"  config),
	       geneConnector = fromJust (lookupChar   "genomeGeneConnector" config),
	       maxArity      = fromJust (lookupInt    "genomeMaxArity"      config),
	       numGenes      = fromJust (lookupInt    "genomeNumGenes"      config),
	       headLength    = fromJust (lookupInt    "genomeHeadLength"    config)
	  }

-- split a string into lines, with lines that are not useful removed.
-- these include empty lines and comment lines that start with a # symbol
linesWithFilter :: String -> [String]
linesWithFilter s =
  let gzlen l = length l > 0
      notcomment l = head l /= '#'
  in filter (\l -> (gzlen l) && (notcomment l)) (lines s)

-- checks Config File is sound. Reports first error on failures.
-- note: removed filter that rejected lines with more than one "=" char.
--       this would have rejected allowing '=' as a valid character in
--       a chromosome, which we don't want to do.
checkConfig :: [String] -> Either String Bool
checkConfig s = 
  multicheck s [isNotEmpty, 
                eitherAnd.(map checkMinLineLength)]

-- makes a dict out of a list of strings
makeDict :: [String] -> [(String,String)]
makeDict s =  map mySplit (filter (\l -> length l > 0) s)

-- splits a line based on '=' char
mySplit :: String -> (String,String)
mySplit s = (a,b) where
	a = takeWhile (/= '=') s
	b = drop (length a + 1) s

-- checks a type against many predicates. The latter are a List of
-- Either Bool String, so you get Right True or a Left String with an
-- error message. It stops at first failed check.
multicheck :: a -> [a -> Either String Bool] -> Either String Bool
multicheck _ []     = Right True
multicheck x (f:fs) = case (f x) of
		Right _ -> multicheck x fs
		Left s -> Left s

-- checks a list is not empty. 
isNotEmpty :: (Show a) => [a] -> Either String Bool
isNotEmpty l = if null l then Left "List is empty on imput." 
                         else Right True

-- checks a String is long enough to carry a key value pair.
checkMinLineLength :: String -> Either String Bool
checkMinLineLength s = 
  if (length s) > 2 then Right True 
                    else Left $ "Line too short to represent a key value pair on input:\n\n" ++ s ++ "\n"

-- this is like the 'and' function on lists. It just takes a list of Either Bool String; it returns a Right String, which is extracted from the very first Right String it encounters, otherwise it returns Left True.
eitherAnd::[Either a Bool]->Either a Bool
eitherAnd [] = Right True
eitherAnd (x:xs) = case x of
	Right _ -> eitherAnd xs
	Left a -> Left a

-- remove every occurrence of ANY Char in the first String, from the second one.	
remove :: String -> String -> String
remove _ "" = ""
remove xs (y:ys) = if elem y xs then remove xs ys else y : remove xs ys

-- removes whitespaces and tabs, curried version of remove.
clean :: String -> String
clean = remove (" " ++ "\t")

-- count things in lists.
count :: (Eq a) => a -> [a] -> Int
count _ [] = 0
count x (y:ys) = if x==y then 1 + (count x ys) else count x ys

-- generic lookup fuction, implemented with monadic code.
lookUpThing:: (Read a) => (String-> Maybe a) -> String -> [(String,String)] 
                       -> Maybe a
lookUpThing f k dict = (lookup k dict) >>= f

-- curried versions of LookUpThing, to serve as utils.
lookupInt :: String -> [(String, String)] -> Maybe Int
lookupInt = lookUpThing (\x -> Just(read x :: Int))

lookupDouble :: String -> [(String, String)] -> Maybe Double
lookupDouble = lookUpThing (\x -> Just(read x :: Double))

lookupChar :: String -> [(String, String)] -> Maybe Char
lookupChar = lookUpThing (\x->Just(head x))

lookupString ::  String -> [(String, String)] -> Maybe String
lookupString = lookUpThing (\x->Just(id x))
