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
import System.IO
import Data.Maybe

-- function visible to the outside world.  passes in a string representing
-- the filepath to the configuration file, and passes back the rates,
-- genome, and simparams structures.  Expected to be called from within the
-- IO monad.

--The result is wrapped in a Either together with a String. If result is Right x, then x is a human readable error message.


readParameters :: String -> IO (Either (Rates,Genome,SimParams) String)
readParameters path = do
	s <- readFile path
	case (checkConfig s) of
		Right er -> return (Right er)			
		Left _ -> return $ Left $ extractParameters $ readStringIntoDict s

-- creates a dict out of a String.
readStringIntoDict::String->[(String->String)]
readStringIntoDict = makeDict.clean

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


-- checks Config File is sound. Reports first error on failures.
checkConfig::String->Either Bool String
checkConfig s = multicheck s [isNotEmpty,checkNewLines,eitherAnd.(map checkDelimiter).lines,eitherAnd.(map checkMinLineLength).lines]

-- makes a dict out of a long String
makeDict::String->[(String,String)]
makeDict s =  map mySplit $ lines s

-- splits a line based on '=' char
mySplit::String->(String,String)
mySplit s = (a,b) where
	a = takeWhile (/='=') s
	b = drop (length a +1) s

-- checks a type against many predicates. The latter are a List of Either Bool String, so you get Left True or a Right String with an error message. It stops at first failed check.
multicheck::a->[a->Either Bool String]->Either Bool String
multicheck x [] = Left True
multicheck x (f:fs) = case (f x) of
		Left _ -> multicheck x fs
		Right s -> Right s

-- checks a list is not empty. 
isNotEmpty::(Show a)=>[a]->Either Bool String
isNotEmpty l = if null l then Right "List is empty on imput." else Left True

-- checks there are no empty lines interspersed into a bigger String.
checkNewLines::String-> Either Bool String
checkNewLines s = if (not $ elem "" $ lines s) then Left True else Right $ "Failed checkNewLines on input:\n\n" ++ s ++ "\n"

-- checks there is exactly one '=' char in a String.
checkDelimiter::String->Either Bool String
checkDelimiter s = let c = count '=' s in 
		if c==1 then Left True else Right $ "Delimiter count is not 1, it is " ++ (show c) ++ " in input:\n\n" ++ s ++ "\n"

-- checks a String is long enough to carry a key value pair.
checkMinLineLength::String->Either Bool String
checkMinLineLength s = if (length s)>2 then Left True else Right "Line too short to represent a key value pair on input:\n\n" ++ s ++ "\n"

-- this is like the 'and' function on lists. It just takes a list of Either Bool String; it returns a Right String, which is extracted from the very first Right String it encounters, otherwise it returns Left True.
eitherAnd::[Either Bool a]->Either Bool a
eitherAnd [] = Left True
eitherAnd (x:xs) = case x of
	Left _ -> eitherAnd xs
	Right a -> Right a

-- remove every occurrence of ANY Char in the first String, from the second one.	
remove::String->String->String
remove _ "" = ""
remove xs (y:ys) = if elem y xs then remove xs ys else y : remove xs ys

-- removes whitespaces and tabs, curried version of remove.
clean = remove (" " ++ "\t")

-- count things in lists.
count::(Eq a)=>a->[a]->Int
count _ [] = 0
count x (y:ys) = if x==y then 1 + (count x ys) else count x ys

-- generic lookup fuction, implemented with monadic code.
lookUpThing:: (Read a) => (String-> Maybe a)->String -> [(String,String)] -> Maybe a
lookUpThing f k dict = (lookup k dict) >>= f

-- curried versions of LookUpThing, to serve as utils.
lookupInt = lookUpThing (\x -> Just(read x :: Int))
lookupDouble = lookUpThing (\x -> Just(read x :: Double))
lookupChar = lookUpThing (\x->Just(head x))
lookupString = lookUpThing (\x->Just(id x))
