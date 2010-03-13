-- |
--  Monad based on state for passing random number state around for GEP.
--  The choice of Mersenne.Pure64 was for performance, and the pure version
--  will play nicely with threading.
-- 
--  Author: mjsottile\@computer.org
-- 

{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module GEP.Rmonad (
    GEPMonad,
    nextF,
    nextR,
    nextRDifferent,
    nextRList,
    nextRListUnique,
    nextRListPairs,
    generatePairs,
    runRmonad
) where

import System.Random.Mersenne.Pure64
import Control.Monad.State.Strict
import Debug.Trace

newtype Rmonad s a = S (State s a)
    deriving (Monad)

-- | The GEPMonad is just a specific instance of the State monad where the
--   state is just the PureMT PRNG state.
type GEPMonad a = Rmonad PureMT a

-- | Generate a random number as a Double between 0.0 and the given upper
--   bound.
nextF :: Double -- ^ Upper bound.
      -> Rmonad PureMT Double
nextF up = S $ do st <- get
                  let (x,st') = randomDouble st
                  put st'
                  return (x*up)

-- | Generate a random integer between 1 and the upper bound (inclusive).
nextR :: Int -- ^ Upper bound.
      -> Rmonad PureMT Int
nextR up = S $ do st <- get
                  let (x,st') = randomInt st
                  put st'
                  return (1 + ((abs x) `mod` up))

-- | Generate a list of random integers.
nextRList :: Int -- ^ Number of integers to generate
          -> Int -- ^ Upper bound for each integer.
          -> Rmonad PureMT [Int]
nextRList 0 _  = do return []
nextRList n up = do val <- nextR up
                    vals <- nextRList (n-1) up
                    return (val:vals)

removeNth :: [a] -> Int -> [a]
removeNth [] _ = []
removeNth (_:xs) 0 = (xs)
removeNth (x:xs) n = x:(removeNth xs (n-1))

shuffle :: [Int] -> Rmonad PureMT [Int]
shuffle [] = do return []
shuffle x = do val <- nextR $ (length x)
               rest <- shuffle $ (removeNth x (val-1))
               return ((x !! (val-1)):rest)

pairify :: [Int] -> [(Int,Int)]
pairify [] = []
pairify (_:[]) = []
pairify (x:y:xs) = ((x,y):(pairify xs))

-- | Document me!
generatePairs :: Int -> Rmonad PureMT [(Int,Int)]
generatePairs 0 = do return []
generatePairs 1 = do return []
generatePairs n = do vals <- shuffle $! [1..n]
                     return $ (pairify vals)

-- | Generate a list of n random integers such that each entry occurs at most
--   once.  Each number in the list must be unique.
nextRListUnique :: Int -> [Int] -> Int -> Rmonad PureMT [Int]
nextRListUnique 0 l _  = do return l
nextRListUnique n l up = do val <- nextR up
                            let t = foldr (||) False (map (\i -> i==val) l)
                            if t == True
                               then do ret <- nextRListUnique n l up
                                       return ret
                               else do ret <- nextRListUnique (n-1) (val:l) up
                                       return ret

nextRListPairs :: Int -> Int -> Rmonad PureMT [(Int,Int)]
nextRListPairs 0 _  = do return []
nextRListPairs n up = do val1 <- nextR up
                         val2 <- nextRDifferent up val1
                         rest <- nextRListPairs (n-1) up
                         return $ ((val1,val2):rest)

-- | Generate a random integer in the specified range that is NOT equal to
--   the integer provided.
nextRDifferent :: Int -- ^ Upper bound.
               -> Int -- ^ Integer to avoid.
               -> Rmonad PureMT Int
nextRDifferent up x = do x' <- nextR up
                         if x' == x
                            then do x'' <- nextRDifferent up x
                                    return x''
                            else return x'

-- | Run function for the Rmonad.
runRmonad :: Rmonad PureMT a -> PureMT -> (a, PureMT)
runRmonad (S m) s = runState m s
