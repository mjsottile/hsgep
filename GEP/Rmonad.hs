-- |
--  monad based on state for passing random
--  number state around for GEP
-- 
--  Author: mjsottile\@computer.org
-- 

{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module GEP.Rmonad (
    nextF,
    nextR,
    nextRDifferent,
    nextRList,
    nextRListUnique,
    nextRListPairs,
    generatePairs,
    runRmonad,
    GEPMonad
) where

import System.Random.Mersenne.Pure64
import Control.Monad.State.Strict
import Debug.Trace

newtype Rmonad s a = S (State s a)
    deriving (Monad)

type GEPMonad a = Rmonad PureMT a

nextF :: Double -> Rmonad PureMT Double
nextF up = S $ do st <- get
                  let (x,st') = randomDouble st
                  put st'
                  return (x*up)

nextRList :: Int 
          -> Int 
          -> Rmonad PureMT [Int]
nextRList 0 _  = do return []
nextRList n up = do val <- nextR up
                    vals <- nextRList (n-1) up
                    return (val:vals)

nextR :: Int 
      -> Rmonad PureMT Int
nextR up = S $ do st <- get
                  let (x,st') = randomInt st
                  put st'
                  return (1 + ((abs x) `mod` up))

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

generatePairs :: Int -> Rmonad PureMT [(Int,Int)]
generatePairs 0 = do return []
generatePairs 1 = do return []
generatePairs n = do vals <- shuffle $! [1..n]
                     return $ (pairify vals)

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

nextRDifferent :: Int -> Int -> Rmonad PureMT Int
nextRDifferent up x = do x' <- nextR up
                         if x' == x
                            then do x'' <- nextRDifferent up x
                                    return x''
                            else return x'

runRmonad :: Rmonad PureMT a -> PureMT -> (a, PureMT)
runRmonad (S m) s = runState m s
