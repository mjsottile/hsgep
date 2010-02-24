-- |
--  monad based on state for passing random
--  number state around for GEP
-- 
--  Author: mjsottile\@computer.org
-- 

{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module GEP.Rmonad (
    Rmonad,
    nextF,
    nextR,
    nextRDifferent,
    nextRList,
    nextRListUnique,
    nextRListPairs,
    generatePairs,
    runRmonad
) where

import System.Random hiding (next)
import Control.Monad.State

newtype Rmonad s a = S (State s a)
    deriving (Monad)

nextF :: (RandomGen s) => Float -> Rmonad s Float
nextF up = S $ do st <- get
                  let (x,st') = randomR (0.0,up) st
                  put st'
                  return x

nextRList :: (RandomGen s) 
          => Int 
          -> Int 
          -> Rmonad s [Int]
nextRList 0 _  = do return []
nextRList n up = do val <- nextR up
                    vals <- nextRList (n-1) up
                    return (val:vals)

nextR :: (RandomGen s) 
      => Int 
      -> Rmonad s Int
nextR up = S $ do st <- get
                  let (x,st') = randomR (1,up) st
                  put st'
                  return x

removeNth :: [a] -> Int -> [a]
removeNth [] _ = []
removeNth (_:xs) 0 = (xs)
removeNth (x:xs) n = x:(removeNth xs (n-1))

shuffle :: (RandomGen s) => [Int] -> Rmonad s [Int]
shuffle [] = do return []
shuffle x = do val <- nextR $ (length x)
               rest <- shuffle $ (removeNth x (val-1))
               return ((x !! (val-1)):rest)

pairify :: [Int] -> [(Int,Int)]
pairify [] = []
pairify (_:[]) = []
pairify (x:y:xs) = ((x,y):(pairify xs))

generatePairs :: (RandomGen s) => Int -> Rmonad s [(Int,Int)]
generatePairs 0 = do return []
generatePairs 1 = do return []
generatePairs n = do vals <- shuffle $! [1..n]
                     return $ (pairify vals)

nextRListUnique :: (RandomGen s) => Int -> [Int] -> Int -> Rmonad s [Int]
nextRListUnique 0 l _  = do return l
nextRListUnique n l up = do val <- nextR up
                            let t = foldr (||) False (map (\i -> i==val) l)
                            if t == True
                               then do ret <- nextRListUnique n l up
                                       return ret
                               else do ret <- nextRListUnique (n-1) (val:l) up
                                       return ret

nextRListPairs :: (RandomGen s) => Int -> Int -> Rmonad s [(Int,Int)]
nextRListPairs 0 _  = do return []
nextRListPairs n up = do val1 <- nextR up
                         val2 <- nextRDifferent up val1
                         rest <- nextRListPairs (n-1) up
                         return $ ((val1,val2):rest)

nextRDifferent :: (RandomGen s) => Int -> Int -> Rmonad s Int
nextRDifferent up x = do x' <- nextR up
                         if x' == x
                            then do x'' <- nextRDifferent up x
                                    return x''
                            else return x'

runRmonad :: (RandomGen s) => Rmonad s a -> s -> (a, s)
runRmonad (S m) s = runState m s
