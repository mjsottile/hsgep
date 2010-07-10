-- |
-- Code representing a single step of the GEP algorithm resides here.
-- 
-- single step of fitness evaluation, selection and reproduction to make
-- a new population
-- 
-- process includes:
--
--   (1) expression of individuals
--
--   (2) fitness evaluation
--
--   (3) filtration to eliminate individuals yielding impossible
--       fitness values (infinite or NaN)
--
--   (4) preservation of best individual
--
--   (5) generation of roulette selection weights
--
--   (6) roulette selection of individuals
--
--   (7) perform mutation operator
--
--   (8) IS transposition
--
--   (9) RIS transposition
--
--   (10) Gene transposition
--
--   (11) 1Pt recombination
--
--   (12) 2Pt recombination
--
--   (13) Gene recombination
-- 
-- Author: mjsottile\@computer.org
--
module GEP.TimeStep (
  multiStep
) where
    
import GEP.Rmonad
import GEP.MonadicGeneOperations
import GEP.Random
import GEP.Selection
import GEP.Fitness
import GEP.Types
import GEP.Params
import Debug.Trace
import List (sort)


-- | debugging version of (!!) thanks to #haskell help.  by default we let
--   (!!!) simply alias (!!), but when we need to we can swap in a new

--  implementation of (!!) to trace for debugging reasons.
(!!!) :: [a] -> Int -> String -> a

-- debugging version
-- (!!!) x y s = trace (s++": "++(show y)++"//"++(show (length x))) (x !! y)

-- production version : just alias (!!)
(!!!) x y _ = (x !! y)

--
-- helper for type conversion
--
intToDouble :: Int -> Double
intToDouble n = fromInteger (toInteger n)

{-|
  Reassemble a population.  We are given a full population, the set
  of individuals that are to be replaced and their indices.  The output
  of this function is the new population where the unmodified individuals
  are carried forward and those that were modified are replaced with their
  new versions.
-}
putTogether :: [Int]         -- ^ Indices of individuals to replace
            -> [Individual]  -- ^ Replacement individuals
            -> [Individual]  -- ^ Original population
            -> [Individual]  -- ^ New population
putTogether indices replacements original =
  let innerPutTogether cur _ [] [] qs = drop (cur-1) qs
      innerPutTogether cur _ [] _  qs = drop (cur-1) qs
      innerPutTogether cur _ _  [] qs = drop (cur-1) qs
      innerPutTogether cur mx (l:ls) (p:ps) qs =
          if (cur > mx) 
          then 
              []
          else 
              if (l==cur) 
              then 
                  (p:(innerPutTogether (cur+1) mx ls ps qs))
              else 
                  (((!!!) qs (cur-1) "putTogether"):
                   (innerPutTogether (cur+1) mx (l:ls) (p:ps) qs))
  in
    innerPutTogether 1 (length original) indices replacements original

fillFilterGap :: Genome -> 
                 Int -> 
                [(Double,Individual)] ->
                GEPMonad [(Double,Individual)]
fillFilterGap genome popsize pop =
    if (popsize-(length pop)) > 0
    then do newIndividuals <- newPopulation genome (popsize-(length pop))
            newPop <- return $ map (\i -> (0.0,i)) newIndividuals
            return $! pop++newPop
    else return $! pop

applyMutations :: Genome ->
                  SimParams ->
                  Rates ->
                  [Individual] ->
                  GEPMonad [Individual]
applyMutations g params r s = do
    mutated <- mapM (mutate g r) s

    -- IS transposition
    isTransposePop <- nextRListUnique pISCount [] nSelect
    isPopIn <- return $ map (\i -> (!!!) mutated (i-1) "isPopIn")
                           isTransposePop
    isPopOut <- mapM (isTransposer g params) isPopIn
    isPop <- return $ putTogether (sort isTransposePop) isPopOut mutated

    -- RIS transposition
    risTransposePop <- nextRListUnique pRISCount [] nSelect
    risPopIn <- return $ map (\i -> (!!!) isPop (i-1) "risPopIn")
                            risTransposePop
    risPopOut <- mapM (risTransposer g params) risPopIn
    risPop <- return $ putTogether (sort risTransposePop) risPopOut isPop

    -- Gene transposition
    geneTransposePop <- nextRListUnique pGTCount [] nSelect
    genePopIn <- return $ map (\i -> (!!!) risPop (i-1) "genePopIn")
                            geneTransposePop
    genePopOut <- mapM (geneTransposer g) genePopIn
    genePop <- return $ putTogether (sort geneTransposePop) genePopOut risPop

    -- 1Pt crossover
    x1ptPopPairs <- generatePairs nSelect
    x1ptPopSomePairs <- return $ take p1PCount x1ptPopPairs
    x1UnpairPop <- return $ foldr (\(a,b) -> \i -> (a:b:i)) [] x1ptPopSomePairs
    x1ptPopIn <- return $ map (\(a,b) -> ((!!!) genePop (a-1) "x1A",
                                         (!!!) genePop (b-1) "x1B"))
                             x1ptPopSomePairs
    x1ptPopOut <- mapM (x1PHelper g) x1ptPopIn
    x1ptPopOutFlat <- return $ foldr (\(a,b) -> \i -> (a:b:i)) [] x1ptPopOut
    x1ptPop <- return $ putTogether (sort x1UnpairPop) x1ptPopOutFlat genePop

    -- 2Pt crossover
    x2ptPopPairs <- generatePairs nSelect
    x2ptPopSome <- return $ take p2PCount x2ptPopPairs
    x2UnpairPop <- return $ foldr (\(a,b) -> \i -> (a:b:i)) [] x2ptPopSome
    x2ptPopIn <- return $ map (\(a,b) -> ((!!!) x1ptPop (a-1) "x2A",
                                         (!!!) x1ptPop (b-1) "x2B"))
                             x2ptPopSome
    x2ptPopOut <- mapM (x2PHelper g) x2ptPopIn
    x2ptPopOutFlat <- return $ foldr (\(a,b) -> \i -> (a:b:i)) [] x2ptPopOut
    x2ptPop <- return $ putTogether (sort x2UnpairPop) x2ptPopOutFlat x1ptPop

    -- Gene crossover
    xGPopPairs <- generatePairs nSelect
    xGPopSome <- return $ take pGRCount xGPopPairs
    xGUnpairPop <- return $ foldr (\(a,b) -> \i -> (a:b:i)) [] xGPopSome
    xGPopIn <- return $ map (\(a,b) -> ((!!!) x2ptPop (a-1) "xGA",
                                       (!!!) x2ptPop (b-1) "xGB"))
                             xGPopSome
    xGPopOut <- mapM (xGHelper g) xGPopIn
    xGPopOutFlat <- return $ foldr (\(a,b) -> \i -> (a:b:i)) [] xGPopOut
    xGPop <- return $ putTogether (sort xGUnpairPop) xGPopOutFlat x2ptPop

    return xGPop
    where
      nSelect = length s
      fnSelect = intToDouble nSelect
      pISCount = floor (fnSelect * (pIS r))
      pRISCount = floor (fnSelect * (pRIS r))
      pGTCount = floor (fnSelect * (pGT r))
      p1PCount = floor (fnSelect * (p1R r))
      p2PCount = floor (fnSelect * (p2R r))
      pGRCount = floor (fnSelect * (pGR r))
{-| 
 Single step of GEP algorithm
-}
singleStep :: [Individual]       -- ^ List of individuals 
           -> Genome             -- ^ Genome
           -> SimParams          -- ^ Simulation parameters
           -> Rates              -- ^ Gene operator rates
           -> ExpressionFunction a -- ^ Expression function
           -> FitnessFunction a b-- ^ Fitness function
           -> TestDict b                -- ^ Fitness inputs
           -> TestOuts            -- ^ Fitness outputs
           -> GEPMonad (Double,[Individual])
singleStep pop g params r express_individual fitness_evaluate 
           testInputs testOutputs =
    do indices <- roulette weights nSelect

       filtered <- fillFilterGap g nSelect initialFiltering

       -- selection
       selected <- return $ map (\(_,b) -> b) (selector indices filtered)

       -- mutation
       resultingPop <- applyMutations g params r selected

       (bestFitness, bestIndividual) <- case best of
            Just (f, i) -> return (f, i)
            Nothing     -> do newI <- newIndividual g (numGenes g)
                              return (0.0, newI)
--       return $ (trace (bestIndividual++" => "++(show bestFitness)++"  AVG="++(show avgFitness)) (bestFitness,[bestIndividual]++resultingPop))
       return $ (trace ((show bestFitness)++" "++(show avgFitness)) (bestFitness,[bestIndividual]++resultingPop))
    where
      nSelect = length pop - 1
      expressedPop = map (\i -> express_individual i g) pop
      fitnesses = map (\i -> fitness_tester 
                             i (fitness_evaluate) 
                             testInputs testOutputs 
                             (selectionRange params)) 
                  expressedPop
      initialFiltering = fitness_filter fitnesses pop
      avgFitness = foldr (\(x,_) -> 
                          \a     -> a + 
                                    (x / 
                                     (intToDouble (length initialFiltering))))
                         0.0 initialFiltering
      best = getBest initialFiltering
      weights = generate_roulette_weights 
                (intToDouble (length initialFiltering)) 
                (rouletteExponent params)

multiStep :: [Individual]        -- ^ List of individuals
          -> Genome              -- ^ Genome
          -> SimParams           -- ^ Simulation parameters
          -> Rates               -- ^ Gene operator rates
          -> ExpressionFunction a -- ^ Expression function
          -> FitnessFunction a b -- ^ Fitness function
          -> TestDict b                 -- ^ Fitness inputs
          -> TestOuts             -- ^ Fitness outputs
          -> Int                 -- ^ Maximum number of generations to test
          -> Double               -- ^ Ideal fitness
          -> GEPMonad (Double,[Individual])
multiStep pop g params r expresser fitnesser tests outs 0 _ =
    do (bf,newp) <- singleStep pop g params r expresser fitnesser tests outs
       return (bf,newp)
multiStep pop g params r expresser fitnesser tests outs i maxfitness =
    do (bf,newp) <- singleStep pop g params r expresser fitnesser tests outs
       (if (bf == maxfitness)
        then return $ (bf,newp)
        else do (bf',newp') <- multiStep newp g params r expresser fitnesser tests outs (i-1) maxfitness
                return $ (bf',newp'))
