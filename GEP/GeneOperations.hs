-- |
--  Operations on the chromosomes of individuals.  The following assumptions
--  are made.
-- 
--   * Symbols are numbered 1 through n for a chromosome of length n.
-- 
--   * Genes are numbered 0 through m-1 for a chromosome with m genes.
--   
--  The functions provided in this module are purely functional.  See
--  "GEP.MonadicGeneOperations" for code that invokes these from within the
--  "GEP.Rmonad" monad.
--  

module GEP.GeneOperations (
  crossover1pt,
  crossover2pt,
  crossoverGene,
  transposeGene,
  transposeIS,
  transposeRIS
) where

import GEP.Types

-- | 
--  One-point crossover
crossover1pt :: ([Symbol], [Symbol]) -- ^ Pair of individuals before crossover
             -> Int                  -- ^ Crossover point
             -> ([Symbol],[Symbol])  -- ^ Pair of individuals after crossover
crossover1pt (x,y) loc = (x', y')
  where
    (fx, bx) = splitAt (loc-1) x
    (fy, by) = splitAt (loc-1) y
    x' = fx++by
    y' = fy++bx

--
-- helper to split a list into three parts. 
--
splitThirds :: [a] -> Int -> Int -> ([a],[a],[a])
splitThirds x l1 l2 = (fx,mx,bx)
  where
    (fx,tmp) = splitAt l1 x
    (mx,bx) = splitAt (l2-l1) tmp

-- |
--  Two-point crossover
crossover2pt :: ([Symbol], [Symbol]) -- ^ Pair of individuals before crossover
             -> Int                  -- ^ Crossover point 1
             -> Int                  -- ^ Crossover point 2
             -> ([Symbol],[Symbol])  -- ^ Pair of individuals after crossover
crossover2pt (x,y) loc1 loc2 = (x',y')
  where
    -- make sure we know which location is lower than the other
    minLoc = min loc1 loc2
    maxLoc = max loc1 loc2
    (fx,mx,bx) = splitThirds x (minLoc-1) (maxLoc-1)
    (fy,my,by) = splitThirds y (minLoc-1) (maxLoc-1)
    x' = fx++my++bx
    y' = fy++mx++by

--
-- Helper to extract a gene from a sequence and return the sequence
-- before the gene, the gene itself, and the sequence after the gene.
--
geneExtract :: [Symbol] -> Int -> Int -> ([Symbol],[Symbol],[Symbol])
geneExtract x gene geneLen = (before, theGene, after)
  where
    geneStart = geneLen * gene
    geneEnd   = geneStart + geneLen
    (before,theGene,after) = splitThirds x geneStart geneEnd

-- |
--  Gene crossover
crossoverGene :: ([Symbol], [Symbol]) -- ^ Pair of individuals before crossover
              -> Int                  -- ^ Gene number for crossover
              -> Int                  -- ^ Gene length in symbols
              -> ([Symbol], [Symbol]) -- ^ Pair of individuals after crossover
crossoverGene (x,y) gene geneLen = (x',y')
  where
    (fx,mx,bx) = geneExtract x gene geneLen
    (fy,my,by) = geneExtract y gene geneLen
    x' = fx++my++bx
    y' = fy++mx++by

--
-- Find a root insertion sequence within a sequence.  This means looking
-- for the first subsequence that starts with a nonterminal.  If no such
-- subsequence exists, return the empty list.
--
findRIS :: [Symbol] -> Genome -> [Symbol]
findRIS [] _                           = []
findRIS (x:xs) g | (isNonterminal x g) = (x:xs)
findRIS (_:xs) g | otherwise           = findRIS xs g

-- |
--  Root insertion sequence transposition.
transposeRIS :: [Symbol] -- ^ Sequence to perform RIS transposition on
             -> Genome   -- ^ Genome information
             -> Int      -- ^ Gene to perform RIS transposition within
             -> Int      -- ^ Position within gene to start search for
                         --   RIS for transposition
             -> Int      -- ^ Length of RIS
             -> [Symbol] -- ^ Sequence after RIS transposition performed
transposeRIS x genome gene pos len = 
    fx ++ risSeq ++ keepHead ++ geneTail ++ bx
  where
    -- pull the gene out that we want
    geneLen = (geneLength genome)
    (fx,theGene,bx) = geneExtract x gene geneLen

    -- separate into head and tail
    (geneHead, geneTail) = splitAt (headLength genome) theGene

    -- find the root insertion sequence within the candidate region given
    -- by the search start position
    risCandidateRegion = drop pos theGene
    risSeq = take len (findRIS risCandidateRegion genome)

    -- determine how much of the head to preserve based on the length of
    -- the root insertion sequence
    keepHeadlen = (headLength genome) - (length risSeq)

    -- extract the parts of the head and tail of the original gene that
    -- are preserved after transposition
    keepHead    = take keepHeadlen geneHead

insertIntoGene :: [Symbol] -> [Symbol] -> Int -> Int -> [Symbol]
insertIntoGene x ins hl pos = (take hl (pre++ins++post))++tX
  where
    hX = take hl x
    tX = drop hl x
    pre = take pos x
    post = drop pos hX

-- |
--  Insertion sequence transposition.
transposeIS :: [Symbol]  -- ^ Chromosome
            -> Genome    -- ^ Genome
            -> Int       -- ^ Gene number
            -> Int       -- ^ Position to take from within a gene
            -> Int       -- ^ Length to take
            -> Int       -- ^ Position to put within a gene
            -> [Symbol]  -- ^ Resulting chromosome
transposeIS x genome genenum takepos len putpos = 
    genesBefore ++ gene' ++ genesAfter
  where
    geneLen = (geneLength genome)
    (genesBefore, gene, genesAfter) = geneExtract x genenum geneLen
    iseq = take len (drop takepos gene)
    gene' = insertIntoGene gene iseq (headLength genome) putpos

-- |
--  Gene transposition.
transposeGene :: [Symbol] -- ^ Chromosome
              -> Genome   -- ^ Genome
              -> Int      -- ^ Gene number
              -> [Symbol] -- ^ Resulting chromosome
transposeGene x genome gnum = gene++pregene++postgene
  where
    geneLen = (headLength genome) + (tailLength genome)
    gene = take geneLen (drop (geneLen * gnum) x)
    pregene = take (geneLen * gnum) x
    postgene = drop (geneLen * (gnum+1)) x

