-- | This module defines the types used for implementing GEP problems
--   and operations.  A few functions are also provided for convenience
--   here for performing common operations.
--

module GEP.Types (
    -- * Types
    Genome(..),
    Symbol,
    Gene,
    Chromosome,
    Individual,
    SymTable,

    -- * Functions
    tailLength,
    geneLength,
    allsymbols,
    chromToGenes,
    genesToChrom,
    isNonterminal
) where

-- | A symbol in a chromosome
type Symbol     = Char

-- | A gene in a chromosome is a list of symbols
type Gene       = [Symbol]

-- | A chromosome is a list of symbols.  We avoided using a list of genes to
--   maintain the view of a chromosome as nothing more than a flattened,
--   linear sequence of genes.
type Chromosome = [Symbol]

-- | An individual is a chromosome
type Individual = Chromosome

-- | Symbol table used for fitness tests.  We assume that there is exactly
--   one pair per symbol.  If there are symbols missing, fitness testing
--   may fail (the library does not have facilities yet to allow for
--   default values).  If a symbol occurs multiple times in the symbol
--   table, no guarantee is provided for which value will be chosen.
type SymTable a = [(Symbol,a)]

-- | Data type representing a genome.  The genome contains all necessary
--   parameters to interpret a chromosome.  These include the alphabet (split
--   between terminal and nonterminal characters), connective characters for
--   multi-gene chromosomes, the maximum arity of any nonterminal, the length
--   of the head of a gene, and the number of genes per chromosome.
data Genome = Genome {
      terminals     :: [Symbol], -- ^ Set of terminal symbols
      nonterminals  :: [Symbol], -- ^ Set of nonterminal symbols
      geneConnector :: Symbol,   -- ^ Symbol connecting genes in a chromosome
      maxArity      :: Int,      -- ^ Highest arity nonterminal function
      headLength    :: Int,      -- ^ Length of gene head sequence
      numGenes      :: Int       -- ^ Number of genes per chromosome
} deriving Show

-- | Given a genome, provide the list of all symbols possible in a chromosome.
--   This is just nonterminals ++ terminals.
allsymbols :: Genome   -- ^ Genome 
           -> [Symbol] -- ^ List of symbols
allsymbols g = (terminals g)++(nonterminals g)

-- | Return the length of the tail of a gene for a given genome
tailLength :: Genome   -- ^ Genome
           -> Int      -- ^ Number of symbols in a gene tail
tailLength g = ((headLength g) * ((maxArity g)-1))+1

-- | Return length of a gene (tail + head) for a given genome
geneLength :: Genome   -- ^ Genome 
           -> Int      -- ^ Total length of a gene.
geneLength g = (headLength g) + (tailLength g)

-- | Test if a symbol is a nonterminal
isNonterminal :: Symbol  -- ^ Symbol to test 
              -> Genome  -- ^ Genome providing context
              -> Bool    -- ^ True if symbol is a nonterminal, false otherwise
isNonterminal s g =
  let isNT []                 = False
      isNT (x:_)  | (s == x)  = True
      isNT (_:xs) | otherwise = (isNT xs)
  in
    isNT (nonterminals g)

-- | Fracture a chromosome into a set of genes
chromToGenes :: Chromosome  -- ^ Chromosome to split into a set of genes 
             -> Int         -- ^ Length of a single gene
             -> [Gene]      -- ^ Ordered list of genes from chromosome
chromToGenes [] _ = []
chromToGenes c  glen = (take glen c):(chromToGenes (drop glen c) glen)

-- | Assemble a chromosome from a set of genes
genesToChrom :: [Gene]      -- ^ List of genes
             -> Chromosome  -- ^ Chromosome assembled from genes
genesToChrom genes = foldl (++) [] genes
