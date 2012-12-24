-- |
--  Code for individuals representing the 1D CA Density classification problem
--
-- matt\@galois.com
--
module GEP.Examples.CADensity.CADensityIndividual(
    express_individual,
    fitness_evaluate,
    evaluate_nodes,
    infixWalker,
    tofunc,
    SymTable,
    CANode
) where

import GEP.Expression
import GEP.Examples.CADensity.CAFitness
import GEP.Types
import Maybe

data TriOperator = If
                   deriving Show

data BinOperator = And | Or
                   deriving Show

data UnOperator  = Not
                   deriving Show

data TermChar = L3 | L2 | L1 | CENTER | R1 | R2 | R3
                deriving Show

data CANode = BinOp BinOperator CANode CANode
            | UnOp UnOperator CANode
            | TriOp TriOperator CANode CANode CANode
            | Terminal TermChar
              deriving Show

arity :: Char -> Int
arity 'I' = 3 -- if
arity '|' = 2 -- or
arity '&' = 2 -- and
arity '~' = 1 -- not
arity _   = 0 -- terminal 

infixWalker :: CANode -> String
infixWalker (Terminal c) = [charFromTermChar c]
infixWalker (TriOp _ a b c) = "if ("++(infixWalker a)++") then ("++
                            (infixWalker b)++") else ("++
                            (infixWalker c)++")"
infixWalker (UnOp Not e) = "not("++(infixWalker e)++")"
infixWalker (BinOp op a b) = "("++as++ops++bs++")"
  where
    as = infixWalker a
    bs = infixWalker b
    ops = case op of
            And  -> "&"
            Or   -> "|"

charFromTermChar :: TermChar -> Char
charFromTermChar L3 = 'c'
charFromTermChar L2 = 'b'
charFromTermChar L1 = 'a'
charFromTermChar CENTER = '0'
charFromTermChar R1 = '1'
charFromTermChar R2 = '2'
charFromTermChar R3 = '3'

translateTermChar :: Char -> TermChar
translateTermChar 'c' = L3
translateTermChar 'b' = L2
translateTermChar 'a' = L1
translateTermChar '0' = CENTER
translateTermChar '1' = R1
translateTermChar '2' = R2
translateTermChar '3' = R3
translateTermChar _ = error "Bad"

express :: Char -> [CANode] -> CANode
express c kids =
    case c of
      'I' -> TriOp If lhs rhs thrd
      '~' -> UnOp Not lhs
      '&' -> BinOp And lhs rhs
      '|' -> BinOp Or lhs rhs
      _ -> Terminal (translateTermChar c)
    where
      lhs = head kids
      rhs = head (tail kids)
      thrd = head (tail (tail kids))

lvlAssemble :: [Char] -> [CANode] -> [CANode]
lvlAssemble [] _        = []
lvlAssemble (c:cs) kids = 
    [express c cneed]++(lvlAssemble cs csneed)
    where
      ac = arity c
      (cneed,csneed) = splitAt ac kids

assemble :: [[Char]] -> [CANode]
assemble []     = []
assemble (c:[]) = (map (\x -> Terminal (translateTermChar x)) c)
assemble (c:cs) = lvlAssemble c (assemble cs)

express_individual :: [Char] -> [CANode]
express_individual i = assemble (levelize i 1)

evaluate_nodes :: [CANode] -> CA7Neighborhood -> [Bool]
evaluate_nodes nodes syms =
    map (\x -> (tofunc x) syms) nodes

fitness_evaluate :: CANode -> CA7Neighborhood -> Bool -> Bool
fitness_evaluate node syms target =
    c == target
    where
        c = (tofunc node) syms

tofunc :: CANode -> CA7Rule
tofunc (TriOp If a b c) = (\x -> if ((tofunc a) x) then ((tofunc b) x)
                                                   else ((tofunc c) x))
tofunc (BinOp And a b) = (\x -> ((tofunc a) x) && ((tofunc b) x))
tofunc (BinOp Or a b) = (\x -> ((tofunc a) x) || ((tofunc b) x))
tofunc (UnOp Not a) = (\x -> not ((tofunc a) x))
tofunc (Terminal a) = (\(l3,l2,l1,c,r1,r2,r3) ->
  case a of
    L3 -> l3
    L2 -> l2
    L1 -> l1
    CENTER -> c
    R1 -> r1
    R2 -> r2
    R3 -> r3)
