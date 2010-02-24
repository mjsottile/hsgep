-- |
--  Code for individuals representing the 1D CA Density classification problem
--
module GEP.Examples.CADensity.CADensityIndividual(
    express_individual,
    evaluate,
    fitness_evaluate,
    evaluate_nodes,
    infixWalker,
    SymTable,
    CANode
) where

import GEP.Types
import Maybe

data TriOperator = If
                   deriving Show

data BinOperator = And | Or
                   deriving Show

data UnOperator  = Not
                   deriving Show

data LINode = BinOp BinOperator LINode LINode
            | UnOp UnOperator LINode
            | TriOp TriOperator LINode LINode LINode
            | Terminal Char
              deriving Show

type SymTable   = [(Symbol,Bool)]

arity :: Char -> Int
arity 'I' = 3 -- if
arity '|' = 2 -- or
arity '&' = 2 -- and
arity '~' = 1 -- not
arity _   = 0 -- terminal

levelize :: [Char] -> Int -> [[Char]]
levelize _  0 = []
levelize [] _ = []
levelize s  i =
    [front]++(levelize back (foldr (+) 0 (map arity front)))
    where
      (front,back) = splitAt i s

infixWalker :: LINode -> String
infixWalker (Terminal c) = [c]
infixWalker (TriOp a b c) = "if ("++(infixWalker a)++") then ("++
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

express :: Char -> [LINode] -> LINode
express c kids =
    case c of
      'I' -> TriOp lhs rhs thrd
      '~' -> UnOp Not lhs
      '&' -> BinOp And lhs rhs
      '|' -> BinOp Or lhs rhs
      _ -> Terminal c
    where
      lhs = head kids
      rhs = head (tail kids)
      thrd = head (tail (tail kids))

lvlAssemble :: [Char] -> [LINode] -> [LINode]
lvlAssemble [] _        = []
lvlAssemble (c:cs) kids = 
    [express c cneed]++(lvlAssemble cs csneed)
    where
      ac = arity c
      (cneed,csneed) = splitAt ac kids

assemble :: [[Char]] -> [LINode]
assemble []     = []
assemble (c:[]) = (map (\x -> Terminal x) c)
assemble (c:cs) = lvlAssemble c (assemble cs)

express_individual :: [Char] -> [LINode]
express_individual i = assemble (levelize i 1)

lookup_sym :: Char -> SymTable -> Maybe Bool
lookup_sym _ []             = Nothing
lookup_sym 't' _            = Just True
lookup_sym 'f' _            = Just False
lookup_sym sym ((c,x):syms) =
    if sym==c 
    then 
        Just x 
    else 
        (lookup_sym sym syms)

evaluate :: LINode -> SymTable -> Bool
evaluate node syms =
    case node of
      (BinOp op a b) ->
          let ea = evaluate a syms in
          let eb = evaluate b syms
          in
            case op of
              And -> ea && eb
              Or  -> ea || eb
      (UnOp Not a) -> not (evaluate a syms)
      (Terminal x) -> fromJust (lookup_sym x syms)

evaluate_nodes :: [LINode] -> SymTable -> [Bool]
evaluate_nodes nodes syms =
    map (\x -> evaluate x syms) nodes

fitness_evaluate :: LINode -> SymTable -> Bool -> Bool
fitness_evaluate node syms target =
    c == target
    where
        c = evaluate node syms
