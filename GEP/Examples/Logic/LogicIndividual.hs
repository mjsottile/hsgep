{-|
  Code for individuals representing logic expressions.

  Author: mjsottile\@computer.org
-}
module GEP.Examples.LogicIndividual(
    express_individual,
    evaluate,
    fitness_evaluate,
    evaluate_nodes,
    infixWalker,
    SymTable,
    LINode
) where

import GEO.Expression
import GEP.Types
import Maybe

data BinOperator = And | Or
                   deriving Show

data UnOperator  = Not
                   deriving Show

data LINode = BinOp BinOperator LINode LINode
            | UnOp UnOperator LINode
            | Terminal Char
              deriving Show

type SymTable   = [(Symbol,Bool)]

arity :: Char -> Int
arity '|' = 2 -- or
arity '&' = 2 -- and
arity '~' = 1 -- not
arity _   = 0 -- terminal

infixWalker :: LINode -> String
infixWalker (Terminal c) = [c]
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
      '~' -> UnOp Not lhs
      '&' -> BinOp And lhs rhs
      '|' -> BinOp Or lhs rhs
      _ -> Terminal c
    where
      lhs = head kids
      rhs = head (tail kids)

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
