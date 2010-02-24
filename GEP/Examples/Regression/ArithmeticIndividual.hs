{-|
  Code for individuals representing arithmetic expressions.  This is used
  most frequently for regression applications.

  Author: mjsottile\@computer.org
-}
module GEP.Examples.Regression.ArithmeticIndividual(
    express_individual,
    evaluate,
    fitness_evaluate_absolute,
    fitness_evaluate_relative,
    evaluate_nodes,
    infixWalker,
    aiToGraphviz,
    dumpDotFile
) where

import GEP.Types
import Maybe
import IO

data BinOperator = Plus | Minus | Divide | Times | Exp
                   deriving Show

data UnOperator  = Sqrt
                   deriving Show

data AINode = BinOp BinOperator AINode AINode
            | UnOp UnOperator AINode
            | GeneConnector AINode
            | Terminal Char
              deriving Show

--
-- dump an expressed individual to a file as a graphviz dot file
--
dumpDotFile :: Maybe String -> AINode -> IO ()
dumpDotFile Nothing      _ = return ()
dumpDotFile (Just fname) n = do
  fh <- openFile fname WriteMode
  hPutStrLn fh "digraph HSGEP_Regression {"
  mapM (hPutStrLn fh) (aiToGraphviz n)
  hPutStrLn fh "}"
  hClose fh

-- Node, parent ID, (kidsstring,maxkidid)
arithToGraphviz :: AINode -> Int -> Bool -> ([String],Int)
arithToGraphviz (Terminal c) i _ =
    (["  "++ident++" [label=\""++lbl++"\"];"], i')
  where
    i' = i+1
    ident = "l"++(show i')
    lbl = (show c)

arithToGraphviz (UnOp Sqrt kidNodes) i isGC =
    (["  "++ident++" [label=\""++lbl++"\""++special++"];",
      "  "++ident++" -> "++kidIdent++";"]++kids, kidID)
  where
    special = if isGC then ", color=red" else ""
    i' = i+1
    (kids,kidID) = arithToGraphviz kidNodes i' False
    ident = "l"++(show i')
    kidIdent = "l"++(show (i'+1))
    lbl = "Q"

arithToGraphviz (BinOp bop lKids rKids) i isGC =
    (["  "++ident++" [label=\""++ops++"\""++special++"];",
      "  "++ident++" -> "++lkidIdent++";",
      "  "++ident++" -> "++rkidIdent++";"]++lkidlist++rkidlist, rkidID)
  where
    special = if isGC then ", color=red" else ""
    i' = i+1
    ident = "l"++(show i')
    lkidIdent = "l"++(show (i'+1))
    (lkidlist,lkidID) = arithToGraphviz lKids i' False
    rkidIdent = "l"++(show (lkidID+1))
    (rkidlist,rkidID) = arithToGraphviz rKids lkidID False
    ops = case bop of
            Minus  -> "-"
            Plus   -> "+"
            Divide -> "/"
            Times  -> "*"
            Exp    -> "^"

arithToGraphviz (GeneConnector g) i _ = arithToGraphviz g i True

aiToGraphviz :: AINode -> [String]
aiToGraphviz n = ss
  where
    (ss,_) = arithToGraphviz n 0 False

type AISymTable = SymTable Float

{-|
  Return the arity of a character representing a terminal or nonterminal.

  TODO: This should be made part of the genome, and the arity of each
        symbol should be specified with the symbols in the input file.
-}
arity :: Char -> Int
arity 'Q' = 1
arity '-' = 2
arity '+' = 2
arity '*' = 2
arity '/' = 2
arity '^' = 2
arity _   = 0

levelize :: [Char] -> Int -> [[Char]]
levelize _  0 = []
levelize [] _ = []
levelize s  i =
    [front]++(levelize back (foldr (+) 0 (map arity front)))
    where
      (front,back) = splitAt i s

infixWalker :: AINode -> String
infixWalker (Terminal c) = [c]
infixWalker (UnOp Sqrt e) = "sqrt("++(infixWalker e)++")"
infixWalker (GeneConnector g) = infixWalker g
infixWalker (BinOp op a b) = "("++as++ops++bs++")"
  where
    as = infixWalker a
    bs = infixWalker b
    ops = case op of
            Minus  -> "-"
            Plus   -> "+"
            Divide -> "/"
            Times  -> "*"
            Exp    -> "^"

express :: Char -> [AINode] -> AINode
express c kids =
    case c of
      'Q' -> UnOp Sqrt lhs
      '-' -> BinOp Minus lhs rhs
      '+' -> BinOp Plus lhs rhs
      '*' -> BinOp Times lhs rhs
      '/' -> BinOp Divide lhs rhs
      '^' -> BinOp Exp lhs rhs
      _ -> Terminal c
    where
      lhs = head kids
      rhs = head (tail kids)

lvlAssemble :: [Char] -> [AINode] -> [AINode]
lvlAssemble [] _        = []
lvlAssemble (c:cs) kids = 
    [express c cneed]++(lvlAssemble cs csneed)
    where
      ac = arity c
      (cneed,csneed) = splitAt ac kids

assemble :: [[Char]] -> [AINode]
assemble []     = []
assemble (c:[]) = (map (\x -> Terminal x) c)
assemble (c:cs) = lvlAssemble c (assemble cs)

express_individual :: Individual -> Genome -> AINode
express_individual chrom g = 
  connect_genes g ets
  where
    genes = chromToGenes chrom (geneLength g)
    ets = map (\i -> head (assemble (levelize i 1))) genes

connect_genes :: Genome -> [AINode] -> AINode
connect_genes g x | length x == 1 = head x
connect_genes g x | otherwise     = connect_genes g (xh':ys)
  where
    c = geneConnector g
    xh = head x
    xs = tail x
    y = head xs
    ys = tail xs
    xh' = GeneConnector (express c [xh,y])

lookup_sym :: Char -> AISymTable -> Maybe Float
lookup_sym _ []             = Nothing
lookup_sym '1' _            = Just 1.0
lookup_sym sym ((c,x):syms) =
    if sym==c 
    then 
        Just x 
    else 
        (lookup_sym sym syms)

evaluate :: AINode -> AISymTable -> Float
evaluate node syms =
    case node of
      (GeneConnector g) -> evaluate g syms
      (BinOp op a b) ->
          let ea = evaluate a syms in
          let eb = evaluate b syms
          in
            case op of
              Plus -> ea + eb
              Minus -> ea - eb
              Times -> ea * eb
              Divide -> ea / eb
              Exp -> ea ** eb
      (UnOp Sqrt a) -> sqrt(evaluate a syms)
      (Terminal x) -> fromJust (lookup_sym x syms)

evaluate_nodes :: [AINode] -> AISymTable -> [Float]
evaluate_nodes nodes syms =
    map (\x -> evaluate x syms) nodes

fitness_evaluate_absolute :: AINode -> AISymTable -> Float -> Float -> Float
fitness_evaluate_absolute node syms target selection_range =
    selection_range - (abs (c - target))
    where
        c = evaluate node syms

fitness_evaluate_relative :: AINode -> AISymTable -> Float -> Float -> Float
fitness_evaluate_relative node syms target selection_range =
    selection_range - (abs ( ( (c - target) / target ) * 100.0 ) )
    where
        c = evaluate node syms
