module AST where
import Text.Parsec.Prim
import Data.Tree
import Data.Tree.Zipper
import Data.String
import PiCalculus

{---------}
{- TYPES -}
{---------}

data AST = [(Int, Forest String)]

{-----------------------}
{- AUXILIARY FUNCTIONS -}
{-----------------------}

-- intermediate tokenizer
tokenizer :: Term -> [String]
tokenizer x = words $ rectify $ show x
   where
      rectify = concatMap (\x -> if x == '(' then "( " else (if x == ')' then " )" else [x]))

-- last tokenizer (consider fusing with the preceding function
getTokens :: String -> [String]
getTokens str = tokenizer $ fromRight TermErr (parse parseTerm "" str)

-- turn token list into Forest of singletons
listToForest :: [a] -> Forest a
listToForest xs = fmap (\x -> Node x []) xs

{------------------}
{- CORE FUNCTIONS -}
{------------------}

