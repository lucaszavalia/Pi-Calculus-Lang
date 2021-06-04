module Rewrite where
import Data.Tree
import Data.List
import PiCalculus
import AST

type AST' = Tree String

{--------}
{-AXIOMS-}
{--------}

--reverse the subforest of simult to commute its values
simultCommute :: AST' -> AST'
simultCommute val_tr
   | (rootLabel val_tr) == "Simult"   = Node "Simult" (reverse $ subForest val_tr)
   | otherwise                        = val_tr

-- turn (Simult (Simult P Q) R) to (Simult P (Simult Q R)) and vice versa
simultAssoc :: AST' -> AST'
simultAssoc val_tr
   | tstR val_tr == True   = y3 val_tr --after test of right condition
   | tstL val_tr == True   = y3 $ simultCommute val_tr --after test of left condition
   | otherwise             = val_tr
   where
      tstR val = ((rootLabel val) == "Simult") && ((rootLabel $ head $ subForest val) == "Simult")
      tstL val' = ((rootLabel val') == "Simult") && ((rootLabel $ last $ subForest val') == "Simult")
      y3 x = Node "Simult" [head $ subForest $ y2 x, Node "Simult" (tail $ subForest $ last $ subForest $ y2 x)] where
         y2 x'  = (\z -> Node "Simult" z) $ init $ subForest $ y1 x' where
            y1 x'' = Node "Simult" [head $ y0 x'', (\z -> Node "Simult" z) $ y0 x'', last $ y0 x''] where
               y0 x''' =  (subForest $ head $ subForest x''') ++ [last $ subForest x''']

{------------}
{-REDUCTIONS-}
{------------}

--perform transformation (Simult P Null) -> P or (Simult Null Q) -> Q  or (Simult Null Null) -> Null
simultNull :: AST' -> AST'
simultNull val_tr
   | tstForAll val_tr   = Node "Null" [] --constant tree 
   | tstR val_tr        = head $ subForest val_tr
   | tstL val_tr        = last $ subForest val_tr
   | otherwise          = val_tr
   where
      tstL :: AST' -> Bool
      tstL val = ((head $ subForest val) == Node "Null" [])
      tstR :: AST' -> Bool --test if either P or Q in (Simult P Q) is null
      tstR val' = ((last $ subForest val') == Node "Null" []) 
      tstForAll :: AST' -> Bool --test if both P and Q in (Simult P Q) are null
      tstForAll val'' = ((last $ subForest val'') == Node "Null" []) &&  ((head $ subForest val'') == Node "Null" [])


--perform transformation (Bind "CHAN" Null) -> Null
bind_null :: AST' -> AST'
bind_null val_tr
   | tstBindNull val_tr = Node (rootLabel val_tr) [Node "Null" []]
   | otherwise          = val_tr
   where
      tstBindNull :: AST' -> Bool --test if the current node is Bind and the child is null
      tstBindNull val = ((head $ subForest val) == Node "Bind" []) && ((last $ subForest val) == Node "Null" [])

{-----------}
{-AUXILIARY-}
{-----------}

symbol_table :: [String]
symbol_table = [
   "Bind",
   "Send",
   "Receive",
   "Simult",
   "Repeat",
   "Null",
   "BTerm",
   "ITerm",
   "And",
   "Or",
   "Xor",
   "Not",
   "Parenth",
   "Variable",
   "Num",
   "Add",
   "Sub",
   "Mul",
   "Div",
   "Parens"]   

rename :: AST' -> String -> String -> AST'
rename ast_vals src dest= fmap (\z -> rename' src dest z) ast_vals where
   rename' :: String -> String -> String -> String
   rename' src' dest' val
      | val == src'  = dest'
      | otherwise    = val

{-----------------------------------------------
 - Module that implements term rewriting rules -
 -----------------------------------------------}
