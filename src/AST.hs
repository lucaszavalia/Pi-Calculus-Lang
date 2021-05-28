module AST where
import Data.List
import Data.String
import Data.Tree
import Data.Stack
import Data.Maybe
import Data.Tuple
import PiCalculus

--AST type and related functions
type AST = (Stack (Tree String), Tree String)

emptyAST :: AST
emptyAST = (stackNew, Node "" [])

updateAST :: AST -> Tree String -> AST
updateAST (x, y) tr = (x, tr)

addAST :: AST -> Tree String -> AST
addAST (x, y) tr = (x, addTree y tr) 

popAST :: AST -> Tree String -> AST
popAST (x, y) tr = addAST (fromJust $ stackPop x) (snd $ addAST (x, y) tr)

popAST' :: AST -> AST
popAST' (x, y)  
   | stackIsEmpty x == True   = (x, y)
   | otherwise                = popAST' $ popAST'' (x, y)
   where
      popAST'' :: AST -> AST
      popAST'' (x', y') = addAST (fromJust $ stackPop x') y'

pushAST :: AST -> Tree String -> AST
pushAST (x, y) tr = (stackPush x y, tr)

getTree :: AST -> Tree String
getTree (x, y) = y

--Functions on trees
addTree :: Tree a -> Tree a -> Tree a --add tr2 to subforest of tr1
addTree tr1 tr2 = Node (rootLabel tr1) (subForest tr1 ++ [tr2])

--Core functions
buildTree lst = foldl buildTree' emptyAST lst
   where
      buildTree' :: AST -> Tree String -> AST
      buildTree' val tr
         | has_both tr == True   = addAST val tr
         | has_left tr == True   = pushAST val tr
         | has_right tr == True  = popAST val tr
         | rootLabel tr == ""    = updateAST val tr
         | otherwise             = addAST val tr
         where
            has_both x = has_left x && has_right x
            has_left x = elem '(' (rootLabel x)
            has_right x = elem ')' (rootLabel x)

prepString :: String -> Forest String
prepString str = fmap (\x -> Node x []) (words str)

treeBuilder :: String -> Tree String
treeBuilder str = fmap removeParens (getTree $ popAST' $ buildTree $ prepString str)
   where
      removeParens xs = filter (\x -> x `notElem` "()") xs
