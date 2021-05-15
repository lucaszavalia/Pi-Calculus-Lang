module Test where
import Data.List
import Data.String
import Data.Tree
import Data.Stack
--import qualified Data.Tree.Zipper as Z

type AST = (Tree String, Stack Tree String)

fstAst :: AST -> Tree String
fstAst (a, b) = a

testStr :: [String]
testStr = words "This is (not a (test) again)"

typeToTree :: a -> Tree a
typeToTree = (\x -> Node x [])

addToTree :: Tree a -> a -> Tree a
addToTree ay by = Node (rootLabel by) (typeToTree ay ++ subForest by)

addTree :: Tree a -> Tree a -> Tree a
addTree tr1 tr2 = Node (rootLabel tr1) (singleton tr2 ++ subForest tr1)

strToForest :: [String] -> Forest String
strToForest strList = fmap typeToTree strList

updateLabel :: Tree a -> a -> Tree a
updateLabel inTree value = Node value (subForest inTree)

treeStack :: Stack Tree String
treeStack = newStack

emptyAST :: AST
emptyAST = (Node "" [], treeStack)

builldTree lst = foldl buildOpen emptyAST lst
   where
      buildOpen :: AST -> Tree String  -> AST
      buildOpen (tre, stck) tr
         | rootLabel tr == "("                  = (tr, stackPush stck tre)
         | rootLabel tr == ")"                  = applyFst addTree tr (swap $ fromJust $ stackPop stck)
         | rootLabel $ fst (tre, stck) == ""    = (tr, stck)
         | otherwise                            = (addTree tre tr, stck)
         where
            applyFst :: (a -> a -> a) -> a -> (a, b) -> (a, b)
            applyFst f z (x, y) = (f z x, y)

exportStr = drawTree $ fstAst $ strToForest testStr
