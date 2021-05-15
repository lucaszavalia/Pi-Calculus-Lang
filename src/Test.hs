module Test where
import Data.List
import Data.String
import Data.Tree
import Data.Stack
import Data.Maybe
import Data.Tuple
--import qualified Data.Tree.Zipper as Z

type AST = (Tree String, Stack (Tree String))

fstAst :: AST -> Tree String
fstAst (a, b) = a

testStr :: [String]
testStr = words "This is ( not a ( test ) again )"

typeToTree :: a -> Tree a
typeToTree = (\x -> Node x [])

addToTree :: Tree a -> a -> Tree a
addToTree ay by = Node (rootLabel ay) ((\x -> [Node x []]) by ++ subForest ay)

addTree :: Tree a -> Tree a -> Tree a
addTree tr1 tr2 = Node (rootLabel tr1) ((\x -> [x]) tr2 ++ subForest tr1)

strToForest :: [String] -> Forest String
strToForest strList = fmap typeToTree strList

updateLabel :: Tree a -> a -> Tree a
updateLabel inTree value = Node value (subForest inTree)

treeStack :: Stack (Tree String)
treeStack = stackNew

emptyAST :: AST
emptyAST = (Node "" [], treeStack)

buildTree lst = foldl buildOpen emptyAST lst
   where
      buildOpen :: AST -> Tree String  -> AST
      buildOpen (tre, stck) tr
         | rootLabel tr == "("                  = (tr, stackPush stck tre)
         | rootLabel tr == ")"                  = applyFst addTree (swap $ fromJust $ stackPop stck) tr
         | rootLabel tre == ""                  = (tr, stck)
         | otherwise                            = (addTree tre tr, stck)
         where
            applyFst :: (a -> a -> a) -> (a,b) -> a -> (a, b)
            applyFst f (x, y) z = (f x z, y)

exportStr = drawTree $ fstAst $ buildTree $ strToForest testStr
--exportStr = drawTree $ addTree (Node "this" [Node "is" [Node "a" []], Node "Test" []]) (Node "Syke!" [])
