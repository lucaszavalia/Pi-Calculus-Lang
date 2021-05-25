module Main where
import System.IO
import Text.Parsec.Prim
import Data.Tree
import Data.Either
import PiCalculus
import AST

main :: IO ()
main = do
   str <- getLine
   putStrLn $ drawTree $ treeBuilder $ show $ (\(Right x) -> x) $ parse parseTerm "" str
