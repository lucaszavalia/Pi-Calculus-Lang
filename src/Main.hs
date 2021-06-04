module Main where
import System.IO
import System.Environment
import Data.Either

data ReplState = Compile | Reduce | NoReduce 

main = do
   args <- getArgs
   case (head args) of
      "-c" -> do compile $ head $ tail args
      "-n" -> do noReduce $ head $ tail args
      "-r" -> do reduction $ head $ tail args
      "-R" -> do repl Compile

compile inputFile = do
   fileContents <- readFile $ openFile ReadMode inputFile
   test_either $ parse parseTerm "" fileContents
   where
      test_either :: Either a b -> IO ()
      test_either x
         | (isRight x) == True   = do evaluate $ reduce $ treeBuilder $ show $ (\(Right z) -> z) x
         | otherwise             = do putStrLn $ (\(Left z) -> z) x

reduction inputFile = do
   fileContents <- readFile $ openFile ReadMode inputFile
   test_either $ parse parseTerm "" fileContents
   where
      test_either :: Either a b -> IO ()
      test_either x
         | (isRight x) == True   = do synth $ reduce $ treeBuilder $ show $ (\(Right z) -> z) x
         | otherwise             = do putStrLn $ show $ (\(Left z) -> z) x
 
repl :: ReplState -> IO ()
repl start = do
   userInput <- getLine
   if head $ userInput == ':'
      then do
         case (words userInput) of
            [":quit"]            -> putStrLn "goodbye :)"
            [":set", "Compile"]  -> do repl Compile
            [":set", "Reduce"]   -> do repl Reduce
            [":set", "NoReduce"] -> do repl NoReduce
      else do
         parsedData <- parse parseTerm "" userInput
         case start of
            Compile  -> do testEither0 parsedData >> repl start
            Reduce   -> do testEither1 parsedData >> repl start
            NoReduce -> do testEither2 parsedData >> repl start
   where
      testEither0 :: Either a b -> IO ()
      testEither0 x
         | (isRight x) == True   = do evaluate $ reduce $ treeBuilder $ show $ (\(Right z) -> z) x
         | otherwise             = do putStrLn $ show $ (\(Left z) -> z) x
      testEither1 :: Either a b -> IO ()
      testEither1 x'
         | (isRight x') == True  = do synth $ reduce $ treeBuilder $ show $ (\(Right z) -> z) x
         | otherwise             = do putStrLn $ show $ (\(Left z) -> z) x'
      testEither2 :: Either a b -> IO ()
      testEither2 x''
         | (isRight x'') == True = do evaluate $ show (\(Right z) -> z) x''
         | otherWise             = do putStrLn $ show $ (\(Left z) -> z) x''
