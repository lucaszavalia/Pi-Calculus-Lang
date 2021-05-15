module PiCalculus(Term, parseTerm)  where
import Text.Parsec.String
import Text.Parsec.Char
import Text.Parsec.Combinator
import Text.Parsec.Prim
import Control.Monad
import Arithmetic
import Propositional
import Globals

{---------}
{- TYPES -}
{---------}

data Term = Receive Var Chan Term |
   Send Var Chan Term |
   Simult Term Term |
   Bind Chan Term |
   Repeat Term |
   Null |
   BTerm Log |
   ITerm Arith |
   TermErr
   deriving (Show)

{--------------------}
{- PARSER FUNCTIONS -}
{--------------------}

-- parse idling operation
parseNull :: Parser Term
parseNull = do
   void $ char '_'
   return $ Null

-- parse boolean term
parseBTerm :: Parser Term
parseBTerm = do
   val <- logParse
   return $ BTerm val

-- parse integer term
parseITerm :: Parser Term
parseITerm = do
   val <- arithParse
   return $ ITerm val

-- parse repeat operation
parseRepeat :: Parser Term -> Parser Term
parseRepeat inputExpr = do
   void $ char '!'
   extra <- inputExpr
   return $ Repeat extra

-- disjunction of the previous two operations
parseConst :: Parser Term -> Parser Term
parseConst inputExpr = parseNull <|> parseBTerm <|> parseITerm <|> parseRepeat inputExpr <|> inputExpr

--parse Bind, Receive, and Send operations
parseBRS' :: Parser Term
parseBRS' = parseConst parseBRS

parseBRS :: Parser Term
parseBRS = do
   void $ char '['
   test <- optionMaybe $ string "new "
   case test of
      Just "new " -> do
         chan <- parseChan
         void $ char ']'
         whitespace
         void $ char '{'
         whitespace
         extra <- parseTerm'
         whitespace
         void $ char '}'
         return $ Bind chan extra
      Nothing -> do
         chan <- parseChan
         val <- string "->" <|> string "<-"
         var <- parseVar
         void $ char ']'
         case val of
            "->" -> do
               whitespace
               void $ char '{'
               whitespace
               extra <- parseTerm'
               whitespace
               void $ char '}'
               return $ Receive chan var extra
            "<-" -> do
               whitespace
               void $ char '{'
               whitespace
               extra <- parseTerm'
               whitespace
               void $ char '}'
               return $ Send chan var extra

-- parallelism operation and RBS parser
parseTerm' :: Parser Term
parseTerm' = parseConst parseTerm

parseTerm :: Parser Term
parseTerm = do
   fst <- parseBRS'
   whitespace
   termSuffix fst
   where
      termSuffix fst' = do
         test <- optionMaybe $ char '%'
         case test of
            Just '%' -> do
               whitespace
               snd <- parseTerm'
               loop (Simult fst' snd)
            Nothing -> do
               return fst'
      loop inputExpr = termSuffix inputExpr <|> return inputExpr
