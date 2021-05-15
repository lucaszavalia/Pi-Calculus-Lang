module Arithmetic(Arith, arithParse) where
import Text.Parsec.String
import Text.Parsec.Char
import Text.Parsec.Combinator
import Text.Parsec.Prim
import Control.Monad
import Globals

{---------}
{- TYPES -}
{---------}

data Arith = Num Int |
   Add Arith Arith |
   Sub Arith Arith |
   Mul Arith Arith |
   Div Arith Arith |
   Parens Arith |
   Variable Var
   deriving (Show, Eq)

{--------------------}
{- PARSER FUNCTIONS -}
{--------------------}

-- parse number (i.e read as Int)
numParse :: Parser Arith
numParse = do
   val <- many1 digit
   return $ Num $ read val

-- parse variable
variableParse :: Parser Arith
variableParse = do
   val <- parseVar
   return $ Variable val

-- parse parentheses
parensParse :: Parser Arith -> Parser Arith
parensParse inputExpr = do
   void $ char '('
   whitespace
   extra <- inputExpr
   whitespace
   void $ char ')'
   return $ Parens extra

-- parse Num or Parens
arithTerm :: Parser Arith -> Parser Arith
arithTerm inputExpr = numParse <|> variableParse <|> parensParse inputExpr

-- add arithmetic parser to the previous function
arithParse' :: Parser Arith
arithParse' = arithTerm arithParse

-- recursively parse arithmetic
arithParse :: Parser Arith
arithParse = do
   fst <- arithParse'
   arithSuffix fst
   where
      arithSuffix fst' = do
         whitespace
         val <-  oneOf "+-*/"
         snd <- arithParse'
         case val of
            '+' -> loop (Add fst' snd)
            '-' -> loop (Sub fst' snd)
            '*' -> loop (Mul fst' snd)
            '/' -> loop (Div fst' snd)
      loop term' = arithSuffix term' <|> return term'
