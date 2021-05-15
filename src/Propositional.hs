module Propositional(Log, logParse) where
import Text.Parsec.String
import Text.Parsec.Char
import Text.Parsec.Combinator
import Text.Parsec.Prim
import Control.Monad
import Globals

{---------}
{- TYPES -}
{---------} 

data Log = Booleans Bool |
   And Log Log |
   Or Log Log |
   Xor Log Log |
   Not Log |
   Parenth Log |
   Variable Var
   deriving (Show, Eq)

{--------------------}
{- PARSER FUNCTIONS -}
{--------------------}

-- parse true and false
booleansParse :: Parser Log
booleansParse = do
   val <- oneOf "TF"
   case val of
      'T' -> return $ Booleans True
      'F' -> return $ Booleans False

-- parse boolean variable
varParse :: Parser Log
varParse = do
   val <- parseVar
   return $ Variable val

-- parse parentheses
parenthParse :: Parser Log -> Parser Log
parenthParse inputExpr = do
   void $ char '('
   whitespace
   extra <- inputExpr
   whitespace
   void $ char ')'
   return $ Parenth extra

-- parse boolean or parentheses
logTerm :: Parser Log -> Parser Log
logTerm inputExpr = try booleansParse <|> varParse <|> parenthParse inputExpr

-- add boolean expressions to previous function
binParse' :: Parser Log
binParse' = logTerm binParse

-- parser for boolean operations
binParse :: Parser Log
binParse = do
   fst <- binParse'
   logSuffix fst
   where
      logSuffix fst' = do
         whitespace
         val <- oneOf "&|^"
         whitespace
         snd <- binParse'
         case val of
            '&' -> loop (And fst' snd)
            '|' -> loop (Or fst' snd)
            '^' -> loop (Xor fst' snd)
      loop term' = logSuffix term' <|> return term'

-- parse not operations
logParse :: Parser Log
logParse = do
   val <- optionMaybe $ char '~'
   case val of
      Just '~' -> do
         extra <- binParse'
         return $ Not extra
      Nothing -> do
         extra <- binParse'
         return extra
