module Control where
import Text.Parsec.String
import Text.Parsec.Char
import Text.Parsec.Combinator
import Text.Parsec.Prim
import Globals
import Compare
import PiCalculus

{---------}
{- TYPES -}
{---------}

data Control = ITE Compare Term Term |
   Assign Var Value

{-------------------}
{- PARSE FUNCTIONS -}
{-------------------}

parseITE :: Parser Control
parseITE = do
   test <- parseCompare
   whitespace
   term1 <- parseTerm
   whitespace
   term2 <- parseTerm
   return $ ITE test term1 term2

parseAssign :: Parser Control
parseAssign = do
   name <- parseVar
   whitespace
   void $ char '='
   whitespace 
   val <- parseValue
   return $ Assign name val
