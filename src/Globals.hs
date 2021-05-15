module Globals where
import Text.Parsec.Prim
import Text.Parsec.Char
import Text.Parsec.Combinator
import Text.Parsec.String
import Control.Monad

{---------}
{- TYPES -}
{---------}

type Chan = String

type Var = String

{--------------------}
{- PARSER FUNCTIONS -}
{--------------------}

whitespace :: Parser ()
whitespace = void $ many $ oneOf " \n\t"

parseChan :: Parser Chan
parseChan = do
   name <- many1 upper
   return name

parseVar :: Parser Var
parseVar = do
   name <- many1 lower
   return name
