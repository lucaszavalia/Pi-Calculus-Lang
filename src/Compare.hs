module Compare where
import Text.Parsec.String
import Text.Parsec.Char
import Text.Parsec.Combinator
import Text.Parsec.Prim
import Globals
import Propositional
import Arith

{---------}
{- TYPES -}
{---------}

data Value = Log | Arith

data Comp = Equal Value Value |
   LT Arith Arith |
   GT Arith Arith |
   LTE Arith Arith |
   GTE Arith Arith

{-------------------}
{- PARSE FUNCTIONS -}
{-------------------}

parseValue :: Parser Value
parseValue = try logParse <|> arithParse

parseEqual :: Parser Comp
parseEqual = do
   val1 <- parseValue
   whitespace
   void $ string "=="
   whitespace
   val2 <- parseValue
   return $ Equal val1 val2

parseLT :: Parser Comp
parseLT = do
   num1 <- parseArith
   whitespace
   void $ char '<'
   whitespace
   num2 <- parseArith
   return $ LT num1 num2

parseGT :: Parser Comp
parseGT = do
   num1 <- parseArith
   whitespace
   void $ char '>'
   whitespace
   num2 <- parseArith
   return $ GT num1 num2

parseLTE :: Parser Comp
parseLTE = do
   num1 <- parseArith
   whitespace
   void $ string "<="
   whitespace
   num2 <- parseArith
   return $ LTE num1 num2

parseGTE :: Parser Comp
parseGTE = do
   num1 <- parseArith
   whitespace
   void $ string ">="
   whitespace
   num2 <- parseArith
   return $ GTE num1 num2
