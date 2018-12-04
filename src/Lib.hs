module Lib
  ( Parser
  , sym
  , int
  )
where

import           Data.Void
import           Text.Megaparsec
import           Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer    as L

type Parser = Parsec Void String

{- Utility parsing functions -}

sc :: Parser ()
sc = L.space space1 empty empty

lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc

sym :: String -> Parser String
sym = L.symbol sc

{- Grammar parsing functions -}

int :: Parser Int
int = L.signed sc (lexeme L.decimal)
