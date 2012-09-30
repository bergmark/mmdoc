module Tokenizer where

import           Control.Applicative           hiding (many, (<|>))
import           Text.ParserCombinators.Parsec

import           ParsecExtra

type Program = [Token]

data Token = Semi -- ;
           | Algorithm
           | End
           | Function
           | Package
           | Record
           | Union
           | W String
             deriving (Eq, Show)

parseFile :: String -> Either ParseError [Token]
parseFile = parse p_top "(unknown)"

p_top :: CharParser st [Token]
p_top = many (ws *> p_token <* ws) <* eof

p_token :: CharParser st Token
p_token = semi' <|> try algorithm <|> try package <|> try union <|> try record <|> try function <|> try end <|> word
  where
    algorithm = str "algorithm" *> return Algorithm
    end       = str "end"       *> return End
    function  = str "function"  *> return Function
    package   = str "package"   *> return Package
    record    = str "record"    *> return Record
    semi'     = semi *> return Semi
    union     = str "uniontype" *> return Union
    word      = W <$> many1 (noneOf " \t\n\r;")
