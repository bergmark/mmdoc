module Tokenizer where

import           Control.Applicative           hiding (many, (<|>))
import           Text.ParserCombinators.Parsec

import           ParsecExtra

data Program = Program [Token]
             deriving (Eq, Show)

data Token = Semi -- ;
           | Algorithm
           | Comment String
           | End
           | Function
           | MComment String
           | Package
           | Record
           | Union
           | W String
           | EOF
             deriving (Eq, Show)

parseFile :: String -> Either ParseError Program
parseFile = parse p_top "(unknown)"

p_top :: CharParser st Program
p_top = Program . (++ [EOF]) <$> many (ws *> p_token <* ws) <* eof

p_token :: CharParser st Token
p_token = semi'
            <|> try algorithm
            <|> try comment
            <|> try end
            <|> try function
            <|> try mcomment
            <|> try package
            <|> try record
            <|> try union
            <|> word
  where
    algorithm = str "algorithm" *> return Algorithm
    comment   = Comment <$> (str "//" *> many1 (noneOf "\r\n") <* many1 (oneOf "\r\n"))
    end       = str "end"       *> return End
    function  = str "function"  *> return Function
    mcomment  = fmap MComment $ str "/*" *> manyTill (noneOf "_") (try (string "*/"))
    package   = str "package"   *> return Package
    record    = str "record"    *> return Record
    semi'     = semi *> return Semi
    union     = str "uniontype" *> return Union
    word      = W <$> many1 (noneOf " \t\n\r;")

