{-# OPTIONS -fno-warn-missing-signatures #-}

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
           | Input
           | Output
           | Match
           | Case
           | Then
           | EOF
             deriving (Eq, Show)

isW (W _) = True
isW _ = False
fromW (W s) = s
fromW _ = error "fromW"
isInputOutput p = Input == p || Output == p

parseFile :: String -> Either ParseError Program
parseFile = parse p_top "(unknown)"

p_top :: CharParser st Program
p_top = Program . (++ [EOF]) <$> many (ws *> p_token <* ws) <* eof

p_token :: CharParser st Token
p_token = semi'
            <|> try algorithm
            <|> try comment
            <|> try end
            <|> try input
            <|> try output
            <|> try match
            <|> try function
            <|> try mcomment
            <|> try _case
            <|> try _then
            <|> try package
            <|> try record
            <|> try union
            <|> word
  where
    algorithm = str "algorithm" *> return Algorithm
    comment   = Comment <$> (str "//" *> many1 (noneOf "\r\n") <* many1 (oneOf "\r\n"))
    end       = str "end"       *> return End
    input     = str "input"     *> return Input
    output    = str "output"    *> return Output
    match     = str "match"     *> return Match
    function  = str "function"  *> return Function
    mcomment  = fmap MComment $ str "/*" *> manyTill (noneOf "_") (try (string "*/"))
    package   = str "package"   *> return Package
    record    = str "record"    *> return Record
    _case     = str "case"      *> return Case
    _then     = str "then"      *> return Then
    semi'     = semi *> return Semi
    union     = str "uniontype" *> return Union
    word      = W <$> many1 (noneOf " \t\n\r;")

