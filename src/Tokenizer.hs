{-# OPTIONS -fno-warn-missing-signatures #-}

module Tokenizer where

import           Control.Applicative           hiding (many, (<|>))
import           Text.ParserCombinators.Parsec

import           ParsecExtra

data Program = Program [Token]
             deriving (Eq, Show)

data Token = Semi -- ;
           | Algorithm
           | Case
           | Comma
           | Comment String
           | Dot -- .
           | Encapsulated
           | End
           | Function
           | Import
           | Input
           | ListEnd
           | ListStart
           | MComment String
           | Match
           | Output
           | Package
           | Partial
           | Protected
           | Record
           | Then
           | Union
           | W String
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
p_token = (char ';' *> return Semi)
            <|> char ','                *> return Comma
            <|> char '.'                *> return Dot
            <|> char '{'                *> return ListStart
            <|> char '}'                *> return ListEnd
            <|> try (Comment <$> (str "//" *> many1 (noneOf "\r\n") <* many1 (oneOf "\r\n")))
            <|> try (fmap MComment $ str "/*" *> manyTill (noneOf "_") (try (string "*/")))
            <|> try (str "algorithm"    *> return Algorithm)
            <|> try (str "case"         *> return Case)
            <|> try (str "encapsulated" *> return Encapsulated)
            <|> try (str "end"          *> return End)
            <|> try (str "function"     *> return Function)
            <|> try (str "import"       *> return Import)
            <|> try (str "input"        *> return Input)
            <|> try (str "match"        *> return Match)
            <|> try (str "output"       *> return Output)
            <|> try (str "package"      *> return Package)
            <|> try (str "partial"      *> return Partial)
            <|> try (str "protected"    *> return Protected)
            <|> try (str "record"       *> return Record)
            <|> try (str "then"         *> return Then)
            <|> try (str "uniontype"    *> return Union)
            <|> W <$> many1 (choice [letter, digit, oneOf ":=*"])
