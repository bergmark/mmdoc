{-# OPTIONS -fno-warn-missing-signatures #-}

module Tokenizer where

import           Control.Applicative           hiding (many, (<|>))
import           Text.ParserCombinators.Parsec

import           ParsecExtra

data Program = Program [Token]
             deriving (Eq, Show)

data Token = Algorithm
           | Case
           | Comma
           | Comment String
           | Constant
           | Dot -- .
           | Else
           | Elseif
           | Encapsulated
           | End
           | Function
           | Gt
           | If
           | Import
           | Input
           | ListEnd
           | ListStart
           | Lt
           | MComment String
           | Match
           | Not
           | Output
           | Package
           | ParenL
           | ParenR
           | Partial
           | Public
           | Protected
           | Record
           | Semi -- ;
           | Then
           | Type
           | Union

           | Str String
           | S String -- Infix operator, including "or" and "and"
           | W String -- Word
           | EOF
             deriving (Eq, Show)

isW (W _) = True
isW _ = False
isStr (Str _) = True
isStr _ = False
isSym (S _) = True
isSym _ = False
fromW (W s) = s
fromW _ = error "fromW"
fromS (S s) = s
fromS _ = error "fromS"
fromStr (Str s) = s
fromStr _ = error "fromStr"
isInputOutput p = Input == p || Output == p

parseFile :: String -> Either ParseError Program
parseFile = parse p_top "(unknown)"

p_top :: CharParser st Program
p_top = Program . (++ [EOF]) <$> many (ws *> p_token <* ws) <* eof

p_token :: CharParser st Token
p_token =
  (char ';' *> return Semi)
    <|> char '('                *> return ParenL
    <|> char ')'                *> return ParenR
    <|> char ','                *> return Comma
    <|> char '.'                *> return Dot
    <|> char '<'                *> return Lt
    <|> char '>'                *> return Gt
    <|> char '{'                *> return ListStart
    <|> char '}'                *> return ListEnd
    <|> try (Comment <$> (str "//" *> many1 (noneOf "\r\n") <* many1 (oneOf "\r\n")))
    <|> try (fmap MComment $ str "/*" *> manyTill (noneOf "_") (try (string "*/")))
    <|> try (str "algorithm"    *> return Algorithm)
    <|> try (str "case"         *> return Case)
    <|> try (str "constant"     *> return Constant)
    <|> try (str "encapsulated" *> return Encapsulated)
    <|> try (str "elseif"       *> return Elseif)
    <|> try (str "else"         *> return Else)
    <|> try (str "end"          *> return End)
    <|> try (str "function"     *> return Function)
    <|> try (str "if"           *> return If)
    <|> try (str "import"       *> return Import)
    <|> try (str "input"        *> return Input)
    <|> try (str "match"        *> return Match)
    <|> try (str "not"          *> return Not)
    <|> try (str "output"       *> return Output)
    <|> try (str "package"      *> return Package)
    <|> try (str "partial"      *> return Partial)
    <|> try (str "protected"    *> return Protected)
    <|> try (str "public"       *> return Public)
    <|> try (str "record"       *> return Record)
    <|> try (str "then"         *> return Then)
    <|> try (str "type"         *> return Type)
    <|> try (str "uniontype"    *> return Union)
    <|> Str <$> between (char '"') (char '"') (many $ noneOf "\"")
    <|> S <$> (try (string "and") <|> try (string "or") <|> many1 (oneOf "+&*+:=/-"))
    <|> W <$> many1 (letter <|> digit <|> char '_')
