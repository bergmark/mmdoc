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
           | Local
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
isComment (Comment _) = True
isComment _ = False
isMComment (MComment _) = True
isMComment _ = False
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
    <|> try (strSep "algorithm"    *> return Algorithm)
    <|> try (strSep "case"         *> return Case)
    <|> try (strSep "constant"     *> return Constant)
    <|> try (strSep "encapsulated" *> return Encapsulated)
    <|> try (strSep "elseif"       *> return Elseif)
    <|> try (strSep "else"         *> return Else)
    <|> try (strSep "end"          *> return End)
    <|> try (strSep "function"     *> return Function)
    <|> try (strSep "if"           *> return If)
    <|> try (strSep "import"       *> return Import)
    <|> try (strSep "input"        *> return Input)
    <|> try (strSep "local"        *> return Local)
    <|> try (strSep "match"        *> return Match)
    <|> try (strSep "not"          *> return Not)
    <|> try (strSep "output"       *> return Output)
    <|> try (strSep "package"      *> return Package)
    <|> try (strSep "partial"      *> return Partial)
    <|> try (strSep "protected"    *> return Protected)
    <|> try (strSep "public"       *> return Public)
    <|> try (strSep "record"       *> return Record)
    <|> try (strSep "then"         *> return Then)
    <|> try (strSep "type"         *> return Type)
    <|> try (strSep "uniontype"    *> return Union)
    <|> Str <$> between (char '"') (char '"')
          (concat <$> many (try (string "\\\"") <|> many1 (noneOf "\\\"")))
    <|> S <$> (try (string "and") <|> try (string "or") <|> many1 (oneOf "+&*+:=/-"))
    <|> W <$> many1 wordChar
  where
    wordChar = letter <|> digit <|> char '_'
    strSep s = str s <* notFollowedBy wordChar
