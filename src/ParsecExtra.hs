module ParsecExtra where

import           Control.Monad
import           Text.ParserCombinators.Parsec

ws1 :: CharParser st ()
ws1 = void $ many1 (oneOf " \t\r\n")

ws :: CharParser st ()
ws = void $ many (oneOf " \t\r\n")

semi :: CharParser st ()
semi = void $ char ';'

str :: String -> CharParser st ()
str = void . string
