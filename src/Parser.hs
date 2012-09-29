module Parser where

import           Control.Applicative           hiding (many)
import           Control.Monad
import           Text.ParserCombinators.Parsec

import           Types

parseFile :: String -> Either ParseError AST
parseFile = parse p_top "(unknown)"

p_top :: CharParser st AST
p_top = ws *> p_pkg <* ws


p_pkg :: CharParser st AST
p_pkg = do
  ws
  void $ string "package"
  ws
  name <- p_name
  ws
  fs <- many p_function
  ws
  void $ string "end"
  ws
  void $ p_name
  semi
  ws
  return $ Package name fs

p_function :: CharParser st AST
p_function = do
  ws
  void $ string "function"
  ws
  name <- p_name
  ws
  void $ string "end"
  ws
  void $ p_name
  semi
  ws
  return $ Function name


p_name :: CharParser st Name
p_name = many1 letter

ws :: CharParser st ()
ws = void $ many (oneOf " \t\n")

semi :: CharParser st ()
semi = void $ char ';'
