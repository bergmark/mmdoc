module Parser where

import           Control.Applicative           hiding (many, (<|>))
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
  str "function"
  ws
  name <- p_name
  ws
  params <- many p_param
  str "algorithm"
  ws
  stmts <- many (try p_stmt)
  ws
  str "end"
  ws
  void $ p_name
  semi
  ws
  return $ Function name params stmts

p_param :: CharParser st Param
p_param = ws *> (p_param_input <|> p_param_output) <* ws

p_param_input :: CharParser st Param
p_param_input = do
  void $ string "input"
  ws
  t <- p_type
  ws
  n <- p_name
  ws
  semi
  return $ Input t n

p_param_output :: CharParser st Param
p_param_output = do
  void $ string "output"
  ws
  t <- p_type
  ws
  n <- p_name
  ws
  semi
  return $ Output t n

p_stmt :: CharParser st Stmt
p_stmt = ws *> p_assign <* semi <* ws

p_assign :: CharParser st Stmt
p_assign = do
  l <- p_name
  ws
  str ":="
  ws
  r <- p_name
  return $ Assign l r

p_type :: CharParser st Type
p_type = do
   u <- upper
   s <- many1 letter
   return $ u : s

p_name :: CharParser st Name
p_name = do
  l <- letter
  r <- many (letter <|> digit)
  return $ l : r

ws :: CharParser st ()
ws = void $ many (oneOf " \t\n")

semi :: CharParser st ()
semi = void $ char ';'

str :: String -> CharParser st ()
str = void . string
