{-# LANGUAGE ScopedTypeVariables #-}

module Parser where

import           Control.Arrow
import           Prelude       hiding (exp)

import           Tokenizer     (Token)
import qualified Tokenizer     as T
import           Types

type TokenParser b = [Token] -> (b, [Token])

parseFile :: T.Program -> [AST]
parseFile (T.Program ts) = fst $ p_top ([],ts)

p_top :: ([AST], [Token]) -> ([AST], [Token])
p_top t@(_,[]) = t
p_top (astels, tokens) = let (ast,ts) = p_ast tokens in p_top (astels ++ ast, ts)

p_ast :: [Token] -> ([AST], [Token])
p_ast [] = ([],[])
p_ast (T.Package : T.W name : xs) = case p_ast xs of
  (content, T.End : T.W _name : T.Semi : rest) -> ([Package name content], rest)
  _ -> err "p_ast#0" xs
p_ast (T.Comment s : ts) = ([Comment s], ts)
p_ast (T.MComment s : ts) = ([MComment s], ts)
p_ast (T.Function : T.W name : xs) = case p_fun_vardecls xs of
  (params :: [Param], T.Algorithm : xs') -> case p_stmts xs' of
    (stmts :: [Stmt], T.End : T.W _name : T.Semi : rest) -> ([Function name params stmts], rest)
    _ -> err "p_ast#2" xs'
  _ -> err "p_ast#1" xs
p_ast ts@(T.End : _) = ([], ts)
p_ast ts = err "p_ast#3" ts

err :: (Show s) => String -> s -> a
err s xs = error $ s ++ " tail not handled " ++ show xs ++ "\n"

p_fun_vardecls :: [Token] -> ([Param], [Token])
p_fun_vardecls ts = case p_fun_vardecl ts of
  (Nothing, ts') -> ([], ts')
  (Just p,  ts') -> first (p :) $ p_fun_vardecls ts'

p_fun_vardecl :: [Token] -> (Maybe Param, [Token])
p_fun_vardecl (T.W "input"  : T.W typ : T.W var : T.Semi : xs) = (Just $ Input  typ var, xs)
p_fun_vardecl (T.W "output" : T.W typ : T.W var : T.Semi : xs) = (Just $ Output typ var, xs)
p_fun_vardecl ts = (Nothing, ts)

p_stmts :: [Token] -> ([Stmt], [Token])
p_stmts ts = case p_stmt ts of
  (Nothing, ts') -> ([], ts')
  (Just s, ts') -> first (s :) $ p_stmts ts'

p_stmt :: [Token] -> (Maybe Stmt, [Token])
p_stmt (T.W lhs : T.W ":=" : xs) = case p_exp xs of
  (e, T.Semi : ts) -> (Just $ Assign lhs e, ts)
  _ -> err "p_stmt" xs
p_stmt ts = (Nothing, ts)

p_exp :: TokenParser Exp
p_exp ts@(T.W "match" : _) = p_match ts
p_exp (T.W var : ts) = (EVar var, ts)
p_exp ts = err "p_exp" ts

p_match :: TokenParser Exp
p_match (T.W "match" : T.W var : ts) = case p_match_cases ts of
   (cases, T.End : T.W "match" : ts') -> (Match [var] cases, ts')
   _ -> err "p_match#1" ts
p_match ts = err "p_match#2" ts

p_match_cases :: TokenParser [Case]
p_match_cases ts = case p_match_case ts of
  (Nothing, ts') -> ([], ts')
  (Just cse, ts') -> first (cse :) $ p_match_cases ts'

p_match_case :: TokenParser (Maybe Case)
p_match_case (T.W "case" : ts) = case p_pat ts of
  (pat, T.W "then" : ts') -> case p_exp ts' of
    (exp, T.Semi : ts'') -> (Just (pat,exp), ts'')
    _ -> err "p_match_case #1" ts'
  _ -> err "p_match_case #2" ts
p_match_case ts = (Nothing, ts)

p_pat :: TokenParser Pat
p_pat (T.W v : ts) = (v, ts)
p_pat ts = err "p_pat" ts

{-

p_exp :: CharParser st Exp
p_exp = try p_match <|> p_evar

p_match :: CharParser st Exp
p_match = do
  str "match"
  ws1
  v <- p_name -- TODO handle match (a,..)
  ws1
  cs <- many p_match_case
  ws1
  str "end"
  ws1
  str "match"
  return $ Match [v] cs

p_match_case :: CharParser st Case
p_match_case = do
  str "case"
  ws
  pat <- p_pat
  ws
  str "then"
  ws
  exp <- p_exp
  ws
  semi
  return (pat, exp)

p_pat :: CharParser st Pat
p_pat = PVar <$> p_name

p_evar :: CharParser st Exp
p_evar = EVar <$> p_name



p_union :: CharParser st AST
p_union = do
  str "uniontype"
  ws1
  t <- p_type
  ws1
  rs <- many (ws *> p_record <* ws)
  ws
  str "end"
  ws1
  void $ p_type
  semi
  return $ Union t rs

p_record :: CharParser st Record
p_record = do
  str "record"
  ws1
  n <- p_name
  ws
  decls <- many (p_vardecl <* ws)
  ws
  str "end"
  ws1
  void $ p_name
  ws
  semi
  return $ Record n decls

p_vardecl :: CharParser st VarDecl
p_vardecl = do
  t <- p_type
  ws1
  v <- p_name
  ws
  semi
  return (t,v)

p_package :: CharParser st AST
p_package = do
  str "package"
  ws
  name <- p_name
  ws
  fs <- p_top'
  ws
  void $ string "end"
  ws
  void $ p_name
  semi
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


p_stmt :: CharParser st Stmt
p_stmt = ws *> p_assign <* ws <* semi <* ws

p_assign :: CharParser st Stmt
p_assign = do
  l <- p_lhs
  ws
  str ":="
  ws
  r <- p_exp
  return $ Assign l r

p_lhs :: CharParser st LHS
p_lhs = LVar <$> p_name

p_type :: CharParser st Type
p_type = do
   u <- upper
   s <- many letter
   return $ u : s


p_name :: CharParser st Name
p_name = do
  l <- letter
  r <- many (letter <|> digit)
  return $ l : r
-}
