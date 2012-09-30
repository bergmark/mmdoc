{-# LANGUAGE DeriveDataTypeable         #-}
{-# LANGUAGE FunctionalDependencies     #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE ScopedTypeVariables        #-}

module Parser where

import           Control.Applicative
-- import           Control.Arrow
import           Control.Monad
import           Control.Monad.Error
import           Control.Monad.State
import           Prelude             hiding (exp)

import           Tokenizer           (Token)
import qualified Tokenizer           as T
import           Types

data ParseState = ParseState { parseAsts :: [AST], parseTokens :: [Token] }
                deriving Show

parseState :: [Token] -> ParseState
parseState ts = ParseState [] ts


-- | Compile monad.
newtype Parse a = Parse { unCompile :: StateT ParseState (ErrorT ParseError IO) a }
  deriving ( MonadState ParseState
           , MonadError ParseError
           , MonadIO
           , Monad
           , Functor
           , Applicative)

data ParseError = ParseError PError ParseState
                  deriving Show

data PError = ExpectedAlgorithm
            | ExpectedEnd
            | ExpectedAssign
            | ExpectedCase
            | ExpectedInput
            | ExpectedInputOutput
            | ExpectedMatch
            | ExpectedSemi
            | ExpectedStmt
            | ExpectedThen
            | ExpectedWord
            | MissingEOF
            | UnsupportedAstToken
  deriving (Show)
instance Error ParseError

throwErr :: PError -> Parse a
throwErr perr = do
  s <- gets id
  throwError $ ParseError perr s

parse :: T.Program -> IO (Either ParseError [AST])
parse (T.Program ts) = do
  r :: Either ParseError ((), ParseState) <- runParse (parseState ts :: ParseState) parseTop
  return $ onEither id (parseAsts . snd) r

runParse :: ParseState -> Parse () -> IO (Either ParseError ((),ParseState))
runParse st m = runErrorT (runStateT (unCompile m) st)

addAst :: AST -> Parse ()
addAst ast = do
  modify $ \s -> s { parseAsts = ast : parseAsts s }

pu :: MonadIO m => String -> m ()
pu s = liftIO $ putStrLn s
pr :: (MonadIO m, Show a) => a -> m ()
pr s = liftIO $ print s

parseTop :: Parse ()
parseTop =
  look >>= maybe (return ()) (const $ p_top >>= mapM_ addAst . reverse)

p_top :: Parse [AST]
p_top = look >>= aux where
  aux :: Maybe Token -> Parse [AST]
  aux (Just t) | isAstStart t = do
    s <- eat
    ast <- p_ast s
    asts <- p_top
    return $ ast : asts
  aux _ = return []

p_ast :: Token -> Parse AST
p_ast (T.Comment s) = return $ Comment s
p_ast (T.MComment s) = return $ MComment s
p_ast T.Package = do
  name <- p_name
  content <- p_top
  t_end
  void p_name
  t_semi
  return $ Package name content
p_ast T.Function = do
  pr "function"
  name <- p_name
  params <- p_params
  t_algorithm
  stmts <- p_stmts
  t_end
  void p_name
  t_semi
  return $ Function name params stmts
p_ast T.Union = do
  pr "uniontype"
  name <- p_name
  recs <- p_records
  t_end
  void $ p_name
  t_semi
  return $ Union name recs
p_ast _ = throwErr $ UnsupportedAstToken

p_records :: Parse [Record]
p_records = undefined

t_algorithm :: Parse ()
t_algorithm = void $ tok ExpectedAlgorithm (== T.Algorithm)

p_params :: Parse [Param]
p_params =
  look >>= \s -> case s of
    Just s | T.isInputOutput s -> eat >>= p_param >>= \p -> (p :) <$> p_params
    _ -> return []

p_param :: Token -> Parse Param
p_param T.Input = Input <$> (eat >>= p_vardecl)
p_param T.Output = Output <$> (eat >>= p_vardecl)
p_param _ = throwErr ExpectedInputOutput

p_stmts :: Parse [Stmt]
p_stmts =
  look >>= \s -> case s of
    Just s | (s /= T.End) -> eat >>= p_stmt >>= (\stmt -> (stmt :) <$> p_stmts)
    _ -> return []

p_stmt :: Token -> Parse Stmt
p_stmt (T.W lhs) =
  eat >>= \s -> case s of
    T.W ":=" -> do
      t <- eat
      exp <- p_exp t
      t_semi
      return (Assign lhs exp)
    _ -> throwErr ExpectedAssign
p_stmt _ = throwErr ExpectedStmt

{-
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
-}

p_exp :: Token -> Parse Exp
p_exp T.Match = do
  mvar <- p_name
  cases <- p_match_cases
  t_end
  t_match
  return $ Match [mvar] cases
p_exp (T.W v) = do
  return $ EVar v
p_exp _ = throwErr ExpectedMatch

t_match :: Parse ()
t_match = void $ tok ExpectedMatch (== T.Match)

p_match_cases :: Parse [Case]
p_match_cases =
  look >>= \s -> case s of
    Just T.Case -> p_match_case >>= \c -> (c :) <$> p_match_cases
    Just _ -> return []

p_match_case :: Parse Case
p_match_case = do
  void $ tok ExpectedCase (== T.Case)
  pat <- p_pat
  void $ tok ExpectedThen (== T.Then)
  exp <- eat >>= p_exp
  t_semi
  return (pat, exp)

p_pat :: Parse Pat
p_pat = t_word

p_vardecl :: Token -> Parse VarDecl
p_vardecl (T.W typ) = do
  var <- p_name
  t_semi
  return (typ, var)
p_vardecl _ = throwErr ExpectedWord

p_name :: Parse Name
p_name = t_word

t_end :: Parse ()
t_end = void $ tok ExpectedEnd (== T.End)

t_word :: Parse String
t_word = T.fromW <$> tok ExpectedWord T.isW

t_semi :: Parse ()
t_semi = void $ tok ExpectedSemi (== T.Semi)

{-

p_ast (T.Union : T.W name : ts) = case p_records ts of
  (recs, T.End : T.W _name : T.Semi : ts') -> ([Union name recs], ts')
  (_, ts'') -> err "p_ast#4" ts''

p_records :: TokenParser [Record]
p_records (T.Record : T.W name : ts) = case p_vardecls ts of
  (vardecls, T.End : T.W _name : T.Semi : ts') -> first (Record name vardecls :) $ p_records ts'
  _ -> err "p_records" ts
p_records ts = ([], ts)

p_vardecls :: [Token] -> ([VarDecl], [Token])
p_vardecls ts = case p_vardecl ts of
  (Nothing, ts') -> ([], ts')
  (Just p,  ts') -> first (p :) $ p_vardecls ts'


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
-}

-- General parsing

look :: Parse (Maybe Token)
look = do
  s <- gets parseTokens
  case s of
    [T.EOF] -> return Nothing
    (t:_) -> return (Just t)
    _ -> throwErr MissingEOF

eat :: Parse Token
eat = do
  t:ts <- gets parseTokens
  modify (\s -> s { parseTokens = ts })
  return t

skip :: Parse ()
skip = void eat

tok :: PError -> (Token -> Bool) -> Parse Token
tok err tokP = do
  s <- look
  case s of
    Just t | tokP t -> eat >> return t
    _ -> throwErr err

-- Misc

onEither :: (a -> a') -> (b -> b') -> Either a b -> Either a' b'
onEither lf _ (Left l) = Left $ lf l
onEither _ rf (Right r) = Right $ rf r
