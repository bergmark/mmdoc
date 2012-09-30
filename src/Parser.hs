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
            | ExpectedRecord
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
  name <- p_name
  params <- p_params
  t_algorithm
  stmts <- p_stmts
  t_end
  void p_name
  t_semi
  return $ Function name params stmts
p_ast T.Union = do
  name <- p_name
  recs <- p_records
  t_end
  void $ p_name
  t_semi
  return $ Union name recs
p_ast _ = throwErr $ UnsupportedAstToken

p_records :: Parse [Record]
p_records =
  look >>= \s -> case s of
    Just T.Record -> eat >>= p_record >>= (\r -> (r :) <$> p_records)
    _ -> return []

p_record :: Token -> Parse Record
p_record T.Record = do
  name <- p_name
  vardecls <- p_vardecls
  t_end
  void $ p_name
  t_semi
  return $ Record name vardecls
p_record _ = throwErr ExpectedRecord

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

p_vardecls :: Parse [VarDecl]
p_vardecls =
  look >>= \s -> case s of
    Just (T.W _) -> eat >>= p_vardecl >>= \vd -> (vd :) <$> p_vardecls
    Just _ -> return []

p_vardecl :: Token -> Parse VarDecl
p_vardecl (T.W typ) = do
  var <- p_name
  t_semi
  return (typ, var)
p_vardecl _ = throwErr ExpectedWord

p_name :: Parse Name
p_name = t_word

-- Tokens

t_match :: Parse ()
t_match = void $ tok ExpectedMatch (== T.Match)

t_end :: Parse ()
t_end = void $ tok ExpectedEnd (== T.End)

t_word :: Parse String
t_word = T.fromW <$> tok ExpectedWord T.isW

t_semi :: Parse ()
t_semi = void $ tok ExpectedSemi (== T.Semi)

t_algorithm :: Parse ()
t_algorithm = void $ tok ExpectedAlgorithm (== T.Algorithm)

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
