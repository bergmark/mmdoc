{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE ScopedTypeVariables        #-}

module Parser where

import           Control.Applicative hiding (many)
import           Control.Monad
import           Control.Monad.Error
import           Control.Monad.State
import           Prelude             hiding (exp, pred)

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

data PError = ExpectedTok [T.Token]
            | UnsupportedAstToken
  deriving (Show)
instance Error ParseError

parse :: T.Program -> IO (Either ParseError [AST])
parse (T.Program ts) = do
  r :: Either ParseError ((), ParseState) <- runParse (parseState ts :: ParseState) parseTop
  return $ either (Left . id) (Right. parseAsts . snd) r

runParse :: ParseState -> Parse () -> IO (Either ParseError ((),ParseState))
runParse st m = runErrorT (runStateT (unCompile m) st)

addAst :: AST -> Parse ()
addAst ast = do
  modify $ \s -> s { parseAsts = ast : parseAsts s }

parseTop :: Parse ()
parseTop = do
  look >>= maybe (return ()) (const $ p_top >>= mapM_ addAst . reverse)
  t_eof

p_top :: Parse [AST]
p_top = many isAstStart p_ast

p_ast :: Token -> Parse AST
p_ast (T.Comment s) = return $ Comment s
p_ast (T.MComment s) = return $ MComment s
p_ast T.Package = p_package T.Package
p_ast T.Function = do
  name <- p_name
  params <- many T.isInputOutput p_param
  t_algorithm
  stmts <- many (/= T.End) p_stmt
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
p_ast T.Protected = do
  eat >>= p_ast >>= return . protectAst
p_ast T.Encapsulated = do
  eat >>= p_package >>= return . encapsulateAst
p_ast T.Import = do
  protection <- maybe Unprotected (const Protected) <$> option (== T.Protected) t_protected
  name <- p_name
  name' <- option (== T.W "=") (tok (ExpectedTok [T.W "="]) (== T.W "=") >> p_name)
  imports <- option (== T.Dot) (t_dot >> eat >>= p_importVars)
  t_semi
  return $ Import protection name name' (maybe (Left Wild) id imports)
p_ast _ = throwErr $ UnsupportedAstToken

p_package :: Token -> Parse AST
p_package T.Package = do
  name <- p_name
  content <- p_top
  t_end
  void p_name
  t_semi
  return $ Package Unencapsulated name content
p_package _ = throwErr $ ExpectedTok [T.Package]

p_importVars :: T.Token -> Parse (Either Wild [Name])
p_importVars (T.W "*") = return $ Left Wild
p_importVars T.ListStart = do
  l <- p_list T.ListStart
  return $ Right l
p_importVars _ = throwErr $ ExpectedTok [T.W "*", T.ListStart]

p_list :: T.Token -> Parse [Name]
p_list T.ListStart = look >>= p_listContents
p_list _ = throwErr $ ExpectedTok [T.ListStart]

p_listContents :: Maybe T.Token -> Parse [Name]
p_listContents (Just T.ListEnd) = t_listEnd >> return []
p_listContents (Just _) = do
  el <- p_name
  void $ option (== T.Comma) t_comma
  look >>= p_listContents >>= return . (el :)
p_listContents Nothing = error "p_listContents unreachable"

p_records :: Parse [Record]
p_records =
  look >>= \s -> case s of
    Just T.Record -> eat >>= p_record >>= (\r -> (r :) <$> p_records)
    _ -> return []

p_record :: Token -> Parse Record
p_record T.Record = do
  name <- p_name
  vardecls <- many T.isW p_vardecl
  t_end
  void $ p_name
  t_semi
  return $ Record name vardecls
p_record _ = throwErr (ExpectedTok [T.Record])

p_param :: Token -> Parse Param
p_param T.Input = Input <$> (eat >>= p_vardecl)
p_param T.Output = Output <$> (eat >>= p_vardecl)
p_param _ = throwErr $ ExpectedTok [T.Input, T.Output]

p_stmt :: Token -> Parse Stmt
p_stmt (T.W lhs) =
  eat >>= \s -> case s of
    T.W ":=" -> do
      t <- eat
      exp <- p_exp t
      t_semi
      return (Assign lhs exp)
    _ -> throwErr $ ExpectedTok [T.W ":="]
p_stmt _ = throwErr $ ExpectedTok [T.W "<<any>>"]

p_exp :: Token -> Parse Exp
p_exp T.Match = do
  mvar <- p_name
  cases <- many (== T.Case) p_match_case
  t_end
  t_match
  return $ Match [mvar] cases
p_exp (T.W v) = return $ EVar v
p_exp _ = throwErr $ ExpectedTok [T.Match]

p_match_case :: Token -> Parse Case
p_match_case T.Case = do
  pat <- p_pat
  void $ tok (ExpectedTok [T.Then]) (== T.Then)
  exp <- eat >>= p_exp
  t_semi
  return (pat, exp)
p_match_case _ = throwErr $ ExpectedTok [T.Case]

p_pat :: Parse Pat
p_pat = t_word

p_vardecl :: Token -> Parse VarDecl
p_vardecl (T.W typ) = do
  var <- p_name
  t_semi
  return (typ, var)
p_vardecl _ = throwErr $ ExpectedTok [T.W "<<any>>"]

p_name :: Parse Name
p_name = t_word

-- Tokens

t_eof :: Parse ()
t_eof = eat >>= \s -> case s of
  T.EOF -> return ()
  _ -> throwErr $ ExpectedTok [T.EOF]

t_comma :: Parse ()
t_comma = void $ tok (ExpectedTok [T.Comma]) (== T.Comma)

t_listEnd :: Parse ()
t_listEnd = void $ tok (ExpectedTok [T.ListEnd]) (== T.ListEnd)

t_dot :: Parse ()
t_dot = void $ tok (ExpectedTok [T.Dot]) (== T.Dot)

t_wild :: Parse ()
t_wild = void $ tok (ExpectedTok [T.W "*"]) (== T.W "*")

t_protected :: Parse ()
t_protected = void $ tok (ExpectedTok [T.Protected]) (== T.Protected)

t_match :: Parse ()
t_match = void $ tok (ExpectedTok [T.Match]) (== T.Match)

t_end :: Parse ()
t_end = void $ tok (ExpectedTok [T.End]) (== T.End)

t_word :: Parse String
t_word = T.fromW <$> tok (ExpectedTok [T.W "<<any>>"]) T.isW

t_semi :: Parse ()
t_semi = void $ tok (ExpectedTok [T.Semi]) (== T.Semi)

t_algorithm :: Parse ()
t_algorithm = void $ tok (ExpectedTok [T.Algorithm]) (== T.Algorithm)

-- General parsing

throwErr :: PError -> Parse a
throwErr perr = do
  s <- gets id
  throwError $ ParseError perr s

look :: Parse (Maybe Token)
look = do
  s <- gets parseTokens
  case s of
    [T.EOF] -> return Nothing
    (t:_) -> return (Just t)
    _ -> throwErr $ ExpectedTok [T.EOF]

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

many :: (Token -> Bool) -> (Token -> Parse a) -> Parse [a]
many pred p =
  look >>= \s -> case s of
    Just v | pred v -> eat >>= p >>= (\res -> (res :) <$> many pred p)
    _ -> return []

option :: (Token -> Bool) -> Parse a -> Parse (Maybe a)
option pred p =
  look >>= \s -> case s of
    Just v | pred v -> p >>= return . Just
    _ -> return Nothing

-- Misc

pr :: (MonadIO m, Show a) => a -> m ()
pr s = liftIO $ print s
