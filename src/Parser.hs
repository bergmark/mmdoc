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

data PError = ExpectedTok [Token]
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
  qs <- p_polytypes
  doc <- p_docstr
  params <- many T.isInputOutput p_param
  t_algorithm
  stmts <- many (/= T.End) p_stmt
  t_end
  void p_name
  t_semi
  return $ Function name qs doc params stmts
p_ast T.Partial = do
  t_function
  name <- p_name
  qs <- p_polytypes
  doc <- p_docstr
  params <- many T.isInputOutput p_param
  t_end
  void p_name
  t_semi
  return $ PartFn name qs doc params
p_ast T.Union = do
  name <- p_name
  doc <- p_docstr
  recs <- many (== T.Record) p_record
  t_end
  void $ p_name
  t_semi
  return $ Union name doc recs
p_ast T.Protected = do
  eat >>= p_ast >>= return . protectAst
p_ast T.Encapsulated = do
  eat >>= p_package >>= return . encapsulateAst
p_ast (T.W "replaceable") = do
  void $ t_w "type"
  name <- p_name
  void $ t_w "subtypeof"
  void $ t_w "Any"
  t_semi
  return $ Replaceable name
p_ast T.Import = do
  protection <- maybe Unprotected (const Protected) <$> option (== T.Protected) t_protected
  name <- p_name
  name' <- option (== T.W "=") (token (ExpectedTok [T.W "="]) (== T.W "=") >> p_name)
  imports <- option (== T.Dot) (t_dot >> eat >>= p_importVars)
  t_semi
  return $ Import protection name name' (maybe (Left Wild) id imports)
p_ast _ = throwErr $ UnsupportedAstToken

p_package :: Token -> Parse AST
p_package T.Package = do
  name <- p_name
  doc <- p_docstr
  content <- p_top
  t_end
  void p_name
  t_semi
  return $ Package Unencapsulated name doc content
p_package _ = throwErr $ ExpectedTok [T.Package]

p_importVars :: Token -> Parse (Either Wild [Name])
p_importVars (T.W "*") = return $ Left Wild
p_importVars T.ListStart = do
  l <- p_list T.ListStart
  return $ Right l
p_importVars _ = throwErr $ ExpectedTok [T.W "*", T.ListStart]

p_list :: Token -> Parse [Name]
p_list T.ListStart = look >>= p_listContents
p_list _ = throwErr $ ExpectedTok [T.ListStart]

p_listContents :: Maybe Token -> Parse [Name]
p_listContents (Just T.ListEnd) = t_listEnd >> return []
p_listContents (Just _) = do
  el <- p_name
  void $ option (== T.Comma) t_comma
  look >>= p_listContents >>= return . (el :)
p_listContents Nothing = error "p_listContents unreachable"

p_polytypes :: Parse [Type]
p_polytypes =
  look >>= \s -> case s of
    Just T.Lt -> eat >> p_polytypes' >>= (\ts -> tok' (T.Gt) >> return ts)
    _ -> return []
  where
    p_polytypes' :: Parse [Type]
    p_polytypes' =
      look >>= \s -> case s of
        Just T.Gt -> return []
        Just (T.W _) -> eat >>= p_type >>= \t -> (p_polytypes' >>= \ts -> return (t:ts))
        _ -> throwErr $ ExpectedTok [T.Gt, T.W "<<type>>"]

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
p_stmt l@(T.W lhs) =
  look >>= \s -> case s of
    Just (T.W ":=") -> do
      void $ eat
      exp <- p_exp =<< eat
      t_semi
      return (Assign lhs exp)
    _ -> do
      exp <- p_exp l
      t_semi
      return (StmtExp exp)
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
  tok' T.Then
  exp <- eat >>= p_exp
  t_semi
  return (pat, exp)
p_match_case _ = throwErr $ ExpectedTok [T.Case]

p_pat :: Parse Pat
p_pat = t_word

p_vardecl :: Token -> Parse VarDecl
p_vardecl n@(T.W _) = do
  typ <- p_type n
  var <- p_name
  t_semi
  return (typ, var)
p_vardecl _ = throwErr $ ExpectedTok [T.W "<<any>>"]

p_type :: Token -> Parse Type
p_type (T.W t) = do
  qs <- (look >>= \s -> case s of
    Just T.Lt -> eat >> p_polytype' >>= (\ts -> tok' T.Gt >> return ts)
    _ -> return [])
  return $ Type t qs
  where
    p_polytype' :: Parse [Name]
    p_polytype' =
      look >>= \s -> case s of
        Just T.Gt -> return []
        Just (T.W _) -> p_name >>= (\n -> p_polytype' >>= \ns -> return (n:ns))
        _ -> throwErr $ ExpectedTok [T.Gt, T.W "<<any>>"]
p_type _ = throwErr $ ExpectedTok [T.W "<<any>>"]

p_name :: Parse Name
p_name = t_word

p_docstr :: Parse (Maybe String)
p_docstr = option (T.isStr) t_str


-- Tokens

t_eof :: Parse ()
t_eof = eat >>= \s -> case s of
  T.EOF -> return ()
  _ -> throwErr $ ExpectedTok [T.EOF]

t_comma :: Parse ()
t_comma = tok' T.Comma

t_listEnd :: Parse ()
t_listEnd = tok' T.ListEnd

t_dot :: Parse ()
t_dot = tok' T.Dot

t_wild :: Parse ()
t_wild = tok' (T.W "*")

t_protected :: Parse ()
t_protected = tok' T.Protected

t_match :: Parse ()
t_match = tok' T.Match

t_end :: Parse ()
t_end = tok' T.End

t_word :: Parse String
t_word = T.fromW <$> token (ExpectedTok [T.W "<<any>>"]) T.isW

t_str :: Parse String
t_str = T.fromStr <$> token (ExpectedTok [T.Str "<<any>>"]) T.isStr

t_w :: String -> Parse String
t_w s = T.fromW <$> tok (T.W s)

t_semi :: Parse ()
t_semi = tok' T.Semi

t_algorithm :: Parse ()
t_algorithm = tok' T.Algorithm

t_function :: Parse ()
t_function = tok' T.Function


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

token :: PError -> (Token -> Bool) -> Parse Token
token err tokP = do
  s <- look
  case s of
    Just t | tokP t -> eat >> return t
    _ -> throwErr err

tok :: Token -> Parse Token
tok t = token (ExpectedTok [t]) (== t)

tok' :: Token -> Parse ()
tok' = void . tok

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
