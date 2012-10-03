{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE ScopedTypeVariables        #-}

module Parser where

import           Control.Applicative hiding (many)
import           Control.Monad
import           Control.Monad.Error
import           Control.Monad.State
import           Data.Maybe
import           Prelude             hiding (exp, pred)

import           Tokenizer           (Token)
import qualified Tokenizer           as T
import           Types

data ParseState = ParseState { parseAsts :: [AST], lastToken :: Maybe Token, parseTokens :: [Token] }
                deriving Show

parseState :: [Token] -> ParseState
parseState ts = ParseState [] Nothing ts

-- | Compile monad.
newtype Parse a = Parse { unCompile :: StateT ParseState (ErrorT ParseError IO) a }
  deriving ( MonadState ParseState
           , MonadError ParseError
           , MonadIO
           , Monad
           , Functor
           , Applicative)

type TParser a = Parser Token a

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

p_ast :: TParser AST
p_ast (T.Comment s) = return $ Comment s
p_ast (T.MComment s) = return $ MComment s
p_ast T.Package = p_package T.Package
p_ast T.Function = do
  name <- p_name =<< eat
  qs <- optionWith [] (== T.Lt) (eat >>= p_polytypes)
  doc <- p_docstr
  params <- many T.isInputOutput p_param
  tok' T.Algorithm
  stmts <- many (/= T.End) p_stmt
  tok' T.End
  void $ p_name =<< eat
  tok' T.Semi
  return $ Function name qs doc params stmts
p_ast T.Partial = do
  tok' T.Function
  name <- p_name =<< eat
  qs <- optionWith [] (== T.Lt) (eat >>= p_polytypes)
  doc <- p_docstr
  params <- many T.isInputOutput p_param
  tok' T.End
  void $ p_name =<< eat
  tok' T.Semi
  return $ PartFn name qs doc params
p_ast T.Union = do
  name <- p_name =<< eat
  doc <- p_docstr
  recs <- many (== T.Record) p_record
  tok' T.End
  void $ p_name =<< eat
  tok' T.Semi
  return $ Union name doc recs
p_ast T.Protected = do
  eat >>= p_ast >>= return . protectAst
p_ast T.Encapsulated = do
  eat >>= p_package >>= return . encapsulateAst
p_ast (T.W "replaceable") = do
  tok' T.Type
  name <- p_name =<< eat
  void $ t_w "subtypeof"
  void $ t_w "Any"
  tok' T.Semi
  return $ Replaceable name
p_ast T.Import = p_import T.Import
p_ast T.Type = do
  a <- p_name =<< eat
  void $ t_s "="
  b <- p_name =<< eat
  tok' T.Semi
  return $ TypeAlias a b
p_ast _ = throwErr $ UnsupportedAstToken

p_package :: TParser AST
p_package T.Package = do
  name <- p_name =<< eat
  doc <- p_docstr
  content <- p_top
  tok' T.End
  void $ p_name =<< eat
  tok' T.Semi
  return $ Package Unencapsulated name doc content
p_package _ = throwErr $ ExpectedTok [T.Package]

p_import :: TParser AST
p_import T.Import = do
  protection <- maybe Unprotected (const Protected) <$> tokM T.Protected
  name <- p_name =<< eat
  name' <- option (== T.S "=") (tok' (T.S "=") >> eat >>= p_name)
  imports <- option (== T.Dot) (tok' T.Dot >> eat >>= p_importVars)
  tok' T.Semi
  return $ Import protection name name' (maybe (Left Wild) id imports)
p_import _ = throwErr $ ExpectedTok [T.Import]

p_importVars :: TParser (Either Wild [Var])
p_importVars (T.S "*") = return $ Left Wild
p_importVars T.ListStart = Right <$> (p_importVarsList =<< eat)
p_importVars _ = throwErr $ ExpectedTok [T.S "*", T.ListStart]

p_importVarsList :: TParser [Var]
p_importVarsList T.ListEnd = return []
p_importVarsList w@(T.W _) = do
  var <- p_var w
  c <- tokM T.Comma
  vars <- case c of
    Just _ -> eat >>= p_importVarsList
    Nothing -> tok' T.ListEnd >> return []
  return $ var : vars
p_importVarsList _ = throwErr $ ExpectedTok [T.ListEnd, anyW]

p_polytypes :: TParser [Var]
p_polytypes T.Lt = eat >>= p_polytypes
p_polytypes T.Gt = return []
p_polytypes w@(T.W _) = do
  t <- p_var w
  ts <- eat >>= p_polytypes
  return (t:ts)
p_polytypes _ = throwErr $ ExpectedTok [T.Lt, T.Gt, anyW]

p_record :: TParser Record
p_record T.Record = do
  name <- p_name =<< eat
  vardecls <- many T.isW p_vardecl
  tok' T.End
  void $ p_name =<< eat
  tok' T.Semi
  return $ Record name vardecls
p_record _ = throwErr (ExpectedTok [T.Record])

p_param :: TParser Param
p_param T.Input = Input <$> (eat >>= p_vardecl)
p_param T.Output = Output <$> (eat >>= p_vardecl)
p_param _ = throwErr $ ExpectedTok [T.Input, T.Output]

p_stmt :: TParser Stmt
p_stmt l@(T.W lhs) =
  look >>= \s -> case s of
    Just (T.S ":=") -> do
      void $ eat
      exp <- p_exp =<< eat
      tok' T.Semi
      return (Assign [lhs] exp)
    _ -> do
      exp <- p_exp l
      tok' T.Semi
      return (StmtExp exp)
p_stmt T.ParenL = do
  lhs <- commaSep (p_var =<< eat) T.ParenR
  tok' T.ParenR
  tok' (T.S ":=")
  exp <- p_exp =<< eat
  tok' T.Semi
  return (Assign lhs exp)
p_stmt T.If = do
  iff <- p_if'
  eifs <- many (== T.Elseif) (const p_if')
  elsestmt <- option (== T.Else) (tok' T.Else >> many (/= T.End) p_stmt)
  tok' T.End >> tok' T.If >> tok' T.Semi
  return $ If (iff : eifs) elsestmt
p_stmt _ = throwErr $ ExpectedTok [anyW, T.ParenL, T.If]

p_if' :: Parse (Exp, [Stmt])
p_if' = do
  pred <- eat >>= p_exp
  tok' T.Then
  stmts <- many (\v -> v /= T.End && v /= T.Elseif && v /= T.Else) p_stmt
  return (pred, stmts)

commaSep :: Parse a -> Token -> Parse [a]
commaSep pel endt = do
  el <- pel
  hi <- lookIs endt
  if hi
    then return [el]
    else do
      tok' T.Comma
      els <- commaSep pel endt
      return $ el : els

p_exp :: TParser Exp
p_exp T.Match = do
  mvar <- p_var =<< eat
  cases <- many (== T.Case) p_match_case
  tok' T.End
  tok' T.Match
  return $ Match [mvar] cases
p_exp w@(T.W _) = do
  n <- p_name w
  s <- look
  case s of
    Just T.ParenL -> do
      tok' T.ParenL
      args <- option (not . (== T.ParenR)) (eat >>= p_expList)
      tok' T.ParenR
      return $ Funcall n (fromMaybe [] args)
    Just (T.S _) -> do
      op <- t_s'
      e <- p_exp =<< eat
      return $ InfixApp op (EVar n) e
    _ -> return $ EVar n
p_exp T.ParenL = do
  lookIs T.ParenR >>= \b -> if b
    then eat >> return Unit
    else do
      ts <- commaSep (eat >>= p_exp) T.ParenR
      tok' T.ParenR
      return (Tuple ts)
p_exp (T.S "-") = do
  e <- eat >>= p_exp
  return $ UnaryApp "-" e
p_exp T.If = do
  iff <- p_expif'
  eifs <- many (== T.Elseif) (const p_expif')
  tok' T.Else
  alt <- p_exp =<< eat
  return $ EIf (iff : eifs) alt
p_exp _ = throwErr $ ExpectedTok [T.Match, anyW, T.ParenL, T.S "-", T.If]

p_expif' :: Parse (Exp, Exp)
p_expif' = do
  pred <- eat >>= p_exp
  tok' T.Then
  exp <- eat >>= p_exp
  return (pred, exp)

p_expList :: TParser [Exp]
p_expList t = do
  e <- p_exp t
  comma <- tokM T.Comma
  es <- case comma of
    Just _ -> eat >>= p_expList
    Nothing -> return []
  return $ e:es

p_match_case :: TParser Case
p_match_case T.Case = do
  pat <- eat >>= p_pat
  tok' T.Then
  exp <- eat >>= p_exp
  tok' T.Semi
  return $ Case pat exp
p_match_case _ = throwErr $ ExpectedTok [T.Case]

p_pat :: TParser Pat
p_pat = p_exp

p_vardecl :: TParser VarDecl
p_vardecl n@(T.W _) = do
  typ <- p_type n
  var <- t_word
  tok' T.Semi
  return (typ, var)
p_vardecl _ = throwErr $ ExpectedTok [anyW]

p_type :: TParser Type
p_type w@(T.W _) = do
  n <- p_name w
  qs <- optionWith [] (== T.Lt) (eat >>= p_polytypes)
  return $ Type n qs
p_type _ = throwErr $ ExpectedTok [anyW]

p_var :: TParser Var
p_var (T.W s) = return s
p_var _ = throwErr $ ExpectedTok [anyW]

p_name :: TParser Name
p_name (T.W s) = do
  lookN 2 >>= \v -> case v of
    [T.Dot,T.W _] -> Qual <$> (tok' T.Dot >> eat >>= p_name) <*> return s
    _ -> return $ UnQual s
p_name _ = throwErr $ ExpectedTok [anyW]

p_docstr :: Parse (Maybe String)
p_docstr = option T.isStr t_str


-- Tokens

t_eof :: Parse ()
t_eof = eat >>= \s -> case s of
  T.EOF -> return ()
  _ -> throwErr $ ExpectedTok [T.EOF]

t_wild :: Parse ()
t_wild = tok' (T.W "*")

t_word :: Parse String
t_word = T.fromW <$> token (ExpectedTok [anyW]) T.isW

t_str :: Parse String
t_str = T.fromStr <$> token (ExpectedTok [anyStr]) T.isStr

t_s' :: Parse String
t_s' = eat >>= \s -> case s of
  T.S s' -> return s'
  _ -> throwErr $ ExpectedTok [anyS]

t_s :: String -> Parse String
t_s s = T.fromS <$> tok (T.S s)

t_w :: String -> Parse String
t_w s = T.fromW <$> tok (T.W s)

-- Error reporting helpers
anyW :: Token
anyW = T.W "<<any>>"

anyS :: Token
anyS = T.S "<<any>>"

anyStr :: Token
anyStr = T.S "<<any>>"

-- General parsing

type Parser t a = t -> Parse a

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

lookN :: Int -> Parse [Token]
lookN n = take n <$> gets parseTokens

lookIs :: TParser Bool
lookIs t = look >>= \s -> return (case s of
  Just x -> t == x
  Nothing -> False)

eat :: Parse Token
eat = do
  t:ts <- gets parseTokens
  modify (\s -> s { parseTokens = ts, lastToken = Just t })
  return t

skip :: Parse ()
skip = void eat

token :: PError -> (Token -> Bool) -> Parse Token
token err tokP = do
  s <- look
  case s of
    Just t | tokP t -> eat >> return t
    _ -> throwErr err

tok :: TParser Token
tok t = token (ExpectedTok [t]) (== t)

tok' :: TParser ()
tok' = void . tok

tokM :: TParser (Maybe Token)
tokM t = option (== t) (tok t)

many :: (Token -> Bool) -> (TParser a) -> Parse [a]
many pred p =
  look >>= \s -> case s of
    Just v | pred v -> eat >>= p >>= (\res -> (res :) <$> many pred p)
    _ -> return []

option :: (Token -> Bool) -> Parse a -> Parse (Maybe a)
option pred p =
  look >>= \s -> case s of
    Just v | pred v -> p >>= return . Just
    _ -> return Nothing

optionWith :: a -> (Token -> Bool) -> Parse a -> Parse a
optionWith def pred p = fromMaybe def <$> option pred p

-- Misc

pr :: (MonadIO m, Show a) => a -> m ()
pr s = liftIO $ print s

pr_state :: Parse ()
pr_state = gets id >>= pr
