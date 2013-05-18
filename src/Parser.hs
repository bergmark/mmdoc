{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TupleSections              #-}

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
p_ast (T.Comment  s) = return $ Comment s
p_ast (T.MComment s) = return $ MComment s
p_ast T.Package      = p_package T.Package
p_ast T.Partial      = ASTPartFn <$> p_partfn T.Partial
p_ast T.Protected    = protectAst Protected    <$> (p_ast =<< eat)
p_ast T.Public       = protectAst Public       <$> (p_ast =<< eat)
p_ast T.Encapsulated = protectAst Encapsulated <$> (p_package =<< eat)
p_ast T.Constant = do
  ty <- p_type =<< eat
  var <- p_var =<< eat
  tok' (T.S "=")
  exp <- p_exp =<< eat
  tok' T.Semi
  return $ Constant ty var exp
p_ast T.Function = do
  name <- p_name =<< eat
  qs <- optionWith (return []) (== T.S "<") (eat >>= p_polytypes)
  doc <- p_docstr
  params <- many T.isInputOutput p_param
  prots <- optionWith (return []) (== T.Protected) (eat >> p_funprots)
  tok' T.Algorithm
  stmts <- many (/= T.End) p_stmt
  tok' T.End
  void $ p_name =<< eat
  tok' T.Semi
  return $ Function Nothing name qs doc params prots stmts
p_ast T.Union = do
  name <- p_name =<< eat
  doc <- p_docstr
  recs <- many (== T.Record) p_record
  tok' T.End
  void $ p_name =<< eat
  tok' T.Semi
  return $ Union Nothing name doc recs
p_ast (T.W "replaceable") =
  Replaceable <$> (tok' T.Type *> (p_name =<< eat) <* t_w "subtypeof" <* t_w "Any" <* tok' T.Semi)
p_ast T.Type = do
  a <- p_name =<< eat
  void $ t_s "="
  b <- p_type =<< eat
  tok' T.Semi
  return $ TypeAlias a b
p_ast _ = throwErr $ UnsupportedAstToken

p_package :: TParser AST
p_package T.Package = do
  name <- p_name =<< eat
  ifM (lookIs $ T.S "=")
    (do
      tok' $ T.S "="
      name2 <- p_name =<< eat
      tok' $ T.ParenL
      redeclares <- sepBy T.Comma T.ParenR p_redeclare =<< eat
      tok' $ T.ParenR
      tok' $ T.Semi
      return $ PackageShort Nothing name name2 redeclares)
    (do
      doc <- p_docstr
      imports <- p_imports
      content <- p_top
      tok' T.End
      void $ p_name =<< eat
      tok' T.Semi
      return $ Package Nothing name doc imports content)
p_package _ = throwErr $ ExpectedTok [T.Package]

p_redeclare :: TParser Redeclare
p_redeclare T.Redeclare = do
  tok' T.Type
  name <- p_name =<< eat
  tok' $ T.S "="
  typ <- p_type =<< eat
  return $ Redeclare name typ
p_redeclare _ = throwErr $ ExpectedTok [T.Type]

p_imports :: Parse [Import]
p_imports = lookN 2 >>= \l -> case l of
  [T.Import,_] -> (:) <$> (eat >>= p_import) <*> p_imports
  [T.Protected,T.Import] -> do
    tok' T.Protected
    (:) . protectImport Protected <$> (eat >>= p_import) <*> p_imports
  [T.Public,T.Import] -> do
    tok' T.Public
    (:) . protectImport Public <$> (eat >>= p_import) <*> p_imports
  _ -> return []

p_import :: TParser Import
p_import T.Import = do
  name <- p_name =<< eat
  name' <- option (== T.S "=") (tok' (T.S "=") >> eat >>= p_name)
  imports <- option (== T.Dot) (tok' T.Dot >> eat >>= p_importVars)
  tok' T.Semi
  return $ Import Nothing name name' (maybe (Left Wild) id imports)
p_import _ = throwErr $ ExpectedTok [T.Import]

p_importVars :: TParser (Either Wild [Var])
p_importVars (T.S "*") = return $ Left Wild
p_importVars T.ListStart = Right <$> (p_importVarsList =<< eat)
p_importVars _ = throwErr $ ExpectedTok [T.S "*", T.ListStart]

p_importVarsList :: TParser [Var]
p_importVarsList T.ListEnd = return []
p_importVarsList w@(T.W _) =
  (:) <$> p_var w <*> (do
    c <- tokM T.Comma
    case c of
      Just _ -> eat >>= p_importVarsList
      Nothing -> tok' T.ListEnd >> return [])
p_importVarsList _ = throwErr $ ExpectedTok [T.ListEnd, anyW]

p_polytypes :: TParser [Name]
p_polytypes (T.S "<") = (commaSep (T.S ">") p_name =<< eat) <* tok' (T.S ">")
p_polytypes _ = throwErr $ ExpectedTok [T.S "<"]

p_partfn :: TParser PartFn
p_partfn T.Partial = do
  tok' T.Function
  name <- p_name =<< eat
  qs <- optionWith (return []) (== T.S "<") (eat >>= p_polytypes)
  doc <- p_docstr
  params <- many T.isInputOutput p_param
  tok' T.End
  void $ p_name =<< eat
  tok' T.Semi
  return $ PartFn Nothing name qs doc params
p_partfn _ = throwErr $ ExpectedTok [T.Partial]

p_record :: TParser Record
p_record T.Record = do
  name <- p_name =<< eat
  vardecls <- p_vardecls
  tok' T.End
  void $ p_name =<< eat
  tok' T.Semi
  return $ Record name vardecls
p_record _ = throwErr $ ExpectedTok [T.Record]

p_param :: TParser Param
p_param T.Input = Input   . head <$> (eat >>= p_vardecl)
p_param T.Output = Output . head <$> (eat >>= p_vardecl)
p_param _ = throwErr $ ExpectedTok [T.Input, T.Output]

p_stmt :: TParser Stmt
p_stmt (T.S "not") = StmtExp <$> p_exp (T.S "not") <* tok' T.Semi
p_stmt s@(T.Str _) = StmtExp <$> p_exp s <* tok' T.Semi
p_stmt T.ListStart = StmtExp <$> p_exp T.ListStart <* tok' T.Semi
p_stmt w@(T.W _) = p_stmtexp' w
p_stmt T.ParenL = p_stmtexp' T.ParenL
p_stmt T.If = do
  iff <- p_if' =<< eat
  eifs <- many (== T.Elseif) (const $ eat >>= p_if')
  elsestmt <- option (== T.Else) (tok' T.Else >> many (/= T.End) p_stmt)
  tok' T.End >> tok' T.If >> tok' T.Semi
  return $ If (iff : eifs) elsestmt
p_stmt T.For = do
  var <- p_var =<< eat
  tok' T.In
  e <- p_exp =<< eat
  tok' T.Loop
  stmts <- many (/= T.End) p_stmt
  tok' T.End
  tok' T.For
  tok' T.Semi
  return $ For var e stmts
p_stmt _ = throwErr $ ExpectedTok [anyW, T.ParenL, T.If, anyStr]

p_stmtexp' :: TParser Stmt
p_stmtexp' t = do
  exp1 <- p_exp t
  look >>= \s -> case s of
    Just (T.Assign) -> do
      void $ eat
      exp2 <- p_exp =<< eat
      tok' T.Semi
      return (Assign exp1 exp2)
    _ -> do
      tok' T.Semi
      return (StmtExp exp1)

commaSep :: Token -> TParser a -> TParser [a]
commaSep = sepBy T.Comma

semiEnd :: Token -> TParser a -> TParser [a]
semiEnd = endBy T.Semi

p_if' :: TParser (Exp, [Stmt])
p_if' t = do
  pred <- p_exp t
  tok' T.Then
  stmts <- many (\v -> v /= T.End && v /= T.Elseif && v /= T.Else) p_stmt
  return (pred, stmts)

p_exp :: TParser Exp
p_exp t = do
  e <- p_exp' t
  l <- look
  case l of
    Just (T.S _) -> p_infixApp e =<< eat
    _ -> return e
  where
    p_infixApp :: Exp -> TParser Exp
    p_infixApp e1 (T.S sym) = InfixApp sym e1 <$> (p_exp =<< eat)
    p_infixApp _ _ = throwErr $ ExpectedTok [anyS]

    p_exp' :: TParser Exp
    p_exp' T.Match = do
      mvars <- p_matchvars =<< eat
      locals <- optionWith (return []) (== T.Local) (tok' T.Local >> p_vardecls)
      cases <- many (== T.Case) p_match_case
      els <- option (== T.Else) $ do
        tok' T.Else
        eqs <- p_equation
        tokM' T.Then
        e <- p_exp =<< eat
        tok' T.Semi
        return $ MatchElse eqs e
      tok' T.End
      tok' T.Match
      return $ Match mvars locals cases els
        where
          p_matchvars :: TParser [Var]
          p_matchvars T.ParenL = -- commaSep T.ParenR p_var =<< eat
            ifM (lookIs T.ParenR)
              (eat >> return [])
              (do
                ts <- commaSep T.ParenR p_var =<< eat
                tok' T.ParenR
                return ts)
          p_matchvars t' = (:[]) <$> p_var t'
    p_exp' T.Matchcontinue = do
      mvars <- p_matchvars =<< eat
      locals <- optionWith (return []) (== T.Local) (tok' T.Local >> p_vardecls)
      cases <- many (== T.Case) p_match_case
      els <- option (== T.Else) $ do
        tok' T.Else
        eqs <- p_equation
        tokM' T.Then
        e <- p_exp =<< eat
        tok' T.Semi
        return $ MatchElse eqs e
      tok' T.End
      tok' T.Matchcontinue
      return $ Matchcontinue mvars locals cases els
        where
          p_matchvars :: TParser [Var]
          p_matchvars T.ParenL = -- commaSep T.ParenR p_var =<< eat
            ifM (lookIs T.ParenR)
              (eat >> return [])
              (do
                ts <- commaSep T.ParenR p_var =<< eat
                tok' T.ParenR
                return ts)
          p_matchvars t' = (:[]) <$> p_var t'
    p_exp' w@(T.W _) = do
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
    p_exp' T.ParenL = do
      ifM (lookIs T.ParenR)
        (eat >> return Unit)
        (Tuple <$> ((eat >>= commaSep T.ParenR p_exp) <* tok' T.ParenR))
    p_exp' T.If = do
      iff <- p_expif' =<< eat
      eifs <- many (== T.Elseif) (const $ eat >>= p_expif')
      tok' T.Else
      alt <- p_exp =<< eat
      return $ EIf (iff : eifs) alt
        where
          p_expif' :: TParser (Exp, Exp)
          p_expif' t' = do
            pred <- p_exp t'
            tok' T.Then
            exp <- eat >>= p_exp
            return (pred, exp)
    p_exp' (T.S "not") = UnaryApp "not" <$> (p_exp =<< eat)
    p_exp' (T.S "-") = UnaryApp "-" <$> (p_exp =<< eat)
    p_exp' (T.Str s) = return $ Str s
    p_exp' T.ListStart =
      List <$> optionWith (return []) (/= T.ListEnd) (commaSep T.ListEnd p_exp =<< eat) <* tok' T.ListEnd
    p_exp' _ = throwErr $ ExpectedTok [T.Match, anyW, T.ParenL, T.S "-", T.If, T.S "not", T.ListStart]

p_expList :: TParser [Exp]
p_expList t = (:) <$> p_exp t <*> optionWith (return []) (== T.Comma) (eat >> eat >>= p_expList)

p_match_case :: TParser Case
p_match_case T.Case = do
  pat <- eat >>= p_pat
  eqs <- p_equation
  tok' T.Then
  exp <- eat >>= p_exp
  tok' T.Semi
  return $ Case pat eqs exp
p_match_case _ = throwErr $ ExpectedTok [T.Case]

p_equation :: Parse [Exp]
p_equation = optionWith (return []) (== T.Equation) (tok' T.Equation >> eat >>= semiEnd T.Then p_exp)

p_pat :: TParser Pat
p_pat = p_exp

p_funprots :: Parse [FunProt]
p_funprots = do
  t <- look
  case t of
    Just T.Partial -> do
      fp <- eat >>= p_partfn
      fps <- p_funprots
      return $ FunProtPart fp : fps
    Just (T.W _) -> do
      vds <- eat >>= p_vardecl
      rest <- p_funprots
      return $ map FunProtVar vds ++ rest
    _ -> return []

p_vardecls :: Parse [VarDecl]
p_vardecls = concat <$> many T.isW p_vardecl

p_vardecl :: TParser [VarDecl]
p_vardecl n@(T.W _) = do
  typ <- p_type n
  vars <- commaSep T.Semi p_var =<< eat
  tok' T.Semi
  return $ map (typ,) vars
p_vardecl _ = throwErr $ ExpectedTok [anyW]

p_type :: TParser Type
p_type w@(T.W _) = do
  n <- p_name w
  qts <- optionWith (return []) (== T.S "<") ((tok' (T.S "<") >> eat >>= commaSep (T.S ">") p_type) <* tok' (T.S ">"))
  return $ Type n qts
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

-- Looks ahead and gets the first non-comment token
look :: Parse (Maybe Token)
look = do
  s <- dropWhile (\v -> T.isComment v || T.isMComment v) <$> gets parseTokens
  case s of
    [T.EOF] -> return Nothing
    (t:_) -> return (Just t)
    _ -> throwErr $ ExpectedTok [T.EOF]

lookN :: Int -> Parse [Token]
lookN n = take n . filter (\v -> not $ T.isComment v || T.isMComment v) <$> gets parseTokens

lookIs :: TParser Bool
lookIs t = look >>= \s -> return (case s of
  Just x -> t == x
  Nothing -> False)

-- Eats input and returns the first non comment
eat :: Parse Token
eat = do
  t:ts <- gets parseTokens
  modify (\s -> s { parseTokens = ts, lastToken = Just t })
  if T.isComment t || T.isMComment t
    then eat
    else return t

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

tokM' :: TParser ()
tokM' = void . tokM

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

optionWith :: Parse a -> (Token -> Bool) -> Parse a -> Parse a
optionWith def pred p = fromMaybe <$> def <*> option pred p

sepBy :: Token -> Token -> TParser a -> TParser [a]
sepBy sept endt pel t = do
  el <- pel t
  ifM (lookIs endt)
    (return [el])
    (do
      tok' sept
      els <- sepBy sept endt pel =<< eat
      return $ el : els)

endBy :: Token -> Token -> TParser a -> TParser [a]
endBy sept endt pel t = do
  el <- pel t
  tok' sept
  ifM (lookIs endt)
    (return [el])
    (do
      els <- endBy sept endt pel =<< eat
      return $ el : els)

ifM :: Monad m => m Bool -> m a -> m a -> m a
ifM pred exp alt = do
  p <- pred
  if p then exp else alt

-- Misc

pr :: (MonadIO m, Show a) => a -> m ()
pr s = liftIO $ print s

pr_state :: Parse ()
pr_state = gets id >>= pr
