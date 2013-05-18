module Types where

import           Data.List.Split
import           Data.String
import           Prelude         hiding (exp)

import qualified Tokenizer       as T

data AST = Comment String  -- //
         | Constant Type Var Exp
         | Function (Maybe Protection) Name [Name] (Maybe DocString) [Param] [FunProt] [Stmt]
         | MComment String -- /* */
         | Package (Maybe Protection) Name (Maybe DocString) [Import] [AST]
         | PackageShort (Maybe Protection) Name Name [Redeclare]
         | ASTPartFn PartFn
         | Replaceable Name
         | TypeAlias Name Type
         | Union (Maybe Protection) Name (Maybe DocString) [Record]
  deriving (Eq, Show)

data Import = Import (Maybe Protection) Name (Maybe Name) (Either Wild [Var])
  deriving (Eq, Show)

data Redeclare = Redeclare Name Type
  deriving (Eq, Show)

isAstStart :: T.Token -> Bool
isAstStart T.Package = True
isAstStart (T.Comment _) = True
isAstStart T.Constant = True
isAstStart T.Function = True
isAstStart (T.MComment _) = True
isAstStart T.Union = True
isAstStart T.Import = True
isAstStart T.Protected = True
isAstStart T.Encapsulated = True
isAstStart T.Public = True
isAstStart T.Partial = True
isAstStart T.Type = True
isAstStart (T.W "replaceable") = True
isAstStart _ = False

protectAst :: Protection -> AST -> AST
protectAst p (Function _ a b c d e f) = Function (Just p) a b c d e f
protectAst p (Package _ a b c d) = Package (Just p) a b c d
protectAst p (ASTPartFn pf) = ASTPartFn $ protectPartFn p pf
protectAst p (Union _ a b c) = Union (Just p) a b c
protectAst _p ast = error $ "protectAst cannot protect " ++ show ast

protectImport :: Protection -> Import -> Import
protectImport p (Import _ a b c) = Import (Just p) a b c


protectPartFn :: Protection -> PartFn -> PartFn
protectPartFn p (PartFn _ a b c d) = PartFn (Just p) a b c d

data PartFn = PartFn (Maybe Protection) Name [Name] (Maybe DocString) [Param]
  deriving (Eq, Show)

data Protection = Protected | Public | Encapsulated
  deriving (Eq, Show)
data Partiality = Partial | Concrete
  deriving (Eq, Show)

data Wild = Wild
  deriving (Eq, Show)

data Param = Input VarDecl
           | Output VarDecl
  deriving (Eq, Show)

isInput :: Param -> Bool
isInput (Input {}) = True
isInput _ = False
isOutput :: Param -> Bool
isOutput (Output {}) = True
isOutput _ = False

data Stmt = Assign Exp Exp
          | StmtExp Exp
          | If [(Exp,[Stmt])] (Maybe [Stmt])
          | For Var Exp [Stmt]
  deriving (Eq, Ord, Show)

instance IsString Stmt where
  fromString = StmtExp . fromString

data Case = Case Pat [Exp] Exp
  deriving (Eq, Ord, Show)

data FunProt = FunProtVar VarDecl
             | FunProtPart PartFn
  deriving (Eq, Show)

data Exp = EIf [(Exp,Exp)] Exp
         | EVar Name
         | Match [Var] [VarDecl] [Case] (Maybe MatchElse)
         | Matchcontinue [Var] [VarDecl] [Case] (Maybe MatchElse)
         | Funcall Name [Exp]
         | InfixApp Op Exp Exp
         | UnaryApp Op Exp
         | Str String
         | Tuple [Exp]
         | Unit -- ()
         | List [Exp]
         -- TODO List
  deriving (Eq, Ord, Show)

instance IsString Exp where
  fromString = EVar . fromString

data MatchElse = MatchElse [Exp] Exp
  deriving (Eq, Ord, Show)

type Pat = Exp

data Record = Record Name [VarDecl]
  deriving (Eq, Show)

type VarDecl = (Type, Var)

type DocString = String
type Var = String
data Type = Type Name [Type]
  deriving (Eq, Ord, Show)
instance IsString Type where
  fromString s = Type (fromString s) []

type Op = String

data Name = UnQual String
          | Qual Name String
  deriving (Eq, Ord, Show)

instance IsString Name where
  fromString s = foldl Qual (UnQual x) xs
    where (x:xs) = reverse $ splitOn "." s
