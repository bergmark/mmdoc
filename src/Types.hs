module Types where

import           Prelude   hiding (exp)

import qualified Tokenizer as T

data AST = Comment String  -- //
         | Constant Type Var Exp
         | Function (Maybe Protection) Name [Var] (Maybe DocString) [Param] [FunProt] [Stmt]
         | Import (Maybe Protection) Name (Maybe Name) (Either Wild [Var])
         | MComment String -- /* */
         | Package (Maybe Protection) Name (Maybe DocString) [AST]
         | ASTPartFn PartFn
         | Replaceable Name
         | TypeAlias Name Name
         | Union Name (Maybe DocString) [Record]
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
protectAst p (Import _ a b c) = Import (Just p) a b c
protectAst p (Function _ a b c d e f) = Function (Just p) a b c d e f
protectAst p (Package _ a b c) = Package (Just p) a b c
protectAst p (ASTPartFn pf) = ASTPartFn $ protectPartFn p pf
protectAst _p ast = error $ "protectAst cannot protect " ++ show ast

protectPartFn :: Protection -> PartFn -> PartFn
protectPartFn p (PartFn _ a b c d) = PartFn (Just p) a b c d

data PartFn = PartFn (Maybe Protection) Name [Var] (Maybe DocString) [Param]
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

data Stmt = Assign [Var] Exp
          | StmtExp Exp
          | If [(Exp,[Stmt])] (Maybe [Stmt])
  deriving (Eq, Show)

data Case = Case Pat Exp
  deriving (Eq, Show)

data FunProt = FunProtVar VarDecl
             | FunProtPart PartFn
  deriving (Eq, Show)

data Exp = EIf [(Exp,Exp)] Exp
         | EVar Name
         | Match [Var] [Case]
         | Funcall Name [Exp]
         | InfixApp Op Exp Exp
         | UnaryApp Op Exp
         | Tuple [Exp]
         | Unit -- ()
         -- TODO List
  deriving (Eq, Show)

type Pat = Exp

data Record = Record Name [VarDecl]
  deriving (Eq, Show)

type VarDecl = (Type, Var)

type DocString = String
type Var = String
data Type = Type Name [Var]
  deriving (Eq, Show)
type Op = String

data Name = UnQual String
          | Qual Name String
  deriving (Eq, Show)
