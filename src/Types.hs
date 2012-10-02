module Types where

import           Prelude   hiding (exp)

import qualified Tokenizer as T

data AST =
           Comment String  -- //
         | Function Name [Type] (Maybe DocString) [Param] [Stmt]
         | Import Protection Name (Maybe Name) (Either Wild [Name])
         | MComment String -- /* */
         | Package Encapsulation Name (Maybe DocString) [AST]
         | PartFn Name [Type] (Maybe DocString) [Param]
         | Replaceable Name
         | TypeAlias Name Name
         | Union Name (Maybe DocString) [Record]
  deriving (Eq, Show)

isAstStart :: T.Token -> Bool
isAstStart T.Package = True
isAstStart (T.Comment _) = True
isAstStart T.Function = True
isAstStart (T.MComment _) = True
isAstStart T.Union = True
isAstStart T.Import = True
isAstStart T.Protected = True
isAstStart T.Encapsulated = True
isAstStart T.Partial = True
isAstStart T.Type = True
isAstStart (T.W "replaceable") = True
isAstStart _ = False

protectAst :: AST -> AST
protectAst (Import _ a b c) = Import Protected a b c
protectAst ast = error $ "protectAst cannot protect " ++ show ast

encapsulateAst :: AST -> AST
encapsulateAst (Package _ d a b) = Package Encapsulated d a b
encapsulateAst ast = error $ "encapsulateAst cannot encapsulate " ++ show ast

data Protection = Protected | Unprotected
  deriving (Eq, Show)
data Encapsulation = Encapsulated | Unencapsulated
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
          | If Exp [Stmt]
  deriving (Eq, Show)

data Case = Case Pat Exp
  deriving (Eq, Show)

data Exp = EVar Name
         | Match [Var] [Case]
         | Funcall Name [Exp]
         | InfixApp Op Exp Exp
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
data Type = Type Name [Name]
  deriving (Eq, Show)
type Op = String

type Name = String
