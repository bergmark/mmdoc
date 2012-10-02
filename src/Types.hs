module Types where

import           Prelude   hiding (exp)

import qualified Tokenizer as T

data AST = Package Encapsulation Name (Maybe DocString) [AST]
         | Function Name [Type] (Maybe DocString) [Param] [Stmt]
         | Comment String  -- //
         | MComment String -- /* */
         | PartFn Name [Type] (Maybe DocString) [Param]
         | Union Name (Maybe DocString) [Record]
         | Import Protection Name (Maybe Name) (Either Wild [Name])
         | Replaceable Name
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

data Stmt = Assign Var Exp
          | StmtExp Exp
  deriving (Eq, Show)

type Case = (Pat, Exp)

data Exp = EVar Name
         | Match [Var] [Case]
         | Funcall Name [Exp]
  deriving (Eq, Show)

type Pat = Var

data Record = Record Name [VarDecl]
  deriving (Eq, Show)

type VarDecl = (Type, Var)

type DocString = String
type Var = String
data Type = Type Name [Name]
  deriving (Eq, Show)

type Name = String
