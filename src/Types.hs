module Types where

import           Prelude   hiding (exp)

import qualified Tokenizer as T

data AST = Package Name [AST]
         | Function Name [Param] [Stmt]
         | Comment String  -- //
         | MComment String -- /* */
         | Union Name [Record]
         deriving (Eq, Show)

isAstStart :: T.Token -> Bool
isAstStart T.Package = True
isAstStart (T.Comment _) = True
isAstStart T.Function = True
isAstStart (T.MComment _) = True
isAstStart T.Union = True
isAstStart _ = False

data Param = Input VarDecl
           | Output VarDecl
             deriving (Eq, Show)

data Stmt = Assign Var Exp
            deriving (Eq, Show)

type Case = (Pat, Exp)

data Exp = EVar Name
         | Match [Var] [Case]
         deriving (Eq, Show)

type Pat = Var

data Record = Record Name [VarDecl]
            deriving (Eq, Show)

type VarDecl = (Type, Var)

type Var = String
type Name = String
type Type = String
