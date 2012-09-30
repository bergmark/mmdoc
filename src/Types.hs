module Types where

import           Prelude hiding (exp)

data AST = Package Name [AST]
         | Function Name [Param] [Stmt]
         | Comment String
         deriving (Eq, Show)

data Param = Input Type Name
           | Output Type Name
             deriving (Eq, Show)

data Stmt = Assign LHS Exp
            deriving (Eq, Show)

type Case = (Pat, Exp)

data LHS = LVar Name
         deriving (Eq, Show)
data Exp = EVar Name
         | Match [Var] [Case]
         deriving (Eq, Show)

data Pat = PVar Var
           deriving (Eq, Show)

type Var = String
type Name = String
type Type = String
