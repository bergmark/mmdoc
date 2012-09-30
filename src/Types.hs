module Types where

import           Prelude hiding (exp)

data AST = Package Name [AST]
         | Function Name [Param] [Stmt]
         deriving Show

data Param = Input Type Name
           | Output Type Name
             deriving Show

data Stmt = Assign LHS Exp
            deriving Show

type Case = (Pat, Exp)

data LHS = LVar Name
         deriving Show
data Exp = EVar Name
         | Match [Var] [Case]
         deriving Show

data Pat = PVar Var
           deriving Show

type Var = String
type Name = String
type Type = String
