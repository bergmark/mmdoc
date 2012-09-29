module Types where

data AST = Package Name [AST]
         | Function Name [Param] [Stmt]
         deriving Show

data Param = Input Type Name
           | Output Type Name
             deriving Show

data Stmt = Assign Name Name
            deriving Show

data LHS = LVar Name
         deriving Show
data RHS = RVar Name
         deriving Show

type Name = String
type Type = String
