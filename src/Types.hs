module Types where

data AST = Package Name [AST]
         | Function Name [Param]
         deriving Show

data Param = Input Type Name
           | Output Type Name
             deriving Show

type Name = String
type Type = String
