module Types where

type Name = String

data AST = Package Name [AST]
         | Function Name
         deriving Show

