module Warnings where

import qualified Types as T

data Warning = Unencapsulated String
             | Unprotected String
             | MissingDocstring String
             | StmtExp T.Exp
  deriving (Eq, Ord, Show)
