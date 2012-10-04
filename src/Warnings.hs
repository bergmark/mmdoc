module Warnings where

data Warning = Unencapsulated String
             | Unprotected String
             | MissingDocstring String
  deriving (Eq, Show)
