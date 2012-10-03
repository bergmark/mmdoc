module Warnings where

data Warning = Unencapsulated String
             | Unprotected String
  deriving (Eq, Show)
