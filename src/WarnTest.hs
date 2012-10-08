{-# LANGUAGE OverloadedStrings #-}

module WarnTest where

import qualified Types    as T
import           Warnings

warnExpected :: [(String,[Warning])]
warnExpected = [
    "Protection" `tup` [ Unencapsulated "P", MissingDocstring "P"
                       , Unprotected "I"
                       , Unprotected "f", MissingDocstring "f"
                       , Unprotected "F", MissingDocstring "F"]
  , "StmtExp" `tup` [StmtExp $ T.Funcall "f" []]
  , "Nesting" `tup` [Unencapsulated "P", Unprotected "f", MissingDocstring "f"]
  ] where tup = (,)
