{-# LANGUAGE OverloadedStrings #-}

module WarnTest where

import           TestHelp ()
import qualified Types    as T
import           Warnings

warnExpected :: [(String,[Warning])]
warnExpected = [
    "Protection" `tup` [ Unencapsulated "P", MissingDocstring "P"
                       , Unprotected "I"
                       , Unprotected "f", MissingDocstring "f"
                       , Unprotected "F", MissingDocstring "F"]
  , "StmtExp" `tup` [StmtExp $ T.Funcall "f" []]
  ] where tup = (,)
