module WarnTest where

import           Warnings

warnExpected :: [(String,[Warning])]
warnExpected = [
    "Protection" `tup` [ Unencapsulated "P"
                       , Unprotected "I"
                       , Unprotected "f"
                       , Unprotected "F"]
  ] where tup = (,)
