module WarnTest where

import           Warnings

warnExpected :: [(String,[Warning])]
warnExpected = [
    "Protection" `tup` [ Unencapsulated "P", MissingDocstring "P"
                       , Unprotected "I"
                       , Unprotected "f", MissingDocstring "f"
                       , Unprotected "F", MissingDocstring "F"]
  ] where tup = (,)
