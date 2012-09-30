module TokenTest where

import           Tokenizer

tokenExpected :: [(String, [Token])]
tokenExpected = [
    "Package" `tup` [Package, W "Package", End, W "Package", Semi]
  , "FunctionStatements" `tup` [Package, W "Package", Function, W "f", Algorithm, W "x", W ":=", W "y", Semi, W "aoeu123", W ":=", W "aoeu123", Semi, End, W "f", Semi, End, W "Package", Semi]
--  , "Match" `tup` [Package "Package" [Function "f" [] [Assign (LVar "x") (Match ["y"] [(PVar "z",EVar "w")])]]]
--  , "Comment" `tup` [Comment " foo", Package "Package" [Comment " bar"]]
--  , "UnionType" `tup` [Package "P" [Union "U" []]]
  , "UnionTypeRecord" `tup` [ Union, W "U"
                            ,   Record, W "R", End, W "R", Semi
                            ,   Record, W "Tup"
                            ,     W "Integer", W "a", Semi
                            ,     W "String", W "b", Semi
                            ,   End, W "Tup", Semi
                            , End, W "U", Semi]
  ] where tup = (,)
