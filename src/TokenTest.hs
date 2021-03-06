module TokenTest where

import           Control.Arrow

import           Tokenizer

tokenExpected :: [(String, Program)]
tokenExpected = map (second (Program . (++ [EOF]))) [
    "Package" `tup` [Package, W "Package", End, W "Package", Semi]
  , "FunctionStatements" `tup` [Package, W "Package", Function, W "f", Algorithm, W "x", Assign, W "y", Semi, W "aoeu123", Assign, W "aoeu123", Semi, End, W "f", Semi, End, W "Package", Semi]
  , "UnionTypeRecord" `tup` [ Union, W "U"
                            ,   Record, W "R", End, W "R", Semi
                            ,   Record, W "Tup"
                            ,     W "Integer", W "a", Semi
                            ,     W "String", W "b", Semi
                            ,   End, W "Tup", Semi
                            , End, W "U", Semi]
  , "MComment" `tup` [ MComment "\nhej\npackage Foo\n"
                     , Package, W "F"
                     , MComment " end F; "
                     , End, W "F", Semi
                     , MComment "*\n * hej\n "]
  ] where tup = (,)
