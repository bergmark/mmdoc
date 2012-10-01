module ParserTest where

import           Types

parserExpected :: [(String, [AST])]
parserExpected = [
    "Package" `tup` [Package "Package" []]
  , "Function" `tup` [Package "Package" [Function "f" [] []]]
  , "FunctionArgs" `tup` [Package "Package" [Function "f" [Input ("Integer","x"),Input ("Integer","y"),Output ("Boolean","b1"),Output ("Boolean","b2")] []]]
  , "FunctionStatements" `tup` [Package "Package" [Function "f" [] [Assign "x" (EVar "y"),Assign "aoeu123" (EVar "aoeu123")]]]
  , "Match" `tup` [Package "Package" [Function "f" [] [Assign "x" (Match ["y"] [("z",EVar "w")])]]]
  , "Comment" `tup` [Comment " foo", Package "Package" [Comment " bar"]]
  , "UnionType" `tup` [Package "P" [Union "U" []]]
  , "UnionTypeRecord" `tup` [Union "U" [Record "R" [], Record "Tup" [("Integer","a"), ("String","b")]]]
  , "MComment" `tup` [MComment "\nhej\npackage Foo\n", Package "F" [MComment " end F; "], MComment "*\n * hej\n "]
  , "Import" `tup` [Package "I" [
                       Import Unprotected "X"    Nothing       (Left Wild)
                     , Import Protected   "Y"    Nothing       (Left Wild)
                     , Import Unprotected "L"    (Just "Long") (Left Wild)
                     , Import Protected   "Z"    Nothing       (Left Wild)
                    ]]
  , "ImportList" `tup` [Package "I" [
                         Import Unprotected "W"    Nothing       (Right [])
                       ]]
  ] where tup = (,)
