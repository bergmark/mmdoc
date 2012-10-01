module ParserTest where

import           Types

parserExpected :: [(String, [AST])]
parserExpected = [
    "Package" `tup` [Package Unencapsulated "Package" Nothing []]
  , "Function" `tup` [Package Unencapsulated "Package" Nothing [Function "f" Nothing [] []]]
  , "FunctionArgs" `tup` [Package Unencapsulated "Package" Nothing [Function "f" Nothing [Input ("Integer","x"),Input ("Integer","y"),Output ("Boolean","b1"),Output ("Boolean","b2")] []]]
  , "FunctionStatements" `tup` [Package Unencapsulated "Package" Nothing [Function "f" Nothing [] [Assign "x" (EVar "y"),Assign "aoeu123" (EVar "aoeu123")]]]
  , "Match" `tup` [Package Unencapsulated "Package" Nothing [Function "f" Nothing [] [Assign "x" (Match ["y"] [("z",EVar "w")])]]]
  , "Comment" `tup` [Comment " foo", Package Unencapsulated "Package" Nothing [Comment " bar"]]
  , "UnionType" `tup` [Package Unencapsulated "P" Nothing [Union "U" Nothing []]]
  , "UnionTypeRecord" `tup` [Union "U" Nothing [Record "R" [], Record "Tup" [("Integer","a"), ("String","b")]]]
  , "MComment" `tup` [ MComment "\nhej\npackage Foo\n"
                     , Package Unencapsulated "F" Nothing [MComment " end F; "]
                     , MComment "*\n * hej\n "]
  , "Import" `tup` [Package Unencapsulated "I" Nothing [
                       Import Unprotected "X"    Nothing       (Left Wild)
                     , Import Protected   "Y"    Nothing       (Left Wild)
                     , Import Unprotected "L"    (Just "Long") (Left Wild)
                     , Import Protected   "Z"    Nothing       (Left Wild)
                    ]]
  , "ImportList" `tup` [Package Unencapsulated "I" Nothing [
                         Import Unprotected "W"    Nothing       (Right [])
                       , Import Unprotected "W"    Nothing       (Right ["a","b","cde"])
                       ]]
  , "EncapsulatedPackage" `tup` [Package Encapsulated "P" Nothing []]
  , "PartialFunction" `tup` [PartFn "X" Nothing [Input ("Integer", "a"), Output ("Integer", "b")]]
  , "ReplaceableType" `tup` [Replaceable "Element"]
  , "Strings" `tup` [Package Unencapsulated "P" (Just "P doc string") [
                      Function "f" (Just "f doc\n  string") [] []
                    , Union "U" (Just "U docstring") []
                    , Union "W" Nothing []
                    , PartFn "F" (Just "F docstring") [Input ("String", "x")]
                    ]]
  ] where tup = (,)
