module ParserTest where

import           Types

ty :: String -> Type
ty s = Type s []

parserExpected :: [(String, [AST])]
parserExpected = [
    "Package" `tup` [Package Unencapsulated "Package" Nothing []]
  , "Function" `tup` [Package Unencapsulated "Package" Nothing [Function "f" [] Nothing [] []]]
  , "FunctionArgs" `tup` [Package Unencapsulated "Package" Nothing
                           [Function "f" [] Nothing
                             [Input (ty "Integer", "x")
                             ,Input (ty "Integer","y")
                             ,Output (ty "Boolean","b1")
                             ,Output (ty "Boolean","b2")] []]]
  , "FunctionStatements" `tup` [Package Unencapsulated "Package" Nothing [Function "f" [] Nothing [] [Assign "x" (EVar "y"),Assign "aoeu123" (EVar "aoeu123")]]]
  , "Match" `tup` [Package Unencapsulated "Package" Nothing
                    [Function "f" [] Nothing [] [Assign "x" (Match ["y"] [("z",EVar "w")])]]]
  , "Comment" `tup` [Comment " foo", Package Unencapsulated "Package" Nothing [Comment " bar"]]
  , "UnionType" `tup` [Package Unencapsulated "P" Nothing [Union "U" Nothing []]]
  , "UnionTypeRecord" `tup` [Union "U" Nothing [Record "R" [], Record "Tup" [(ty "Integer","a"), (ty "String","b")]]]
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
  , "PartialFunction" `tup` [PartFn "X" [] Nothing [
                                Input (ty "Integer", "a")
                              , Output (ty "Integer", "b")]]
  , "ReplaceableType" `tup` [Replaceable "Element"]
  , "Strings" `tup` [Package Unencapsulated "P" (Just "P doc string") [
                      Function "f" [] (Just "f doc\n  string") [] []
                    , Union "U" (Just "U docstring") []
                    , Union "W" Nothing []
                    , PartFn "F" [] (Just "F docstring") [Input (ty "String", "x")]
                    ]]
  , "PolyType" `tup` [
      Function "f" [ty "A"] Nothing [Input (Type "List" ["A"], "a"), Output (ty "A", "b")] []
    , PartFn "f" [ty "A"] Nothing [Input (Type "List" ["A"], "a")]
    ]
  , "StandAloneStmt" `tup` [Function "f" [] Nothing [] [StmtExp (EVar "stmt")]]
  , "Funcall" `tup` [Function "f" [] Nothing [] [
                      StmtExp (Funcall "f" [])
                    , Assign "x" (Funcall "f" [])
                    , StmtExp (Funcall "g" [Funcall "f" [], EVar "x", EVar "y"])
                    ]]
  , "TypeAlias" `tup` [TypeAlias "X" "Y"]
  ] where tup = (,)
