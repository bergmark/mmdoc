{-# LANGUAGE OverloadedStrings #-}

module ParserTest where

import           Data.String
import           Types

ty :: Name -> Type
ty n = Type n []

func :: Name -> [Stmt] -> AST
func n s = Function n [] Nothing [] s

qual :: [String] -> Name
qual [] = error "qual"
qual [x] = UnQual x
qual (x:xs) = Qual (qual xs) x

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
  , "FunctionStatements" `tup` [Package Unencapsulated "Package" Nothing [Function "f" [] Nothing [] [Assign ["x"] (EVar "y"),Assign ["aoeu123"] (EVar "aoeu123")]]]
  , "Match" `tup` [Package Unencapsulated "Package" Nothing
                    [func "f" [
                      Assign ["x"] (Match ["y"] [Case (EVar "z") (EVar "w")])]]]
  , "MatchPats" `tup` [func "f" [
                        Assign ["res"] (Match ["x"] [Case (Tuple [EVar "_", EVar "_"]) (EVar "a")])
                      ]]
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
      Function "f" ["A"] Nothing [Input (Type "List" ["A"], "a"), Output (ty "A", "b")] []
    , PartFn "f" ["A"] Nothing [Input (Type "List" ["A"], "a")]
    ]
  , "StandAloneStmt" `tup` [Function "f" [] Nothing [] [StmtExp (EVar "stmt")]]
  , "Funcall" `tup` [Function "f" [] Nothing [] [
                      StmtExp (Funcall "f" [])
                    , Assign ["x"] (Funcall "f" [])
                    , StmtExp (Funcall "g" [Funcall "f" [], EVar "x", EVar "y"])
                    ]]
  , "TypeAlias" `tup` [TypeAlias "X" "Y"]
  , "Underscores" `tup` [func "f" [Assign ["_"] (EVar "x")]]
  , "Operators" `tup` [func "f" [
                        StmtExp (InfixApp "+&" (EVar "a") (EVar "b"))
                      , StmtExp (InfixApp "/" (EVar "c") (EVar "d"))
                      , StmtExp (InfixApp "*" (EVar "e") (EVar "f"))
                      , StmtExp (InfixApp "*" (EVar "e") (InfixApp "/" (EVar "g") (EVar "h")))
                      ]]
  , "Tuples" `tup` [func "f" [
                     Assign ["x","y"] (Tuple [EVar "z",EVar "w"])
                   ]]
  , "Unit" `tup` [func "f" [Assign ["x"] Unit]]
  , "IfStmt" `tup` [func "f" [If [(InfixApp "==" (EVar "x") (EVar "y"), [Assign ["a"] (EVar "b")])] Nothing]]
  , "IfElseifElse" `tup` [func "f" [
                           If [
                             (InfixApp "==" (EVar "x") (EVar "y"), [Assign ["a"] (EVar "b")])
                           , (EVar "z", [StmtExp (EVar "c"), StmtExp (EVar "d")])
                           ] (Just [StmtExp (EVar "e"), StmtExp (EVar "f")])
                          ]]
  , "Integer" `tup` [func "f" [
                      Assign ["x"] (InfixApp "+" (EVar "1") (EVar "2"))
                    , Assign ["y"] (UnaryApp "-" (EVar "3"))
                    ]]
  , "QualifiedName" `tup` [Function "f" [] Nothing [
                            Input (Type "A" [] , "b")
                          , Input (Type (qual ["A","B"]) [], "c")
                          , Input (Type (qual ["A","B"]) ["C"] , "d")
                          , Input (Type (qual ["A","B","C"]) [] , "d")
                          ] [
                            StmtExp (Funcall (qual ["A","B","c"]) [])
                          , StmtExp (EVar (qual ["A", "B", "c"]))
                          ]]
  , "IfExpr" `tup` [func "f" [
                     Assign ["r"] (EIf [(InfixApp "==" (EVar "x") (EVar "y"), EVar "a")] (EVar "b"))
                   , Assign ["r"] (EIf [
                       (InfixApp "==" (EVar "x") (EVar "y"), EVar "a")
                     , (InfixApp "==" (EVar "z") (EVar "w"), EVar "b")
                     ] (EVar "c"))
                   ]]
  ] where tup = (,)

instance IsString Name
  where fromString = UnQual
