{-# LANGUAGE OverloadedStrings #-}

module ParserTest where

import           TestHelp
import           Types

parserExpected :: [(String, [AST])]
parserExpected = [
    "Package" `tup` [Package Nothing "Package" Nothing []]
  , "Function" `tup` [Package Nothing "Package" Nothing [func "f" []]]
  , "FunctionArgs" `tup` [Package Nothing "Package" Nothing
                           [Function Nothing "f" [] Nothing [
                             Input (ty "Integer", "x")
                           , Input (ty "Integer","y")
                           , Output (ty "Boolean","b1")
                           , Output (ty "Boolean","b2")] [] []
                           ]]
  , "FunctionStatements" `tup` [Package Nothing "Package" Nothing [
                                 func "f" [Assign ["x"] "y",Assign ["aoeu123"] "aoeu123"]]
                               ]
  , "Match" `tup` [Package Nothing "Package" Nothing
                    [func "f" [
                      Assign ["x"] (Match ["y"] [] [Case "z" "w"])]]]
  , "MatchPats" `tup` [func "f" [
                        Assign ["res"] (Match ["x"] [] [Case (Tuple ["_", "_"]) "a"])
                      ]]
  , "MatchLocal" `tup` [func "f" [
                         Assign ["res"] (Match ["x"] [(ty "A", "y")] [Case "_" "y"])
                       , Assign ["res"] (Match ["x"]
                           [(ty "A", "a1"),(ty "A", "a2"),(ty "B", "b")]
                           [Case "_" "y"])
                       ]]
  , "MatchArgs" `tup` [func "f" [
                        Assign ["res"] $ Match ["a","b","c"] []
                          [Case "_" Unit]
                      ]]
  , "UnionType" `tup` [Package Nothing "P" Nothing [Union "U" Nothing []]]
  , "UnionTypeRecord" `tup` [Union "U" Nothing [Record "R" [], Record "Tup" [(ty "Integer","a"), (ty "String","b")]]]
  , "Import" `tup` [Package Nothing "I" Nothing [
                       Import Nothing "X"    Nothing       (Left Wild)
                     , Import prot    "Y"    Nothing       (Left Wild)
                     , Import Nothing "L"    (Just "Long") (Left Wild)
                     , Import prot    "Z"    Nothing       (Left Wild)
                    ]]
  , "ImportList" `tup` [Package Nothing "I" Nothing [
                         Import Nothing "W"    Nothing       (Right [])
                       , Import Nothing "W"    Nothing       (Right ["a","b","cde"])
                       ]]
  , "EncapsulatedPackage" `tup` [Package enca "P" Nothing []]
  , "PartialFunction" `tup` [partfn "X" [
                              Input (ty "Integer", "a")
                            , Output (ty "Integer", "b")
                            ]]
  , "ReplaceableType" `tup` [Replaceable "Element"]
  , "String" `tup` [func "f" [StmtExp $ Str "x y z", StmtExp $ Str "1 \\\" 2"]]
  , "Docstring" `tup` [Package Nothing "P" (Just "P doc string") [
                      Function Nothing "f" [] (Just "f doc\n  string") [] [] []
                    , Union "U" (Just "U docstring") []
                    , Union "W" Nothing []
                    , ASTPartFn $ PartFn Nothing "F" [] (Just "F docstring") [Input (ty "String", "x")]
                    ]]
  , "PolyType" `tup` [
      Function Nothing "f" ["A"] Nothing [Input (Type "List" ["A"], "a"), Output (ty "A", "b")] [] []
    , ASTPartFn $ PartFn Nothing "f" ["A"] Nothing [Input (Type "List" ["A"], "a")]
    ]
  , "StandAloneStmt" `tup` [func "f" [StmtExp "stmt"]]
  , "Funcall" `tup` [func "f" [
                      StmtExp (Funcall "f" [])
                    , Assign ["x"] (Funcall "f" [])
                    , StmtExp (Funcall "g" [Funcall "f" [], "x", "y"])
                    ]]
  , "TypeAlias" `tup` [TypeAlias "X" "Y"]
  , "Underscores" `tup` [func "f" [Assign ["_"] "x"]]
  , "Operators" `tup` [func "f" [
                        StmtExp $ InfixApp "+&" "a" "b"
                      , StmtExp $ InfixApp "/" "c" "d"
                      , StmtExp $ InfixApp "*" "e" "f"
                      , StmtExp $ InfixApp "*" "e" $ InfixApp "/" "g" "h"
                      , StmtExp $ InfixApp "<=" "a" "b"
                      , StmtExp $ InfixApp "<" "a" "b"
                      , StmtExp $ InfixApp "<>" "a" "b"
                      , StmtExp $ InfixApp ">" "a" "b"
                      , StmtExp $ InfixApp ">=" "a" "b"
                      , StmtExp $ InfixApp "=" "a" "b"
                      , StmtExp $ InfixApp "==" "a" "b"
                      ]]
  , "Tuples" `tup` [func "f" [
                     Assign ["x","y"] (Tuple ["z", "w"])
                   ]]
  , "Unit" `tup` [func "f" [Assign ["x"] Unit]]
  , "IfStmt" `tup` [func "f" [If [(InfixApp "==" "x" "y", [Assign ["a"] "b"])] Nothing]]
  , "IfElseifElse" `tup` [func "f" [
                           If [
                             (InfixApp "==" "x" "y", [Assign ["a"] "b"])
                           , (EVar "z", [StmtExp "c", StmtExp "d"])
                           ] (Just [StmtExp "e", StmtExp "f"])
                          ]]
  , "Integer" `tup` [func "f" [
                      Assign ["x"] (InfixApp "+" "1" "2")
                    , Assign ["y"] (UnaryApp "-" "3")
                    ]]
  , "QualifiedName" `tup` [Function Nothing "f" [] Nothing [
                            Input (Type "A" [] , "b")
                          , Input (Type (qual ["A","B"]) [], "c")
                          , Input (Type (qual ["A","B"]) ["C"] , "d")
                          , Input (Type (qual ["A","B","C"]) [] , "d")
                          , Input (Type "A" [qual ["B","C"]] , "d")
                          ] [] [
                            StmtExp (Funcall (qual ["A","B","c"]) [])
                          , StmtExp (EVar (qual ["A", "B", "c"]))
                          ]]
  , "IfExpr" `tup` [func "f" [
                     Assign ["r"] (EIf [(InfixApp "==" "x" "y", "a")] "b")
                   , Assign ["r"] (EIf [
                       (InfixApp "==" "x" "y", "a")
                     , (InfixApp "==" "z" "w", "b")
                     ] "c")
                   ]]
  , "Protection" `tup` [
      Package  Nothing "P" Nothing []
    , ASTPartFn (PartFn Nothing "F" [] Nothing [])
    , Function Nothing "f" [] Nothing [] [] []
    , Import   Nothing "I" Nothing (Left Wild)

    , Package  publ "P" Nothing []
    , ASTPartFn $ PartFn publ "F" [] Nothing []
    , Function publ "f" [] Nothing [] [] []
    , Import   publ "I" Nothing (Left Wild)

    , Package  prot "P" Nothing []
    , ASTPartFn $ PartFn prot "F" [] Nothing []
    , Function prot "f" [] Nothing [] [] []
    , Import   prot "I" Nothing (Left Wild)
    ]
  , "Constant" `tup` [Constant (ty "Integer") "i" "3"]
  , "FunProtected" `tup` [
      Function Nothing "f" [] Nothing [] [] []
    , Function Nothing "g" [] Nothing [] [
        FunProtVar (ty "Integer", "x")
      , FunProtVar (ty "String", "y")
      , FunProtPart $ PartFn Nothing "G" [] Nothing []
      ] []
    ]
  , "Not" `tup` [
      func "f" [StmtExp $ UnaryApp "not" "y"]
    ]
  , "StrExp" `tup` [
      func "f" [StmtExp $ Str "str"]
    ]
  , "VarDeclMultiple" `tup` [
     Function Nothing "f" [] Nothing [] [FunProtVar (ty "Integer", "x"),FunProtVar (ty "Integer", "y")] []
    ]
  , "BoolOp" `tup` [func "f" [
      StmtExp $ InfixApp "or" "a" "b"
    , StmtExp $ InfixApp "and" "c" "d"
    , StmtExp $ InfixApp "or" "x" $ InfixApp "and" "y" "z"
    , If [(InfixApp "and" (Funcall "f" ["a"]) (Funcall "f" ["b"]), [StmtExp ("x")])] Nothing
    ]]
  , "SkipComment" `tup` [func "f" [StmtExp $ "x"]]
  , "List" `tup` [func "f" [StmtExp $ List [], StmtExp $ List ["1", "2", "3"]]]
  ] where tup = (,)
