{-# LANGUAGE OverloadedStrings #-}

module ParserTest where

import           TestHelp
import           Types

parserExpected :: [(String, [AST])]
parserExpected = [
    "Package" `tup` [Package Nothing "Package" Nothing [] []]
  , "Function" `tup` [pkg "Package" [Function Nothing "f" [] Nothing [] [] []]]
  , "FunctionArgs" `tup` [pkg "Package"
                           [Function Nothing "f" [] Nothing [
                             Input ("Integer", "x")
                           , Input ("Integer","y")
                           , Output ("Boolean","b1")
                           , Output ("Boolean","b2")] [] []
                           ]]
  , "FunctionStatements" `tup` [pkg "Package" [
                                 func "f" [Assign "x" "y", Assign "aoeu123" "aoeu123"]]
                               ]
  , "Match" `tup` [pkg "Package"
                    [func "f" [
                      Assign "x" (Match ["y"] [] [Case "z" [] "w"] Nothing)]]]
  , "MatchPats" `tup` [func "f" [
                        Assign "res" (Match ["x"] [] [Case (Tuple ["_", "_"]) [] "a"] Nothing)
                      ]]
  , "MatchLocal" `tup` [func "f" [
                         Assign "res" (Match ["x"] [("A", "y")] [Case "_" [] "y"] Nothing)
                       , Assign "res" (Match ["x"]
                           [("A", "a1"),("A", "a2"),("B", "b")]
                           [Case "_" [] "y"]
                           Nothing)
                       ]]
  , "MatchArgs" `tup` [func "f" [
                        Assign "res" $ Match ["a","b","c"] [] [Case "_" [] Unit] Nothing
                      ]]
  , "MatchElse" `tup` [func "f" [
                        Assign "res" $ Match ["a","b","c"] [] [Case "_" [] Unit] (Just $ MatchElse [] Unit)
                      ]]
  , "MatchEquation" `tup` [func "f" [
                            Assign "res" $ Match ["x"] [] [
                              Case "_" ["a"] Unit
                            , Case "_" ["b", Funcall "f" []] Unit
                            ] Nothing
                          ]]
  , "MatchElseEquation" `tup` [func "f" [
                                Assign "res" $ Match ["x"] [] [Case "_" [] "x"] (Just $ MatchElse ["y"] "z")]]
  , "MatchContinue" `tup`
                    [func "f" [
                      Assign "x" (Matchcontinue ["y"] [] [Case "z" [] "w"] Nothing)]]
  , "UnionType" `tup` [pkg "P" [Union Nothing "U" Nothing []]]
  , "UnionTypeRecord" `tup` [Union Nothing "U" Nothing
                              [Record "R" [], Record "Tup" [("Integer","a"), ("String","b")]]
                            ]
  , "Import" `tup` [Package Nothing "I" Nothing [
                       Import Nothing "X"    Nothing       (Left Wild)
                     , Import prot    "Y"    Nothing       (Left Wild)
                     , Import Nothing "L"    (Just "Long") (Left Wild)
                     , Import prot    "Z"    Nothing       (Left Wild)
                    ] []]
  , "ImportList" `tup` [Package Nothing "I" Nothing [
                         Import Nothing "W"    Nothing       (Right [])
                       , Import Nothing "W"    Nothing       (Right ["a","b","cde"])
                       ] []]
  , "EncapsulatedPackage" `tup` [Package enca "P" Nothing [] []]
  , "PartialFunction" `tup` [partfn "X" [
                              Input ("Integer", "a")
                            , Output ("Integer", "b")
                            ]]
  , "ReplaceableType" `tup` [Replaceable "Element"]
  , "String" `tup` [func "f" [StmtExp $ Str "x y z", StmtExp $ Str "1 \\\" 2"]]
  , "Docstring" `tup` [Package Nothing "P" (Just "P doc string") [] [
                      Function Nothing "f" [] (Just "f doc\n  string") [] [] []
                    , Union Nothing "U" (Just "U docstring") []
                    , Union Nothing "W" Nothing []
                    , ASTPartFn $ PartFn Nothing "F" [] (Just "F docstring") [Input ("String", "x")]
                    ]]
  , "PolyType" `tup` [
      Function Nothing "f" ["A"] Nothing [Input (Type "List" ["A"], "a"), Output ("A", "b")] [] []
    , ASTPartFn $ PartFn Nothing "f" ["A"] Nothing [Input (Type "List" ["A"], "a")]
    , Function Nothing "f" ["A","B"] Nothing [Output (Type "Tuple" ["A","B"], "v")] [] []
    , ASTPartFn $ PartFn Nothing "f" ["A"] Nothing [Output (Type "List" [Type "List" ["A"]], "v")]
    ]
  , "StandAloneStmt" `tup` [func "f" [StmtExp "stmt"]]
  , "Funcall" `tup` [func "f" [
                      StmtExp (Funcall "f" [])
                    , Assign "x" (Funcall "f" [])
                    , StmtExp (Funcall "g" [Funcall "f" [], "x", "y"])
                    ]]
  , "TypeAlias" `tup` [
      TypeAlias "X" "Y"
    , TypeAlias "X" $ Type "Tuple" [Type "Array" ["A"], "B"]
    ]
  , "Underscores" `tup` [func "f" [Assign "_" "x"]]
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
                     Assign (Tuple ["x","y"]) (Tuple ["z", "w"])
                   ]]
  , "Unit" `tup` [func "f" [Assign "x" Unit]]
  , "IfStmt" `tup` [func "f" [If [(InfixApp "==" "x" "y", [Assign "a" "b"])] Nothing]]
  , "IfElseifElse" `tup` [func "f" [
                           If [
                             (InfixApp "==" "x" "y", [Assign "a" "b"])
                           , ("z", [StmtExp "c", StmtExp "d"])
                           ] (Just [StmtExp "e", StmtExp "f"])
                          ]]
  , "Integer" `tup` [func "f" [
                      Assign "x" (InfixApp "+" "1" "2")
                    , Assign "y" (UnaryApp "-" "3")
                    ]]
  , "QualifiedName" `tup` [Function Nothing "f" [] Nothing [
                            Input ("A", "b")
                          , Input ("A.B", "c")
                          , Input (Type "A.B" ["C"] , "d")
                          , Input ("A.B.C" , "d")
                          , Input (Type "A" ["B.C"] , "d")
                          ] [] [
                            StmtExp (Funcall "A.B.c" [])
                          , StmtExp "A.B.c"
                          ]]
  , "IfExpr" `tup` [func "f" [
                     Assign "r" (EIf [(InfixApp "==" "x" "y", "a")] "b")
                   , Assign "r" (EIf [
                       (InfixApp "==" "x" "y", "a")
                     , (InfixApp "==" "z" "w", "b")
                     ] "c")
                   ]]
  , "Protection" `tup` [
      Package Nothing "X" Nothing [
        Import   Nothing "I" Nothing (Left Wild)
      , Import   publ "I" Nothing (Left Wild)
      , Import   prot "I" Nothing (Left Wild)
      ] [
        Package  Nothing "P" Nothing [] []
      , ASTPartFn (PartFn Nothing "F" [] Nothing [])
      , Function Nothing "f" [] Nothing [] [] []

      , Package  publ "P" Nothing [] []
      , ASTPartFn $ PartFn publ "F" [] Nothing []
      , Function publ "f" [] Nothing [] [] []

      , Package  prot "P" Nothing [] []
      , ASTPartFn $ PartFn prot "F" [] Nothing []
      , Function prot "f" [] Nothing [] [] []

      , Union Nothing "U" Nothing []
      , Union publ "U" Nothing []
      , Union prot "U" Nothing []
      ]
    ]
  , "Constant" `tup` [Constant ("Integer") "i" "3"]
  , "FunProtected" `tup` [
      Function Nothing "f" [] Nothing [] [] []
    , Function Nothing "g" [] Nothing [] [
        FunProtVar ("Integer", "x")
      , FunProtVar ("String", "y")
      , FunProtPart $ PartFn Nothing "G" [] Nothing []
      ] []
    ]
  , "Not" `tup` [func "f" [StmtExp $ UnaryApp "not" "y"]]
  , "StrExp" `tup` [func "f" [StmtExp $ Str "str"]]
  , "VarDeclMultiple" `tup` [
     Function Nothing "f" [] Nothing [] [FunProtVar ("Integer", "x"),FunProtVar ("Integer", "y")] []
    ]
  , "BoolOp" `tup` [func "f" [
      StmtExp $ InfixApp "or" "a" "b"
    , StmtExp $ InfixApp "and" "c" "d"
    , StmtExp $ InfixApp "or" "x" $ InfixApp "and" "y" "z"
    , If [(InfixApp "and" (Funcall "f" ["a"]) (Funcall "f" ["b"]), [StmtExp ("x")])] Nothing
    ]]
  , "SkipComment" `tup` [func "f" [StmtExp $ "x"]]
  , "List" `tup` [func "f" [StmtExp $ List [], StmtExp $ List ["1", "2", "3"]]]
  , "For" `tup` [func "f" [For "i" (InfixApp ":" "1" "n") ["x"]]]
  , "Assign" `tup` [func "f" [
      Assign "x" "a"
    , Assign (Tuple ["x", "y"]) (Tuple ["a", "b"])
    , Assign (Tuple [Tuple ["x", "y"], "z"]) (Tuple [Tuple ["a", "b"], "c"])
    ]]
  , "ReservedPrefix" `tup` [ASTPartFn $ PartFn Nothing "F" [] Nothing [Input ("ordering", "ord")]]
  , "PackageShort" `tup` [
      PackageShort Nothing "P" "PKG" [Redeclare "K" "Integer", Redeclare "A" "String"]
    , PackageShort Nothing "P" "PKG" [Redeclare "K" "Integer"]
    ]
  ] where tup = (,)
