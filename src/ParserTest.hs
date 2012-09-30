module ParserTest where

import           Parser
import           Types

parserExpected :: [(String, [AST])]
parserExpected = [
    "Package" `tup` [Package "Package" []]
  , "Function" `tup` [Package "Package" [Function "f" [] []]]
  , "FunctionArgs" `tup` [Package "Package" [Function "f" [Input "Integer" "x",Input "Integer" "y",Output "Boolean" "b1",Output "Boolean" "b2"] []]]
  , "FunctionStatements" `tup` [Package "Package" [Function "f" [] [Assign (LVar "x") (EVar "y"),Assign (LVar "aoeu123") (EVar "aoeu123")]]]
  , "Match" `tup` [Package "Package" [Function "f" [] [Assign (LVar "x") (Match ["y"] [(PVar "z",EVar "w")])]]]
  , "Comment" `tup` [Comment " foo", Package "Package" [Comment " bar"]]
  , "UnionType" `tup` [Package "P" [Union "U" []]]
  , "UnionTypeRecord" `tup` [Union "U" [Record "R" [], Record "Tup" [("Integer","a"), ("String","b")]]]
  ] where tup = (,)
