module Main where

import           Control.Applicative
import           Control.Monad
import           Data.List
import           Data.Maybe
import           System.Directory
import           System.FilePath
import           Test.Framework
import           Test.Framework.Providers.HUnit
import           Test.HUnit                     (assertBool, assertEqual)

import           Parser
import           Types

dropSuffix :: FilePath -> FilePath
dropSuffix = reverse . drop 1 . dropWhile (/= '.') . reverse

parserTests :: IO Test
parserTests = do
  files <- fmap (map ("tests" </>) . sort . filter dotMo) $ getDirectoryContents "tests"
  return $ testGroup "Tests" $ flip map files $ \file ->
    testCase file $ do
      let name = (reverse . drop 1 . dropWhile (/= '.') . reverse . drop 1 . dropWhile (/= '/')) file
      let expected = name `lookup` exps
      assertBool (name ++ " is missing in exps") (isJust expected)
      when (isJust expected) $ do
        result <- parseFile <$> readFile file
        either
          (assertEqual file (show $ fromJust expected) . show)
          (assertEqual file (fromJust expected))
          result
  where dotMo = isSuffixOf ".mo"

main :: IO ()
main = do
  compiler <- parserTests
  defaultMain [compiler]

exps :: [(String, [AST])]
exps = [
    "Package" `tup` [Package "Package" []]
  , "Function" `tup` [Package "Package" [Function "f" [] []]]
  , "FunctionArgs" `tup` [Package "Package" [Function "f" [Input "Integer" "x",Input "Integer" "y",Output "Boolean" "b1",Output "Boolean" "b2"] []]]
  , "FunctionStatements" `tup` [Package "Package" [Function "f" [] [Assign (LVar "x") (EVar "y"),Assign (LVar "aoeu123") (EVar "aoeu123")]]]
  , "Match" `tup` [Package "Package" [Function "f" [] [Assign (LVar "x") (Match ["y"] [(PVar "z",EVar "w")])]]]
  , "Comment" `tup` [Comment " foo", Package "Package" [Comment " bar"]]
  , "UnionType" `tup` [Package "P" [Union "U" []]]
  , "UnionTypeRecord" `tup` [Union "U" [Record "R" []]]
  ] where tup = (,)
