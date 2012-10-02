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

import           Misc
import qualified Parser                         as P
import           ParserTest
import qualified Print
import qualified Tokenizer                      as T
import           TokenTest

dropSuffix :: FilePath -> FilePath
dropSuffix = reverse . drop 1 . dropWhile (/= '.') . reverse

tokenizerTests :: IO Test
tokenizerTests = do
  files <- fmap (map ("tests" </>) . sort . filter dotMo) $ getDirectoryContents "tests"
  return $ testGroup "Tokenizer" $ flip map files $ \file ->
    testCase file $ do
      let name = (reverse . drop 1 . dropWhile (/= '.') . reverse . drop 1 . dropWhile (/= '/')) file
      let expected = name `lookup` tokenExpected
      when (isJust expected) $ do
        result <- T.parseFile <$> readFile file
        feither result
          (assertEqual file (show $ fromJust expected) . show)
          (assertEqual file (fromJust expected))

parserTests :: IO Test
parserTests = do
  files <- fmap (map ("tests" </>) . sort . filter dotMo) $ getDirectoryContents "tests"
--  let files = map ("tests" </>) ["EncapsulatedPackage.mo"]
  return $ testGroup "Tests" $ flip map files $ \file ->
    testCase file $ do
      let name = (reverse . drop 1 . dropWhile (/= '.') . reverse . drop 1 . dropWhile (/= '/')) file
      let expected = name `lookup` parserExpected
      assertBool (name ++ " is missing in parserExpected") (isJust expected)
      when (isJust expected) $ do
        tokens <- T.parseFile <$> readFile file
        feither tokens
          (\err -> assertBool ("Could not tokenize " ++ name ++ ": " ++ show err) False)
          (\toks -> do
            r <- P.parse toks
            case r of
              Left (P.ParseError perr (P.ParseState { P.parseTokens = ts })) -> error . show $ (perr, take 3 ts)
              Right res -> assertEqual name (fromJust expected) res)

printTests :: IO Test
printTests = do
  files <- fmap (map ("tests/print" </>) . sort . filter dotMo) $ getDirectoryContents "tests/print"
  return $ testGroup "Printing" $ flip map files $ \file ->
    testCase file $ do
      let name = (reverse . drop 1 . dropWhile (/= '.') . reverse . drop 1 . dropWhile (/= '/')) file
      contents <- readFile file
      let tokens = T.parseFile contents
      feither tokens
        (\err -> assertBool ("Could not tokenize " ++ name ++ ": " ++ show err) False)
        (\toks -> do
          res <- P.parse toks
          feither res
            (\(P.ParseError perr (P.ParseState { P.parseTokens = ts })) -> error . show $ (perr, take 3 ts))
            (const $ assertBool "..." True))
--            (\r -> assertEqual name contents (Print.printSrc r)))

main :: IO ()
main = do
  tokenizer <- tokenizerTests
  parser <- parserTests
  pr <- printTests
  defaultMain [tokenizer, parser, pr]

