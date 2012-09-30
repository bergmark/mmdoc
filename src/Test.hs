{-# LANGUAGE RankNTypes #-}

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


import qualified Parser                         as P
import           ParserTest
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
  where dotMo = isSuffixOf ".mo"

feither :: forall a c b. Either a b -> (a -> c) -> (b -> c) -> c
feither e f g = either f g e

parserTests :: IO Test
parserTests = do
  files <- fmap (map ("tests" </>) . sort . filter dotMo) $ getDirectoryContents "tests"
--  let files = map ("tests" </>) ["Package.mo", "Comment.mo"]
  return $ testGroup "Tests" $ flip map files $ \file ->
    testCase file $ do
      let name = (reverse . drop 1 . dropWhile (/= '.') . reverse . drop 1 . dropWhile (/= '/')) file
      let expected = name `lookup` parserExpected
      assertBool (name ++ " is missing in parserExpected") (isJust expected)
      when (isJust expected) $ do
        tokens <- T.parseFile <$> readFile file
        feither tokens
          (const $ assertBool ("Could not tokenize " ++ name) False)
          (\ts -> do
            r <- P.parse ts
            case r of
              Left (P.ParseError perr (P.ParseState { P.parseTokens = ts })) -> error . show $ (perr, take 3 ts)
              Right res -> assertEqual name (fromJust expected) res)
  where dotMo = isSuffixOf ".mo"

main :: IO ()
main = do
  tokenizer <- tokenizerTests
  parser <- parserTests
  defaultMain [{-tokenizer,-} parser]

