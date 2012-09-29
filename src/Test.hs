module Main where

import           Control.Applicative
import           Data.List
import           System.Directory
import           System.FilePath
import           Test.Framework
import           Test.Framework.Providers.HUnit
import           Test.HUnit                     (assertEqual)

import           Parser

dropSuffix :: FilePath -> FilePath
dropSuffix = reverse . drop 1 . dropWhile (/= '.') . reverse

parserTests :: IO Test
parserTests = do
  files <- fmap (map ("tests" </>) . sort . filter dotMo) $ getDirectoryContents "tests"
  return $ testGroup "Tests" $ flip map files $ \file ->
    testCase file $ do
      let expfp = (reverse . drop 1 . dropWhile (/= '.') . reverse) file
      expected <- readFile expfp
      result <- parseFile <$> readFile file
      assertEqual file expected (either show show result)
  where dotMo = isSuffixOf ".mo"

main :: IO ()
main = do
  compiler <- parserTests
  defaultMain [compiler]
