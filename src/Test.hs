module Main where

import           Control.Applicative
import           Control.Monad                  hiding (fail)
import           Data.List
import           Data.Maybe
import           Prelude                        hiding (exp, fail)
import           System.Directory
import           System.Environment
import           System.FilePath
import           Test.Framework
import           Test.Framework.Providers.HUnit
import           Test.HUnit                     (assertBool, assertEqual)
import           Test.HUnit.Lang
import           Text.Groom

import           Misc
import qualified Parser                         as P
import           ParserTest
import qualified Tokenizer                      as T
import           TokenTest
import qualified Warn                           as W
import           WarnTest                       (warnExpected)

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

parserTests :: Maybe [FilePath] -> IO Test
parserTests jfiles = do
  files <- fmaybe jfiles
             (fmap (map ("tests" </>) . sort . filter dotMo) $ getDirectoryContents "tests")
             return
--  let files = map ("tests" </>) ["Funcall.mo"]
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
              Left (P.ParseError perr (P.ParseState { P.lastToken = lt, P.parseTokens = ts })) ->
                error . show $ (perr, lt, take 3 ts)
              Right res -> eq name (fromJust expected) res)

eq :: (Eq a, Show a) => String -> a -> a -> IO ()
eq preface expected actual =
  unless (actual == expected) (assertFailure msg)
 where msg = (if null preface then "" else preface ++ "\n") ++
             "expected: " ++ groom expected ++ "\n but got: " ++ groom actual

fail :: String -> IO ()
fail s = assertBool s False

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
            (\(P.ParseError perr (P.ParseState { P.parseTokens = ts })) -> error $ show (perr, take 3 ts))
            (const $ assertBool "..." True))
--            (\r -> assertEqual name contents (Print.printSrc r)))

warnTests :: IO Test
warnTests = do
  files <- fmap (map ("tests/warn" </>) . sort . filter dotMo) $ getDirectoryContents "tests/warn"
  return $ testGroup "Warn" $ flip map files $ \file ->
    testCase file $ do
      let name = (drop 1 . dropWhile (/= '/') . reverse . drop 1 . dropWhile (/= '.') . reverse . drop 1 . dropWhile (/= '/')) file
      contents <- readFile file
      let tokens = T.parseFile contents
      feither tokens
        (\err -> assertBool ("Could not tokenize " ++ name ++ ": " ++ show err) False)
        (\toks -> do
          res <- P.parse toks
          feither res
            (\(P.ParseError perr (P.ParseState { P.parseTokens = ts })) -> error $ show (perr, take 3 ts))
            (\r -> do
               fmaybe (name `lookup` warnExpected)
                 (fail $ name ++ " is missing in warnExpected")
                 (\exp -> assertEqual name exp (W.check r))))

main :: IO ()
main = do
  testfiles <- (\v -> if null v then Nothing else Just v) . filter dotMo <$> getArgs
  runnerArgs <- filter (not . dotMo) <$> getArgs
  tests <- fmaybe testfiles
    (do
      tokenizer <- tokenizerTests
      parser <- parserTests Nothing
      pr <- printTests
      w <- warnTests
      return [tokenizer, parser, pr, w])
    (\j -> (: []) <$> parserTests (Just j))

  defaultMainWithArgs tests runnerArgs
