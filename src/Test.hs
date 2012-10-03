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
import           Types
import qualified Warn                           as W
import           WarnTest                       (warnExpected)

main :: IO ()
main = do
  testfiles  <- (\v -> if null v then Nothing else Just v) . filter dotMo <$> getArgs
  runnerArgs <- filter (not . dotMo) <$> getArgs
  tokenizer  <- mkTest testfiles tokenizerTests "Tokenizer" "tests"
  parser     <- mkTest testfiles parserTests    "Parser"    "tests"
  p          <- mkTest testfiles printTests     "Print"     "tests/print"
  w          <- mkTest testfiles warnTests      "Warn"      "tests/warn"
  defaultMainWithArgs [tokenizer, parser, p, w] runnerArgs

parserTests :: FilePath -> String -> Test
parserTests file name =
  let expected = name `lookup` parserExpected in
  testCase file $ do
    fmaybe expected
      (fail $ name ++ " is missing in parserExpected")
      (\exp -> tokenize file name $
        parse (eq name exp))

printTests :: FilePath -> String -> Test
printTests file name =
  testCase file $
    tokenize file name $
      parse (const $ assertBool "..." True)

tokenizerTests :: FilePath -> String -> Test
tokenizerTests file name =
  let expected = name `lookup` tokenExpected in
  testCase file $
    when (isJust expected) $ do
      result <- T.parseFile <$> readFile file
      feither result
        (assertEqual file (show $ fromJust expected) . show)
        (assertEqual file (fromJust expected))

warnTests :: FilePath -> String -> Test
warnTests file name =
  testCase file $ do
    tokenize file name $
      parse (\r -> do
        fmaybe (name `lookup` warnExpected)
          (fail $ name ++ " is missing in warnExpected")
          (\exp -> assertEqual name exp (W.check r)))

mkTest :: Maybe [FilePath] -> (FilePath -> String -> Test) -> TestName -> FilePath -> IO Test
mkTest fs p tgName dir = do
  files <- maybe (findTestFiles dir) return fs
  return . testGroup tgName $ map (\f -> p f (takeFileName $ dropExtension f)) files

findTestFiles :: FilePath -> IO [FilePath]
findTestFiles fp = fmap (map (fp </>) . sort . filter dotMo) . getDirectoryContents $ fp

tokenize :: FilePath -> String -> (T.Program -> IO ()) -> IO ()
tokenize file name f = do
  contents <- readFile file
  let tokens = T.parseFile contents
  either (\err -> assertBool ("Could not tokenize " ++ name ++ ": " ++ show err) False) f tokens

parse :: ([AST] -> IO b) -> T.Program -> IO b
parse f toks =
  P.parse toks >>= either (\(P.ParseError perr (P.ParseState { P.parseTokens = ts })) -> error $ show (perr, take 3 ts)) f

eq :: (Eq a, Show a) => String -> a -> a -> IO ()
eq preface expected actual =
  unless (actual == expected) (assertFailure msg)
 where msg = (if null preface then "" else preface ++ "\n") ++
             "expected: " ++ groom expected ++ "\n but got: " ++ groom actual

fail :: String -> IO ()
fail s = assertBool s False
