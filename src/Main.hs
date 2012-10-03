module Main where

import           Control.Applicative
import           System.Environment

import qualified Parser              as P
import           Print
import qualified Tokenizer           as T

main :: IO ()
main = do
  fp <- head <$> getArgs
  f <- readFile fp
  print fp
  case T.parseFile f of
    Left err -> putStrLn "----- tokenize error:" >> print err
    Right ts -> do
      putStrLn "----- Tokens:" >> print ts
      p <- P.parse ts
      case p of
        Left err -> putStrLn "---- parse error:" >> print err
        Right ast -> do
          putStrLn "----- AST:" >> print ast
          putStrLn "----- Print:" >> putStrLn (printSrc ast)
