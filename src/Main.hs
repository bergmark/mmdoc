module Main where

import           Control.Applicative
import           System.Environment

import qualified Doc                 as D
import qualified Parser              as P
import qualified Tokenizer           as T
import qualified Warn                as W

main :: IO ()
main = do
  fp   <- (!! 0) <$> getArgs
  dest <- (!! 1) <$> getArgs
  f    <- readFile fp
  case T.parseFile f of
    Left err -> putStrLn "----- tokenize error:" >> print err
    Right ts -> do
      p <- P.parse ts
      case p of
        Left err -> putStrLn "---- parse error:" >> print err
        Right ast -> do
          let ws = W.check ast
          if (not . null) ws
            then putStrLn "---- warnings: " >> print ws
            else writeFile dest (D.generate ast)


