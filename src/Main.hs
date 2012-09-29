module Main where

import           Control.Applicative
import           System.Environment

import           Parser

main :: IO ()
main = do
  args <- getArgs
  res <- parseFile <$> readFile (head args)
  either print print res
