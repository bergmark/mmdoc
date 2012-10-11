module Main where

import           Distribution.Simple
import           System.Exit
import           System.FilePath
import           System.IO
import           System.Process

srcFp = "src/Fay/Index.hs"
destFp = "static/Index.js"

compileFay :: IO ()
compileFay = do
  (code,out,err) <- readProcessWithExitCode "fay" [srcFp, "--output", destFp] ""
  case code of
    ExitFailure _ ->
      hPutStrLn stderr $ "fay: Error compiling " ++ srcFp ++ ":\n" ++ err
    ExitSuccess -> return ()

main :: IO ()
main = do
  compileFay
  defaultMain
