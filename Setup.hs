module Main where

import           Distribution.ModuleName            (ModuleName, toFilePath)
import           Distribution.PackageDescription
import           Distribution.Simple
import           Distribution.Simple.LocalBuildInfo
import           Distribution.Simple.Setup
import           System.Exit
import           System.FilePath
import           System.IO
import           System.Process

srcFp = "src/Fay/Index.hs"
destFp = "static/Index.js"

main :: IO ()
main =
  defaultMainWithHooks simpleUserHooks { postBuild = buildFay }

-- | Build the client.
buildFay :: Args -> BuildFlags -> PackageDescription -> LocalBuildInfo -> IO ()
buildFay _ _ _ _ = do
  (code,out,err) <- readProcessWithExitCode "fay" [srcFp, "--output", destFp] ""
  case code of
    ExitFailure _ ->
      hPutStrLn stderr $ "fay: Error compiling " ++ srcFp ++ ":\n" ++ err
    ExitSuccess -> return ()
