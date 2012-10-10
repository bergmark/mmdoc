module Main where

import           Control.Applicative
import           System.Environment
import           System.FilePath
import           Text.Blaze.Html.Renderer.String

import qualified Doc                             as D
import           Misc
import qualified Parser                          as P
import qualified Tokenizer                       as T
import qualified Warn                            as W

main :: IO ()
main = do
  src <- (!! 0) <$> getArgs
  if dotMo src
    then print =<< doFile =<< readFile src
    else do
      srcfps <- filter dotMo <$> getDirectoryContentsFullPath src
      destdir <- (!! 1) <$> getArgs
      mapM_ (\fp -> readFile fp >>= doFile >>= writeF fp destdir) srcfps

doFile :: String -> IO (Either String String)
doFile f = do
  case T.parseFile f of
    Left err -> return . Left $ show err
    Right ts -> do
      p <- P.parse ts
      case p of
        Left err -> do
          case err of
            P.ParseError perr (P.ParseState { P.lastToken = lt, P.parseTokens = ts' }) ->
              return . Left $ show (perr, lt, take 5 ts')

        Right ast -> return $
          let ws = W.check ast in
          if (not . null) ws
            then Right . renderHtml $ D.warnings ws
            else Right . renderHtml $ D.htmlDoc ast

writeF :: FilePath -> FilePath -> Either String String -> IO ()
writeF srcfp _ (Left err) = error $ "error in " ++ srcfp ++ ": " ++ err
writeF srcfp destdir (Right _html) = print (destdir </> srcfp) -- writeFile (destdir </> srcfp) html

