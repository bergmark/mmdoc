module Main where

import           Control.Applicative
import           Control.Monad
import           System.Directory
import           System.Environment
import           System.FilePath
import           System.IO
import           Text.Blaze.Html.Renderer.String
import           Text.Groom

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
      let destfps = flip map srcfps $ (`addExtension` ".html") . dropExtension . (destdir </>) . takeFileName
      mapM_ (\fp -> readFile fp >>= doFile >>= writeF fp destdir) srcfps
      writeIndex (destdir </> "index.html") destfps

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

        Right ast -> do
          let ws = W.check ast
          when (not . null $ ws) $
            putStrLn . groom $ ws
          return . Right . renderHtml . D.htmlDoc $ ast

writeF :: FilePath -> FilePath -> Either String String -> IO ()
writeF srcfp _ (Left err) = hPutStrLn stderr $ "error in " ++ srcfp ++ ": " ++ err
--writeF srcfp destdir (Right _html) = print (destdir </> srcfp)
writeF srcfp destdir (Right html) = do
  let fp = (`addExtension` ".html") . dropExtension $ destdir </> takeFileName srcfp
  putStrLn $ "writing " ++ fp
  writeFile fp html

writeIndex :: FilePath -> [String] -> IO ()
writeIndex fp documents = do
  exs <- mapM doesFileExist documents
  let fns = flip map documents $ takeFileName . dropExtension
  writeFile fp . renderHtml . D.index $ zip fns exs
