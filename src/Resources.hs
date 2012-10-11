module Resources where

import           Paths_mmdoc
import           System.FilePath

jsShare :: IO FilePath
jsShare = getDataFileName $ "static" </> js

cssShare :: IO FilePath
cssShare = getDataFileName $ "static" </> css

js :: FilePath
js = "Index.js"

css :: FilePath
css = "Index.css"

jsDest :: FilePath -> FilePath
jsDest = (</> js)

cssDest :: FilePath -> FilePath
cssDest = (</> css)
