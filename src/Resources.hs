module Resources where

import           Paths_mmdoc
import           System.FilePath

jsShare :: IO FilePath
jsShare = getDataFileName $ "static" </> js

cssShare :: IO FilePath
cssShare = getDataFileName $ "static" </> css

jqueryShare :: IO FilePath
jqueryShare = getDataFileName $ "static" </> jquery

js :: FilePath
js = "Index.js"

css :: FilePath
css = "Index.css"

jquery :: FilePath
jquery = "jquery.js"

jsDest :: FilePath -> FilePath
jsDest = (</> js)

cssDest :: FilePath -> FilePath
cssDest = (</> css)

jqueryDest :: FilePath -> FilePath
jqueryDest = (</> jquery)
