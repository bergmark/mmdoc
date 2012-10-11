module Misc where

import           Control.Applicative
import           System.Directory
import           System.FilePath

import           Data.List

feither :: Either a b -> (a -> c) -> (b -> c) -> c
feither e f g = either f g e

fmaybe :: Maybe a -> b -> (a -> b) -> b
fmaybe m f g = maybe f g m

dotMo :: String -> Bool
dotMo = isSuffixOf ".mo"

getDirectoryContentsFullPath :: FilePath -> IO [FilePath]
getDirectoryContentsFullPath dir = map (dir </>) <$> getDirectoryContents dir

fileName :: FilePath -> String
fileName = takeFileName . dropExtension
