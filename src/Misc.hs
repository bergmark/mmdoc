module Misc where

import           Data.List

feither :: Either a b -> (a -> c) -> (b -> c) -> c
feither e f g = either f g e

dotMo :: String -> Bool
dotMo = isSuffixOf ".mo"
