module Doc where

import           Print
import           Types

generate :: [AST] -> String
generate = concatMap generate'

generate' :: AST -> String
generate' p@(Package _ _ _ _) = pr p
