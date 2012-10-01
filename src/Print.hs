{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE TypeSynonymInstances #-}

module Print where

import           Data.List

import           Types

class Print a where
    pr :: a -> String

printSrc :: [AST] -> String
printSrc = concatLines pr

linewrap :: [String] -> String
linewrap = intercalate "\n"

concatLines :: (a -> String) -> [a] -> String
concatLines f cs = linewrap (map f cs)

ind :: String -> String
ind s = "  " ++ s


instance Print AST where
  pr (Package Encapsulated n d cs) = concat [ "encapsulated package " ++ pr n ++ "\n"
                                          , concatLines pr cs
                                          , "\nend " ++ pr n ++ ";"]
  pr (Package Unencapsulated n d cs) = "package " ++ pr n ++ "\n" ++ concatLines pr cs ++ "\nend " ++ pr n ++ ";"
  pr (Function n d ps _stms) = "function " ++ pr n ++ concatLines (ind . pr) ps ++ "\nend " ++ pr n ++ ";"
  pr (PartFn n d ps) = "partial function " ++ pr n ++ concatLines (ind . pr) ps ++ "\nend " ++ pr n ++ ";"
  pr (Comment s) = "//" ++ s
  pr (MComment s) = "/*" ++ s ++ "*/"
  pr (Union n d rs) = concat [
                       "uniontype " ++ pr n
                     ,   concatLines (ind . pr) rs
                     , "\nend " ++ pr n ++ ";"]
  pr (Import Protected n Nothing vs)     = "protected import " ++ pr n ++ pr_importList vs ++ ";"
  pr (Import Protected n (Just ln) vs)   = "protected import " ++ pr n ++ " = " ++ pr ln ++ pr_importList vs ++ ";"
  pr (Import Unprotected n Nothing vs)   = "import " ++ pr n ++ pr_importList  vs ++ ";"
  pr (Import Unprotected n (Just ln) vs) = "import " ++ pr n ++ " = " ++ pr ln ++ pr_importList vs ++ ";"
  pr (Replaceable n) = "replaceable type " ++ pr n ++ " subtypeof Any;"

pr_importList :: Either Wild [Name] -> String
pr_importList (Left Wild) = ".*"
pr_importList (Right ns) = "." ++ pr_nameList ns

pr_nameList :: [Name] -> String
pr_nameList = intercalate ", " . map pr

instance Print Name where
  pr s = s

instance Print Record where
  pr (Record n vds) = "record" ++ pr n ++ concatMap pr vds ++ "end" ++ pr n ++ ";"

instance Print VarDecl where
  pr (t, v) = t ++ " " ++ v

instance Print Param where
  pr (Input vd) = "input " ++ pr vd
  pr (Output vd) = "output " ++ pr vd
