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

(<++>) :: String -> String -> String
(<++>) "" y = y
(<++>) x  y = x ++ " " ++ y


instance Print AST where
  pr (Package e n d cs) = concat [ pr e <++> "package " ++ pr n ++ "\n"
                                 , pr_docstr d
                                 , concatLines pr cs
                                 , "\nend " ++ pr n ++ ";"]
  pr (Function p n qs d ps _stms) = pr p <++> "function " ++ pr n ++ pr_polyList qs ++ "\n" ++ pr_docstr d ++ "\n" ++ concatLines (ind . pr) ps ++ "\nend " ++ pr n ++ ";"
  pr (PartFn p n qs d ps) = pr p <++> "partial function " ++ pr n ++ pr_polyList qs ++ "\n" ++ pr_docstr d ++ "\n" ++ concatLines (ind . pr) ps ++ "\nend " ++ pr n ++ ";"
  pr (Comment s) = "//" ++ s
  pr (MComment s) = "/*" ++ s ++ "*/"
  pr (Union n d rs) = concat [
                       "uniontype " ++ pr n ++ "\n"
                     ,   pr_docstr d ++ "\n"
                     ,   concatLines (ind . pr) rs
                     , "\nend " ++ pr n ++ ";"]
  pr (Import p n Nothing vs)     = pr p <++> "import " ++ pr n ++ pr_importList vs ++ ";"
  pr (Import p n (Just ln) vs)   = pr p <++> "import " ++ pr n ++ " = " ++ pr ln ++ pr_importList vs ++ ";"
  pr (Replaceable n) = "replaceable type " ++ pr n ++ " subtypeof Any;"
  pr (TypeAlias a b) = "type " ++ pr a ++ " = " ++ pr b ++ ";"

pr_importList :: Either Wild [Var] -> String
pr_importList (Left Wild) = ".*"
pr_importList (Right ns) = "." ++ pr_list ns

pr_list :: Print p => [p] -> String
pr_list ps = "{" ++ (intercalate ", " . map pr $ ps) ++ "}"

pr_polyList :: Print p => [p] -> String
pr_polyList ps = "<" ++ (intercalate ", " . map pr $ ps) ++ ">"

pr_docstr :: Maybe DocString -> String
pr_docstr Nothing = ""
pr_docstr (Just s) = "\"" ++ s ++ "\""

instance Print Name where
  pr (UnQual s) = s
  pr (Qual n s) = pr n ++ "." ++ s

instance Print Record where
  pr (Record n vds) = "record" ++ pr n ++ concatMap pr vds ++ "end" ++ pr n ++ ";"

instance Print VarDecl where
  pr (t, v) = pr t ++ " " ++ v

instance Print Param where
  pr (Input vd) = "input " ++ pr vd
  pr (Output vd) = "output " ++ pr vd

instance Print Type where
  pr (Type n ns) = pr n ++ (if null ns then "" else "<" ++ (intercalate ", " ns) ++ ">")

instance Print Var where
  pr = id

instance Print Protection where
  pr Unprotected = ""
  pr Protected = "protected"
  pr Public = "public"

instance Print Encapsulation where
  pr Encapsulated = "encapsulated"
  pr Unencapsulated = ""
