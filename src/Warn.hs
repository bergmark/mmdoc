module Warn where

import           Print
import           Types
import           Warnings (Warning)
import qualified Warnings as W

check :: [AST] -> [Warning]
check = concatMap check'

check' :: AST -> [Warning]
check' (Comment _) = []
check' (Constant _ _ _) = []
check' (Import Nothing n _ (Left Wild)) = return $ W.Unprotected (pr n)
check' (Import _ _ _ _) = []
check' (Function Nothing n _ _ _ _) = return $ W.Unprotected (pr n)
check' (Function _ _ _ _ _ _) = []
check' (MComment _) = []
check' (Package Nothing n _ cs) = W.Unencapsulated (pr n) : check cs
check' (Package _ n _ cs) = check cs
check' (PartFn Nothing n _ _ _) = return $ W.Unprotected (pr n)
check' (PartFn _ _ _ _ _) = []
check' (Replaceable _) = []
check' (TypeAlias _ _) = []
check' (Union _ _ _) = []
