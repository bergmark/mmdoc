module Warn where

import           Print
import           Types
import           Warnings (Warning)
import qualified Warnings as W

check :: [AST] -> [Warning]
check = concatMap (\ast -> checkProtection ast ++ checkDocstring ast ++ checkStmts ast)

checkProtection :: AST -> [Warning]
checkProtection (Function Nothing n _ _ _ _ _) = return $ W.Unprotected (pr n)
checkProtection (Package Nothing n _ is cs) = W.Unencapsulated (pr n) : concatMap checkImportProtection is ++ concatMap checkProtection cs
checkProtection (Package _       _ _ is cs) = concatMap checkImportProtection is ++ concatMap checkProtection cs
checkProtection (ASTPartFn (PartFn Nothing n _ _ _)) = return $ W.Unprotected (pr n)
checkProtection _ = []

checkImportProtection :: Import -> [Warning]
checkImportProtection (Import Nothing n _ (Left Wild)) = return $ W.Unprotected (pr n)
checkImportProtection _ = []

checkDocstring :: AST -> [Warning]
checkDocstring (Function _ n  _ Nothing _ _ _) = return $ W.MissingDocstring (pr n)
checkDocstring (Package _ n Nothing _ cs) = W.MissingDocstring (pr n) : concatMap checkDocstring cs
checkDocstring (Package _ _ _ _ cs) = concatMap checkDocstring cs
checkDocstring (ASTPartFn (PartFn _ n _ Nothing _)) = return $ W.MissingDocstring (pr n)
checkDocstring (Union n Nothing _) = return $ W.MissingDocstring (pr n)
checkDocstring _ = []

checkStmts :: AST -> [Warning]
checkStmts (Function _ _ _ _ _ _ stmts) = concatMap aux stmts
  where
    aux :: Stmt -> [Warning]
    aux (StmtExp e) = return $ W.StmtExp e
    aux _ = []
checkStmts _ = []
