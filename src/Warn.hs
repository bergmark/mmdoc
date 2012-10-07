module Warn where

import           Print
import           Types
import           Warnings (Warning)
import qualified Warnings as W

check :: [AST] -> [Warning]
check = concatMap (\ast -> checkProtection ast ++ checkDocstring ast ++ checkStmts ast)

checkProtection :: AST -> [Warning]
checkProtection (Import Nothing n _ (Left Wild)) = return $ W.Unprotected (pr n)
checkProtection (Function Nothing n _ _ _ _ _) = return $ W.Unprotected (pr n)
checkProtection (Package Nothing n _ cs) = W.Unencapsulated (pr n) : concatMap checkProtection cs
checkProtection (Package _ _ _ cs) = concatMap checkProtection cs
checkProtection (ASTPartFn (PartFn Nothing n _ _ _)) = return $ W.Unprotected (pr n)
checkProtection _ = []

checkDocstring :: AST -> [Warning]
checkDocstring (Function _ n  _ Nothing _ _ _) = return $ W.MissingDocstring (pr n)
checkDocstring (Package _ n Nothing cs) = W.MissingDocstring (pr n) : concatMap checkDocstring cs
checkDocstring (Package _ _ _ cs) = concatMap checkDocstring cs
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
