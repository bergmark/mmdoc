{-# LANGUAGE OverloadedStrings #-}

module TestHelp where

import           Types

pkg :: Name -> [AST] -> AST
pkg n asts = Package Nothing n Nothing [] asts

func :: Name -> [Stmt] -> AST
func n s = Function Nothing n [] Nothing [] [] s

partfn :: Name -> [Param] -> AST
partfn n ps = ASTPartFn $ PartFn Nothing n [] Nothing ps

prot :: Maybe Protection
prot = Just Protected

publ :: Maybe Protection
publ = Just Public

enca :: Maybe Protection
enca = Just Encapsulated
