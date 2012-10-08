{-# LANGUAGE OverloadedStrings #-}

module TestHelp where

import           Types

func :: Name -> [Stmt] -> AST
func n s = Function Nothing n [] Nothing [] [] s

partfn :: Name -> [Param] -> AST
partfn n ps = ASTPartFn $ PartFn Nothing n [] Nothing ps

qual :: [String] -> Name
qual [] = error "qual"
qual [x] = UnQual x
qual (x:xs) = Qual (qual xs) x

prot :: Maybe Protection
prot = Just Protected

publ :: Maybe Protection
publ = Just Public

enca :: Maybe Protection
enca = Just Encapsulated
