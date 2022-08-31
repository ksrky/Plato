{-# LANGUAGE OverloadedStrings #-}

module Plato.Core.Command where

import Plato.Common.Info
import Plato.Common.Name
import Plato.Core.Context
import Plato.Core.Syntax

----------------------------------------------------------------
-- Commands
----------------------------------------------------------------
data Commands = Commands
        { imports :: [ModuleName]
        , binds :: [(Info, (Name, Binding))]
        , body :: Term
        }
        deriving (Eq, Show)

commandsShift :: Int -> Commands -> Commands
commandsShift d cmds = cmds{binds = map (\(fi, (x, b)) -> (fi, (x, bindingShift d b))) (binds cmds)}

----------------------------------------------------------------
-- Module
----------------------------------------------------------------
data Module = Module
        { moduleName :: ModuleName
        , publicModule :: [Module]
        , privateModule :: [Module]
        }
        deriving (Eq, Show)

data ModuleGraph = ModNode Module [ModuleGraph]
        deriving (Eq, Show)

-- baseModule :: [String]
-- baseModule = ["Plato.Base", "Plato.Bool", "Plato.Maybe", "Plato.Either", "Plato.Nat", "Plato.List"]

baseModules :: [ModuleName]
baseModules = map (ModuleName . map str2conName) xs
    where
        xs =
                [ ["Plato", "Base"]
                , ["Plato", "Bool"]
                , ["Plato", "Maybe"]
                , ["Plato", "Either"]
                , ["Plato", "Nat"]
                , ["Plato", "List"]
                ]
