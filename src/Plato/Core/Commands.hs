module Plato.Core.Commands where

import Plato.Common.Name
import Plato.Common.SrcLoc
import Plato.Core.Context
import Plato.Syntax.Core

----------------------------------------------------------------
-- Commands
----------------------------------------------------------------
data Commands = Commands
        { imports :: [Located ModuleName]
        , binds :: [(Located Name, Binding)]
        }
        deriving (Eq, Show)

commandsShift :: Int -> Commands -> Commands
commandsShift d cmds = cmds{binds = map (\(x, b) -> (x, bindingShift d b)) (binds cmds)}
