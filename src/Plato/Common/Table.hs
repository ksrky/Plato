module Plato.Common.Table where

import Plato.Common.Name

import qualified Data.Map.Strict as M

type Table a = M.Map Name a

newTable :: [(Name, a)] -> Table a
newTable = M.fromList

empty :: Table a
empty = M.empty

enter :: Name -> a -> Table a -> Table a
enter = M.insert

look :: Table a -> Name -> Maybe a
look t k = M.lookup k t