module Plato.Common.Table where

import qualified Plato.Common.Name as N

import Control.Monad.State
import qualified Data.Map.Strict as M

type Table a = M.Map N.Name a

new :: [(N.Name, a)] -> Table a
new = M.fromList

empty :: Table a
empty = M.empty

enter :: Monad m => N.Name -> a -> StateT (Table a) m ()
enter n v = modify $ \table -> M.insert n v table

look :: Table a -> N.Name -> Maybe a
look table n = M.lookup n table
