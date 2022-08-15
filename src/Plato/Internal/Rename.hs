module Plato.Internal.Rename where

import Plato.Common.Name
import Plato.Common.Vect

data Memo = Memo {store :: Vect (Name, Name), level :: Int}

emptyMemo :: Memo
emptyMemo = Memo{store = empty, level = 0}

fresh :: Memo -> Name
fresh memo = str2varName ("_" ++ show (level memo))
