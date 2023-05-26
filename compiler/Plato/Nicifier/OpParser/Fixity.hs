module Plato.Nicifier.OpParser.Fixity where

import Data.Map.Strict as M

import Plato.Common.Name
import Plato.Syntax.Parsing

maxPrec, minPrec :: FixPrec
maxPrec = 9
minPrec = 0

defaultFixity :: Fixity
defaultFixity = Fixity maxPrec Leftfix

----------------------------------------------------------------
-- FixityEnv
----------------------------------------------------------------
type FixityEnv = M.Map Name Fixity

initFixityEnv :: FixityEnv
initFixityEnv = M.empty

class HasFixityEnv a where
        getFixityEnv :: a -> FixityEnv
        modifyFixityEnv :: (FixityEnv -> FixityEnv) -> a -> a

instance HasFixityEnv FixityEnv where
        getFixityEnv = id
        modifyFixityEnv = id