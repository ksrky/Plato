module Plato.Core.Error where

import Control.Exception.Safe
import Plato.Common.Info

data Error' = Error' {message :: String, position :: Pos}

instance Show Error' where
    show (Error' msg pos) = show pos ++ " " ++ msg

instance Exception Error'

throwError :: MonadThrow m => Info -> String -> m a
throwError (Info pos) msg = throw $ Error' msg pos

unreachable :: String -> a
unreachable s = error $ "unreachable: " ++ s