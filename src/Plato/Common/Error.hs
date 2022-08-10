module Plato.Common.Error where

import Control.Exception.Safe (Exception, MonadThrow, throw)
import Plato.Common.Info (Info)

data Error = Error {message :: String, info :: Info}

instance Show Error where
    show (Error msg fi) = show fi ++ " " ++ msg

instance Exception Error

throwError :: MonadThrow m => Info -> String -> m a
throwError fi msg = throw $ Error msg fi

unreachable :: String -> a
unreachable s = error $ "unreachable: " ++ s
