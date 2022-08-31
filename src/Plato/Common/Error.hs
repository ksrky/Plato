module Plato.Common.Error where

import Control.Exception.Safe (Exception, MonadThrow, throw)
import Plato.Common.Info

data Error = Error {message :: String, info :: Info}

instance Show Error where
    show (Error msg fi) | fi == dummyInfo = msg
    show (Error msg fi) = show fi ++ " " ++ msg

instance Exception Error

throwError :: MonadThrow m => Info -> String -> m a
throwError fi msg = throw $ Error msg fi

throwMsg :: MonadThrow m => String -> m a
throwMsg msg = throw $ Error msg dummyInfo

unreachable :: String -> a
unreachable s = error $ "unreachable: " ++ s
