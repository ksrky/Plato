module Plato.Common.Error where

import Plato.Common.Pretty
import Plato.Common.SrcLoc
import Plato.Parsing.Error

import Control.Exception.Safe
import Control.Monad.IO.Class

----------------------------------------------------------------
-- Error Handling
----------------------------------------------------------------
eitherToMonadThrow :: (MonadThrow m, Exception e) => Either e a -> m a
eitherToMonadThrow (Left e) = throw e
eitherToMonadThrow (Right a) = return a

catchError :: (MonadCatch m, MonadIO m) => m () -> m ()
catchError =
        ( `catches`
                [ Handler $ \e@PsError{} -> liftIO $ putStrLn $ pretty e
                -- other exceptions
                ]
        )

unreachable :: String -> a
unreachable s = error $ "unreachable: " ++ s
