module Plato.Common.Error where

import Plato.Common.SrcLoc

import Control.Exception.Safe
import Control.Monad.IO.Class
import Prettyprinter

----------------------------------------------------------------
-- Error Handling
----------------------------------------------------------------
eitherToMonadThrow :: (MonadThrow m, Exception e) => Either e a -> m a
eitherToMonadThrow (Left e) = throw e
eitherToMonadThrow (Right a) = return a

catchError :: (MonadCatch m, MonadIO m) => m () -> m ()
catchError =
        ( `catches`
                [ Handler $ \e@LocatedErr{} -> liftIO $ print e
                , Handler $ \e@UnexpectedErr{} -> liftIO $ print e
                -- , Handler $ \(e :: IOException) -> liftIO $ putStrLn "File not found"
                -- Handler $ \(e :: SomeException) -> liftIO $ putStrLn "Unknown error"
                ]
        )

unreachable :: String -> a
unreachable s = error $ "unreachable: " ++ s

-- | Unexpected Error
newtype UnexpectedErr = UnexpectedErr String deriving (Show)

instance Exception UnexpectedErr

throwUnexpectedErr :: MonadThrow m => String -> m a
throwUnexpectedErr msg = throw $ UnexpectedErr msg

-- | Doc Error
throwError :: MonadThrow m => Doc ann -> m a
throwError doc = throwString $ show doc

-- | Error with location
data LocatedErr = LocatedErr Span String deriving (Show)

instance Exception LocatedErr

throwLocErr :: MonadThrow m => Span -> Doc ann -> m a
throwLocErr sp doc = throw $ LocatedErr sp (show doc)