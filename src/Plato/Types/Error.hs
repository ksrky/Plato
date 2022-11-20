{-# LANGUAGE ScopedTypeVariables #-}

module Plato.Types.Error where

import Plato.Types.Location

import Control.Exception.Safe
import Control.Monad.IO.Class
import Prettyprinter
import Prettyprinter.Render.String (renderString)

----------------------------------------------------------------
-- Error Handling
----------------------------------------------------------------
eitherToMonadThrow :: (MonadThrow m, Exception e) => Either e a -> m a
eitherToMonadThrow (Left e) = throw e
eitherToMonadThrow (Right a) = return a

catchError :: (MonadCatch m, MonadIO m) => m () -> m ()
catchError =
        ( `catches`
                [ Handler $ \e@LocatedError{} -> liftIO $ print e
                , Handler $ \e@UnexpectedError{} -> liftIO $ print e
                , Handler $ \(e :: IOException) -> liftIO $ print e
                , Handler $ \(e :: SomeException) -> liftIO $ print e
                ]
        )

continueError :: (MonadCatch m, MonadIO m) => m a -> m a -> m a
continueError cont =
        ( `catches`
                [ Handler $ \e@LocatedError{} -> liftIO (print e) >> cont
                , Handler $ \e@UnexpectedError{} -> liftIO (print e) >> cont
                , Handler $ \(e :: IOException) -> liftIO (print e) >> cont
                , Handler $ \(e :: SomeException) -> liftIO (print e) >> cont
                ]
        )

unreachable :: String -> a
unreachable s = error $ "unreachable: " ++ s

-- | Unexpected Error
newtype UnexpectedError = UnexpectedError String

instance Show UnexpectedError where
        show (UnexpectedError msg) = "report this bug:\n" ++ msg

instance Exception UnexpectedError

throwUnexpErr :: MonadThrow m => Doc ann -> m a
throwUnexpErr doc = throw $ UnexpectedError (renderString $ layoutPretty defaultLayoutOptions doc)

-- | Error with location
data LocatedError = LocatedError Span String

instance Show LocatedError where
        show (LocatedError NoSpan msg) = "<no location info>: " ++ msg
        show (LocatedError sp msg) = renderString (layoutPretty defaultLayoutOptions $ pretty sp) ++ ": " ++ msg

instance Exception LocatedError

throwLocErr :: MonadThrow m => Span -> Doc ann -> m a
throwLocErr sp doc = throw $ LocatedError sp (renderString $ layoutPretty defaultLayoutOptions doc)

-- | Error with no location
newtype PlainError = PlainError String

instance Show PlainError where
        show (PlainError msg) = msg

instance Exception PlainError

throwError :: MonadThrow m => Doc ann -> m a
throwError doc = throw $ PlainError (renderString $ layoutPretty defaultLayoutOptions doc)