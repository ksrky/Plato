{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Plato.Common.Error where

import Plato.Common.Location

import Control.Exception.Safe
import Control.Monad.IO.Class
import Prettyprinter
import Prettyprinter.Render.String (renderString)

----------------------------------------------------------------
-- Error handling
----------------------------------------------------------------
eitherToMonadThrow :: (MonadThrow m, Exception e) => Either e a -> m a
eitherToMonadThrow (Left e) = throw e
eitherToMonadThrow (Right a) = return a

catchError :: (MonadCatch m, MonadIO m) => m () -> m ()
catchError =
        ( `catches`
                [ Handler $ \e@LocErr{} -> liftIO $ print e
                , Handler $ \e@UnexpectedError{} -> liftIO $ print e
                , Handler $ \(e :: IOException) -> liftIO $ print e
                , Handler $ \(e :: SomeException) -> liftIO $ print e
                ]
        )

continueError :: (MonadCatch m, MonadIO m) => m a -> m a -> m a
continueError cont =
        ( `catches`
                [ Handler $ \e@LocErr{} -> liftIO (print e) >> cont
                , Handler $ \e@UnexpectedError{} -> liftIO (print e) >> cont
                , Handler $ \(e :: IOException) -> liftIO (print e) >> cont
                , Handler $ \(e :: SomeException) -> liftIO (print e) >> cont
                ]
        )

unreachable :: String -> a
unreachable s = error $ "unreachable: " ++ s

----------------------------------------------------------------
-- Error type
----------------------------------------------------------------

-- | Unexpected Error
newtype UnexpectedError = UnexpectedError String

instance Show UnexpectedError where
        show (UnexpectedError msg) = "report this bug:\n" ++ msg

instance Exception UnexpectedError

throwUnexpErr :: MonadThrow m => Doc ann -> m a
throwUnexpErr doc = throw $ UnexpectedError (renderString $ layoutPretty defaultLayoutOptions doc)

-- | Parser error
data ParserError = forall ann. PsErr Span (Doc ann)

instance Show ParserError where
        show (PsErr NoSpan doc) = "<no location info>: " ++ renderString (layoutPretty defaultLayoutOptions doc)
        show (PsErr sp doc) =
                renderString
                        ( layoutPretty defaultLayoutOptions $
                                hcat [pretty sp, colon, space, doc]
                        )

instance Exception ParserError

throwPsErr :: MonadThrow m => Span -> Doc ann -> m a
throwPsErr = (throw .) . PsErr

-- | Error with location
data LocatedError = forall ann. LocErr Span (Doc ann)

instance Show LocatedError where
        show (LocErr NoSpan doc) = "<no location info>: " ++ renderString (layoutPretty defaultLayoutOptions doc)
        show (LocErr sp doc) =
                renderString
                        ( layoutPretty defaultLayoutOptions $
                                hcat [pretty sp, colon, space, doc]
                        )

instance Exception LocatedError

throwLocErr :: MonadThrow m => Span -> Doc ann -> m a
throwLocErr = (throw .) . LocErr

-- | Error with no location
newtype PlainError = PlainError String

instance Show PlainError where
        show (PlainError msg) = msg

instance Exception PlainError

throwError :: MonadThrow m => Doc ann -> m a
throwError doc = throw $ PlainError (renderString $ layoutPretty defaultLayoutOptions doc)

newtype FatalError = FatalError String

instance Show FatalError where
        show (FatalError msg) = "Fatal error: " ++ msg

instance Exception FatalError

throwFatal :: MonadThrow m => String -> m a
throwFatal msg = throw $ FatalError msg