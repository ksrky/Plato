module Plato.Common.Error where

import Control.Exception.Safe (
        Exception,
        Handler (Handler),
        IOException,
        MonadCatch,
        MonadThrow,
        SomeException,
        catches,
        throw,
 )
import Control.Monad.IO.Class (MonadIO (..))
import GHC.Stack (HasCallStack)
import Prettyprinter
import Prettyprinter.Render.String (renderString)

import Plato.Common.Location

----------------------------------------------------------------
-- Error handling
----------------------------------------------------------------
catchError :: (MonadCatch m, MonadIO m) => m () -> m ()
catchError =
        ( `catches`
                [ Handler $ \e@PsErr{} -> liftIO $ print e
                , Handler $ \e@LocErr{} -> liftIO $ print e
                , Handler $ \e@PlainErr{} -> liftIO $ print e
                , Handler $ \(e :: IOException) -> liftIO $ print e
                , Handler $ \(e :: SomeException) -> liftIO $ print e
                ]
        )

continueError :: (MonadCatch m, MonadIO m) => m a -> m a -> m a
continueError cont =
        ( `catches`
                [ Handler $ \e@PsErr{} -> liftIO (print e) >> cont
                , Handler $ \e@LocErr{} -> liftIO (print e) >> cont
                , Handler $ \e@PlainErr{} -> liftIO (print e) >> cont
                , Handler $ \(e :: IOException) -> liftIO (print e) >> cont
                , Handler $ \(e :: SomeException) -> liftIO (print e) >> cont
                ]
        )

unreachable :: HasCallStack => String -> a
unreachable s = error $ "unreachable: " ++ s

----------------------------------------------------------------
-- Error type
----------------------------------------------------------------

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
newtype PlainError = PlainErr String

instance Show PlainError where
        show (PlainErr msg) = msg

instance Exception PlainError

throwError :: MonadThrow m => Doc ann -> m a
throwError doc = throw $ PlainErr (renderString $ layoutPretty defaultLayoutOptions doc)