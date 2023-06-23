module Plato.Common.Error where

import Control.Exception.Safe (
        Exception,
        Handler (Handler),
        MonadCatch,
        MonadThrow,
        SomeException,
        catches,
        throw,
 )
import Control.Monad.IO.Class (MonadIO (..))
import GHC.Stack (HasCallStack)
import Prettyprinter (
        Doc,
        Pretty (pretty),
        colon,
        defaultLayoutOptions,
        hcat,
        layoutPretty,
        space,
 )
import Prettyprinter.Render.String (renderString)

import Plato.Common.Location

catchError :: (MonadCatch m, MonadIO m) => m () -> m ()
catchError =
        ( `catches`
                [ Handler $ \e@LocErr{} -> liftIO (print e)
                , Handler $ \e@PlainErr{} -> liftIO (print e)
                , Handler $ \(e :: SomeException) -> liftIO (print e)
                ]
        )

continueError :: (MonadCatch m, MonadIO m) => m a -> m a -> m a
continueError action =
        ( `catches`
                [ Handler $ \e@LocErr{} -> liftIO (print e) >> action
                , Handler $ \e@PlainErr{} -> liftIO (print e) >> action
                , Handler $ \(e :: SomeException) -> liftIO (print e) >> action
                ]
        )

unreachable :: HasCallStack => String -> a
unreachable s = error $ "unreachable: " ++ s

----------------------------------------------------------------
-- Error type
----------------------------------------------------------------

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
throwLocErr sp doc = throw $ LocErr sp doc

-- | Error with no location
newtype PlainError = PlainErr String

instance Show PlainError where
        show (PlainErr msg) = msg

instance Exception PlainError

throwError :: MonadThrow m => Doc ann -> m a
throwError doc = throw $ PlainErr (renderString $ layoutPretty defaultLayoutOptions doc)