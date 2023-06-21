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

continueError :: (MonadCatch m, MonadIO m) => m a -> m a -> m a
continueError cont =
        ( `catches`
                [ Handler $ \e@LocErr{} -> liftIO (print e) >> cont
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