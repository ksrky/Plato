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

defaultHandler :: (MonadIO m, Monoid a) => Handler m a
defaultHandler = Handler $ \(e :: SomeException) -> do
        liftIO (do putStrLn "Compiler bug:"; print e)
        return mempty

catchErrors :: (MonadCatch m, MonadIO m, Monoid a) => m a -> m a
catchErrors =
        ( `catches`
                [ Handler $ \e@LocErr{} -> liftIO (print e) >> return mempty
                , Handler $ \e@PlainErr{} -> liftIO (print e) >> return mempty
                , defaultHandler
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

locatedErrorMessage :: Span -> Doc ann -> String
locatedErrorMessage NoSpan msg =
        "<no location info>: " ++ renderString (layoutPretty defaultLayoutOptions msg)
locatedErrorMessage sp msg =
        renderString (layoutPretty defaultLayoutOptions $ hcat [pretty sp, colon, space, msg])

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