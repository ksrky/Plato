module Plato.Common.Error where

import Control.Exception.Safe
import Control.Monad.IO.Class
import GHC.Stack
import Prettyprinter.Render.Text

import Plato.Common.Location
import Plato.Common.Pretty

defaultHandler :: (MonadIO m, Monoid a) => Handler m a
defaultHandler = Handler $ \(e :: SomeException) -> liftIO (print e) >> return mempty

catchErrors :: (MonadCatch m, MonadIO m, Monoid a) => m a -> m a
catchErrors =
        ( `catches`
                [ Handler $ \e@LocErr{} -> liftIO (printLocErr e) >> return mempty
                , Handler $ \e@PlainErr{} -> liftIO (printPlainErr e) >> return mempty
                , defaultHandler
                ]
        )

unreachable :: (HasCallStack) => String -> a
unreachable s = error $ "unreachable:\n" ++ s ++ "\n"

-- | Error with location
data LocatedError = forall ann. LocErr Span (Doc ann)

instance Show LocatedError where
        show (LocErr sp doc) = show sp ++ ":" ++ show doc

instance Exception LocatedError

throwLocErr :: (MonadThrow m) => Span -> Doc ann -> m a
throwLocErr sp doc = throw $ LocErr sp doc

printLocErr :: LocatedError -> IO ()
printLocErr (LocErr sp msg) = putDoc $ case sp of
        NoSpan -> hcat ["<no location info>: error: ", line, indent 4 msg, line]
        _ -> hcat [pretty sp, ": error: ", line, indent 4 msg, line]

-- | Error with no location
data PlainError = forall ann. PlainErr (Doc ann)

instance Show PlainError where
        show (PlainErr doc) = show doc

instance Exception PlainError

throwError :: (MonadThrow m) => Doc ann -> m a
throwError doc = throw $ PlainErr doc

printPlainErr :: PlainError -> IO ()
printPlainErr (PlainErr msg) = putDoc $ hcat ["error: ", line, indent 4 msg, line]