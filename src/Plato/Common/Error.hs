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
                [ Handler $ \e@PlainErr{} -> liftIO $ putStrLn $ pretty e
                , Handler $ \e@LocatedErr{} -> liftIO $ putStrLn $ pretty e
                , Handler $ \e@PsError{} -> liftIO $ putStrLn $ pretty e
                ]
        )

unreachable :: String -> a
unreachable s = error $ "unreachable: " ++ s

-- | Unexpected Error
newtype UnexpectedErr = UnexpectedErr String deriving (Show)

instance Exception UnexpectedErr

throwUnexpectedErr :: MonadThrow m => String -> m a
throwUnexpectedErr msg = throw $ UnexpectedErr msg

-- | Plain Error
newtype PlainErr = PlainErr String deriving (Show)

instance Exception PlainErr

instance Pretty PlainErr where
        pretty (PlainErr msg) = msg

throwPlainErr :: MonadThrow m => String -> m a
throwPlainErr msg = throw $ PlainErr msg

-- | Error with location
data LocatedErr = LocatedErr Span String deriving (Show)

instance Exception LocatedErr

instance Pretty LocatedErr where
        pretty (LocatedErr (Span s e) msg) = pretty s ++ "-" ++ pretty e ++ ": " ++ msg
        pretty (LocatedErr NoSpan msg) = msg

throwLocatedErr :: MonadThrow m => Span -> String -> m a
throwLocatedErr sp msg = throw $ LocatedErr sp msg
