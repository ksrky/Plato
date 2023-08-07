module Plato.Parsing.Error where

import Control.Exception.Safe
import Control.Monad.IO.Class
import Control.Monad.Trans.Class
import Prettyprinter

import Plato.Common.Error
import Plato.Common.Location
import Plato.Parsing.Monad
import Plato.Parsing.Token

catchPsErrors :: MonadCatch m => m a -> m a
catchPsErrors =
        ( `catches`
                [ Handler $ \(LexError sp msg) -> throwLocErr sp (pretty msg)
                , Handler $ \(PsError sp msg) -> throwLocErr sp (pretty msg)
                ]
        )

-- | Lexical error
data LexError = LexError Span String

instance Show LexError where
        show (LexError sp msg) = locatedErrorMessage sp $ pretty msg

instance Exception LexError

throwLexError :: MonadThrow m => Span -> String -> ParserT m a
throwLexError sp msg = lift $ throw $ LexError sp msg

catchLexError :: (MonadCatch m, MonadIO m, Monoid a) => m a -> m a
catchLexError = (`catch` (\e@PsError{} -> liftIO (print e) >> return mempty))

-- | Parser error
data PsError = PsError Span Token

instance Show PsError where
        show (PsError sp tok) = locatedErrorMessage sp $ hsep ["parse error at", squotes $ pretty tok]

instance Exception PsError

throwPsError :: MonadThrow m => Span -> Token -> ParserT m a
throwPsError sp tok = lift $ throw $ PsError sp tok

catchPsError :: (MonadCatch m, MonadIO m, Monoid a) => m a -> m a
catchPsError = (`catch` (\e@PsError{} -> liftIO (print e) >> return mempty))