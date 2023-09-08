module Plato.Parsing.Error where

import Control.Exception.Safe
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
                , Handler $ \(PsError sp tok) -> throwLocErr sp $ hsep ["parse error at", pretty tok]
                ]
        )

-- | Lexical error
data LexError = LexError Span String

instance Show LexError where
        show _ = "Lexer error"

instance Exception LexError

throwLexError :: MonadThrow m => Span -> String -> ParserT m a
throwLexError sp msg = lift $ throw $ LexError sp msg

-- | Parser error
data PsError = PsError Span Token

instance Show PsError where
        show _ = "Parser error"

instance Exception PsError

throwPsError :: MonadThrow m => Span -> Token -> ParserT m a
throwPsError sp tok = lift $ throw $ PsError sp tok
