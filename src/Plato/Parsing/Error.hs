module Plato.Parsing.Error where

import Plato.Common.Pretty
import Plato.Common.SrcLoc

import Control.Exception.Safe

data PsError = PsError {errorMessage :: String, errorSpan :: Span} deriving (Show)

instance Pretty PsError where
        pretty (PsError msg (Span s e)) = pretty s ++ "-" ++ pretty e ++ ": " ++ msg
        pretty (PsError msg NoSpan) = msg

instance Exception PsError

throwPsError :: MonadThrow m => Span -> String -> m a
throwPsError sp msg = throw $ PsError msg sp
