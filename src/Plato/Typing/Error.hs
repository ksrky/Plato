module Plato.Typing.Error where

import Plato.Common.Pretty
import Plato.Common.SrcLoc

import Control.Exception.Safe

data TypError = TypError {errorMessage :: String, errorSpan :: Span} deriving (Show)

instance Pretty TypError where
        pretty (TypError msg (Span s e)) = pretty s ++ "-" ++ pretty e ++ ": " ++ msg
        pretty (TypError msg NoSpan) = msg

instance Exception TypError

type TypThrow = Either TypError

throwTypError :: Span -> String -> TypThrow a
throwTypError sp msg = Left $ TypError msg sp
