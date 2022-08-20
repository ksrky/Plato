module Plato.Translation.SrcToAbs where

import Plato.Abstract.Lexer (runAlex)
import Plato.Abstract.Parser (parse)
import Plato.Abstract.Syntax (Program)

import Control.Exception.Safe (MonadThrow, throwString)

src2abs :: MonadThrow m => String -> m Program
src2abs input = case runAlex input parse of
        Left msg -> throwString msg
        Right ast -> return ast
