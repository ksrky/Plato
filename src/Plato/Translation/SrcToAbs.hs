module Plato.Translation.SrcToAbs where

import Plato.Abstract.Lexer (runAlex)
import Plato.Abstract.Parser (parse)
import Plato.Abstract.Syntax (Program)
import Plato.Common.Error ( throwMsg )

import Control.Exception.Safe (MonadThrow)

src2abs :: MonadThrow m => String -> m Program
src2abs input = case runAlex input parse of
        Left msg -> throwMsg msg
        Right ast -> return ast
