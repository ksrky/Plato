module Plato.Interpreter where

import Control.Exception.Safe
import Control.Monad.IO.Class
import Data.Text qualified as T

import Plato.Common.Location
import Plato.Driver.Monad
import Plato.Nicifier
import Plato.Parsing
import Plato.PsToTyp
import Plato.Syntax.Core
import Plato.TypToCore
import Plato.Typing

interpretExpr :: PlatoMonad m => T.Text -> m b
interpretExpr inp = do
        pssyn <- parseExpr inp
        pssyn' <- nicifyExpr pssyn
        typsyn <- psToTypExpr pssyn'
        typsyn' <- typingExpr typsyn
        coresyn <- typToCoreExpr typsyn'
        undefined

class Interpreter e where
        enter :: (MonadIO m, MonadThrow m) => e -> Prog -> m ()
        interp :: (MonadIO m, MonadThrow m) => e -> Term -> m ()
