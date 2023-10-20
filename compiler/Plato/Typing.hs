{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}

module Plato.Typing (typingDefns, typing, typingExpr) where

import Control.Exception.Safe
import Control.Monad.IO.Class
import Control.Monad.Reader
import Control.Monad.Writer
import Data.Tuple qualified as Tuple

import Plato.Common.Error
import Plato.Common.Location
import Plato.Common.Uniq
import Plato.Driver.Monad
import Plato.Syntax.Typing
import Plato.Syntax.Typing.Helper
import Plato.Typing.Env
import Plato.Typing.Kc
import Plato.Typing.Tc
import Plato.Typing.Zonking

typingDefns ::
        (MonadReader e m, HasTypEnv e, HasUniq e, MonadCatch m, MonadIO m) =>
        [Defn 'Untyped] ->
        WriterT [Defn 'Typed] m e
typingDefns [] = ask
typingDefns (ValDefn binds : rest) = do
        binds' <- zonk =<< tcBinds binds
        tell [ValDefn binds']
        local (modifyTypEnv $ extendBinds binds') $ typingDefns rest
typingDefns (TypDefn tdefs : rest) = do
        tdefs' <- zonk =<< kcTypDefns tdefs
        tell [TypDefn tdefs']
        let datasig = fmap (\(DatDefn id params _) -> (id, dataSignat params)) tdefs'
            allctors = (`concatMap` tdefs') $ \(DatDefn _ qns ctors) ->
                map (\(id, ty) -> (id, L (getLoc ty) $ AllT qns ty)) ctors
            extconenv env = foldr extendDataBinds env tdefs'
        local (modifyTypEnv $ extendList allctors . extendList datasig . extconenv) $ typingDefns rest

-----------------------------------------------------------
-- typing
-----------------------------------------------------------
typing :: (PlatoMonad m) => Prog 'Untyped -> m (Prog 'Typed)
typing defs = catchErrors $ updateContext $ Tuple.swap <$> runWriterT (typingDefns defs)

typingExpr ::
        (MonadReader e m, HasTypEnv e, HasUniq e, MonadCatch m, MonadIO m) =>
        LExpr 'Untyped ->
        m (Expr 'Typed)
typingExpr exp = do
        (exp', ty') <- inferSigma exp
        checkKindStar =<< zonk (noLoc ty')
        zonk exp'