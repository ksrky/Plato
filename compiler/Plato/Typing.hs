{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}

module Plato.Typing (typingDefns, typing, typingExpr) where

import Control.Exception.Safe
import Control.Monad.IO.Class
import Control.Monad.Reader
import Control.Monad.Writer
import Data.Foldable
import Data.Tuple qualified as Tuple

import Plato.Common.Error
import Plato.Common.Location
import Plato.Common.Uniq
import Plato.Driver.Monad
import Plato.Syntax.Typing
import Plato.Typing.Env
import Plato.Typing.Kc
import Plato.Typing.Tc
import Plato.Typing.Zonking

typingDefns ::
        (MonadReader e m, HasTypEnv e, HasConEnv e, HasUniq e, MonadCatch m, MonadIO m) =>
        [Defn 'Untyped] ->
        WriterT [Defn 'Typed] m e
typingDefns [] = ask
typingDefns (ValDefn binds : rest) = do
        binds' <- zonk =<< tcBinds binds
        tell [ValDefn binds']
        let sig = map (\(Bind' idty _) -> idty) (toList binds')
        local (modifyTypEnv $ extendList sig) $ typingDefns rest
typingDefns (TypDefn tdefs : rest) = do
        tdefs' <- zonk =<< kcTypDefns tdefs
        tell [TypDefn tdefs']
        let datty = map (\(DatDefn' idkn _ _) -> idkn) tdefs'
            allctors = (`concatMap` tdefs') $ \(DatDefn' _ qns ctors) ->
                map (\(id, ty) -> (id, L (getLoc ty) $ AllT qns ty)) ctors
            extconenv env =
                foldr (\(DatDefn' (id, _) qns ctors) -> extendConEnv id (map fst qns) ctors) env tdefs'
        local (modifyTypEnv $ extendList allctors . extendList datty) $
                local (modifyConEnv extconenv) $
                        typingDefns rest

-----------------------------------------------------------
-- typing
-----------------------------------------------------------
typing :: PlatoMonad m => Prog 'Untyped -> m (Prog 'Typed)
typing defs = catchErrors $ updateContext $ Tuple.swap <$> runWriterT (typingDefns defs)

typingExpr ::
        (MonadReader e m, HasTypEnv e, HasConEnv e, HasUniq e, MonadCatch m, MonadIO m) =>
        LExpr 'Untyped ->
        m (LExpr 'Typed)
typingExpr exp = do
        (exp', ty') <- inferSigma exp
        checkKindStar $ noLoc ty'
        zonk exp'