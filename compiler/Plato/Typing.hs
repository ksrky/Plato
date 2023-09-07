{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}

module Plato.Typing (typingDefns, typing, typingExpr) where

import Control.Exception.Safe
import Control.Monad.IO.Class
import Control.Monad.Reader
import Control.Monad.Writer
import Data.Tuple qualified as Tuple

import Plato.Common.Error
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
        let sig = map (\(Bind' idty _) -> idty) binds'
        local (modifyTypEnv $ extendList sig) $ typingDefns rest
typingDefns (TypDefn tdefs : rest) = do
        tdefs' <- zonk =<< kcTypDefns tdefs
        tell [TypDefn tdefs']
        let datsig = map (\(DatDefn' idkn _ _) -> idkn) tdefs'
            ctors = concatMap (\(DatDefn' _ _ ctors) -> ctors) tdefs'
            extce env = foldr (\(DatDefn' (id, _) qns ctors) -> extendConEnv id (map fst qns) ctors) env tdefs'
        local (modifyTypEnv $ extendList ctors . extendList datsig) $
                local (modifyConEnv extce) $
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
typingExpr exp = fst <$> inferType exp