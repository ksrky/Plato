{-# LANGUAGE TupleSections #-}

module Plato.Typing.Kc (
        newDataType,
        getDataType,
        checkKindStar,
        inferDataKind,
        inferKind,
        checkKind,
) where

import Control.Exception.Safe
import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Reader
import Data.Bifunctor qualified as Bifunctor
import GHC.Stack

import Plato.Common.Error
import Plato.Common.Ident
import Plato.Common.Location
import Plato.Common.Uniq
import Plato.Syntax.Typing
import Plato.Typing.Env as Env
import Plato.Typing.Kc.Unify
import Plato.Typing.Monad
import Plato.Typing.Zonking

newDataType :: (MonadReader ctx m, HasUniq ctx, MonadIO m) => Ident -> m (Ident, Kind)
newDataType id = (id,) <$> newKnVar

getDataType ::
        (MonadReader ctx m, HasTypEnv ctx, MonadThrow m, MonadIO m) =>
        Ident ->
        m Kind
getDataType id = zonkKind =<< Env.find id =<< getEnv =<< ask

-- asks . (Env.find @Kind) >=> zonkKind

inferDataKind ::
        (MonadReader ctx m, HasTypEnv ctx, HasUniq ctx, MonadThrow m, MonadIO m) =>
        [Ident] ->
        [(Ident, LType)] ->
        m [(Ident, Type)]
inferDataKind params constrs = do
        bnds <- forM params $ \x -> do
                kv <- newKnVar
                return (x, kv)
        constrs' <- local (modifyEnv $ Env.extendList bnds) $
                forM constrs $ \(con, body) -> do
                        body' <- checkKindStar body
                        return (con, body')
        bnds' <- mapM (\(x, kn) -> (x,) <$> zonkKind kn) bnds
        let tvs = map (Bifunctor.bimap BoundTv Just) bnds'
        let constrs'' = map (\(con, body) -> (con, AllT tvs body)) constrs'
        return constrs''

checkKindStar ::
        (MonadReader ctx m, HasTypEnv ctx, HasUniq ctx, MonadThrow m, MonadIO m) =>
        LType ->
        m LType
checkKindStar ty = checkKind ty StarK

inferKind ::
        (MonadReader ctx m, HasTypEnv ctx, HasUniq ctx, MonadThrow m, MonadIO m) =>
        LType ->
        m (LType, Kind)
inferKind ty = do
        exp_kn <- newKnVar
        ty' <- checkKind ty exp_kn >>= (zonkType `traverse`)
        res_kn <- zonkKind exp_kn
        return (ty', res_kn)

checkKind ::
        (HasCallStack, MonadReader ctx m, HasTypEnv ctx, HasUniq ctx, MonadThrow m, MonadIO m) =>
        LType ->
        Kind ->
        m LType
checkKind (L sp ty) exp_kn =
        L sp <$> case ty of
                VarT tv@(BoundTv id) -> do
                        kn <- Env.find id =<< getEnv =<< ask
                        unify sp kn exp_kn
                        return $ VarT tv
                VarT SkolemTv{} -> unreachable "Plato.KindCheck.Kc.checkKind passed SkolemTv"
                ConT tc -> do
                        kn <- Env.find tc =<< getEnv =<< ask
                        unify sp kn exp_kn
                        return $ ConT tc
                ArrT arg res -> do
                        arg' <- checkKind arg StarK
                        res' <- checkKind res StarK
                        unify sp exp_kn StarK
                        return $ ArrT arg' res'
                AllT tvs body -> do
                        quals <- forM tvs $ \tv -> do
                                kv <- newKnVar
                                return (fst tv, kv)
                        body' <- local (modifyEnv $ Env.extendList (map (Bifunctor.first unTyVar) quals)) $ checkKind body exp_kn
                        return $ AllT tvs body'
                MetaT{} -> unreachable "Plato.KindInfer.Typ.checkKind received MetaT"