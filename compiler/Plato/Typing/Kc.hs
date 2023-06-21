module Plato.Typing.Kc (
        checkKindStar,
        inferDataKind,
        inferKind,
        checkKind,
) where

import Control.Exception.Safe
import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Reader
import GHC.Stack

import Plato.Common.Error
import Plato.Common.Ident
import Plato.Common.Location
import Plato.Common.Uniq
import Plato.Syntax.Typing
import Plato.Typing.Env
import Plato.Typing.Kc.Unify
import Plato.Typing.Monad
import Plato.Typing.Zonking

-- asks . (Env.find @Kind) >=> zonk

inferDataKind ::
        (MonadReader ctx m, HasTypEnv ctx, HasUniq ctx, MonadThrow m, MonadIO m) =>
        [Ident] ->
        [(Ident, LType)] ->
        m [(Ident, Type)]
inferDataKind params constrs = do
        bnds <- forM params $ \x -> do
                kv <- newKnVar
                return (x, kv)
        local (modifyEnv $ extendList bnds) $ forM_ constrs $ \(_, body) -> checkKindStar body
        bnds' <- mapM (\(x, kn) -> (x,) <$> zonk kn) bnds
        let qnts = map (\(id, kn) -> (BoundTv id, kn)) bnds'
            constrs'' = map (\(con, body) -> (con, AllT qnts body)) constrs
        return constrs''

checkKindStar ::
        (MonadReader ctx m, HasTypEnv ctx, HasUniq ctx, MonadThrow m, MonadIO m) => LType -> m ()
checkKindStar ty = checkKind ty StarK

inferKind ::
        (MonadReader ctx m, HasTypEnv ctx, HasUniq ctx, MonadThrow m, MonadIO m) =>
        LType ->
        m (LType, Kind)
inferKind ty = do
        exp_kn <- newKnVar
        checkKind ty exp_kn
        ty' <- zonk `traverse` ty
        res_kn <- zonk exp_kn
        return (ty', res_kn)

checkKind ::
        (HasCallStack, MonadReader ctx m, HasTypEnv ctx, HasUniq ctx, MonadThrow m, MonadIO m) =>
        LType ->
        Kind ->
        m ()
checkKind (L sp ty) exp_kn = case ty of
        VarT (BoundTv id) -> do
                kn <- zonk =<< find id =<< getEnv =<< ask
                unify sp kn exp_kn
        VarT SkolemTv{} -> unreachable "Plato.KindCheck.Kc.checkKind passed SkolemTv"
        ConT tc -> do
                kn <- zonk =<< find tc =<< getEnv =<< ask
                unify sp kn exp_kn
        ArrT arg res -> do
                checkKindStar arg
                checkKindStar res
                unify sp exp_kn StarK
        AllT qnts body ->
                local (modifyEnv $ extendList (map (\(tv, kn) -> (unTyVar tv, kn)) qnts)) $
                        checkKind body exp_kn
        AppT fun arg -> do
                arg_kn <- newKnVar
                checkKind fun (ArrK arg_kn exp_kn)
                checkKind arg arg_kn
        MetaT{} -> unreachable "Plato.KindInfer.Typ.checkKind received MetaT"