{-# LANGUAGE TupleSections #-}

module Plato.KindCheck.Kc where

import Control.Exception.Safe
import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Reader
import qualified Data.Bifunctor as Bifunctor

import Plato.Common.Error
import Plato.Common.Global
import Plato.Common.Ident
import Plato.Common.Location
import Plato.Common.Path as Path
import Plato.KindCheck.Unify
import Plato.Syntax.Typing
import Plato.Typing.Env as Env
import Plato.Typing.Monad
import Plato.Typing.Zonking

newDataCon :: (MonadReader ctx m, HasUnique ctx, MonadIO m) => [Ident] -> m [(Ident, Kind)]
newDataCon = mapM (\con -> (con,) <$> newKnVar)

getDataCon ::
        (MonadReader ctx m, HasEnv ctx, MonadThrow m, MonadIO m) =>
        Path ->
        m Kind
getDataCon p = zonkKind =<< Env.find p =<< getEnv =<< ask

-- asks . (Env.find @Kind) >=> zonkKind

inferDataKind ::
        (MonadReader ctx m, HasEnv ctx, HasUnique ctx, MonadThrow m, MonadIO m) =>
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
        (MonadReader ctx m, HasEnv ctx, HasUnique ctx, MonadThrow m, MonadIO m) =>
        LType ->
        m LType
checkKindStar ty = do
        (ty', kn) <- inferKind ty
        unify NoSpan kn StarK
        return ty'

inferKind ::
        (MonadReader ctx m, HasEnv ctx, HasUnique ctx, MonadThrow m, MonadIO m) =>
        LType ->
        m (LType, Kind)
inferKind ty = do
        exp_kn <- newKnVar
        ty' <- checkKind ty exp_kn >>= (zonkType `traverse`)
        res_kn <- zonkKind exp_kn
        return (ty', res_kn)

checkKind ::
        (MonadReader ctx m, HasEnv ctx, HasUnique ctx, MonadThrow m, MonadIO m) =>
        LType ->
        Kind ->
        m LType
checkKind (L sp ty) exp_kn =
        L sp <$> case ty of
                VarT tv@(BoundTv id) -> do
                        kn <- Env.find (Path.PIdent id) =<< getEnv =<< ask
                        unify sp kn exp_kn
                        return $ VarT tv
                VarT _ -> unreachable "Plato.KindCheck.Kc.checkKind passed SkolemTv"
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
                AbsT x _ body -> do
                        (arg_kn, res_kn) <- unifyFun sp exp_kn
                        body' <- local (modifyEnv $ Env.extend x arg_kn) $ checkKind body res_kn
                        return $ AbsT x (Just arg_kn) body'
                AppT fun arg -> do
                        (fun', fun_kn) <- inferKind fun
                        (arg_kn, res_kn) <- unifyFun sp fun_kn
                        arg' <- checkKind arg arg_kn
                        unify sp res_kn exp_kn
                        return (AppT fun' arg')
                {-RecT x _ body -> do
                        kv <- newKnVar
                        body' <- local (Env.extend x kv) $ checkKind body exp_kn
                        -- unify exp_kn StarK -- temp: mutual recursive?
                        return $ RecT x (Just kv) body'-}
                {-} SumT fields -> do
                         fields' <- forM fields $ \(con, tys) -> do
                                 tys' <- forM tys $ \ty -> checkKind ty StarK
                                 return (con, tys')
                         unify sp exp_kn StarK
                         return $ SumT fields'
                 RecordT{} -> unreachable "Plato.KindInfer.Typ.checkKind received RecordT"-}
                MetaT{} -> unreachable "Plato.KindInfer.Typ.checkKind received MetaT"