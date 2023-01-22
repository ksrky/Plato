module Plato.KindInfer.Ki where

import Plato.Common.Error
import Plato.Common.Location
import Plato.KindInfer.Unify
import Plato.Syntax.Typing

import Control.Exception.Safe
import Control.Monad
import Control.Monad.IO.Class
import qualified Data.Bifunctor as Bifunctor
import Plato.KindInfer.Monad
import Plato.KindInfer.Utils

inferKind :: (MonadThrow m, MonadIO m) => KnEnv -> LType -> m (LType, Kind)
inferKind knenv ty = runKi (infer ty) =<< initEnv knenv

checkKindStar :: (MonadThrow m, MonadIO m) => KnEnv -> LType -> m LType
checkKindStar knenv ty =
        runKi
                ( do
                        (ty', kn) <- infer ty
                        unify kn StarK
                        return ty'
                )
                =<< initEnv knenv

infer :: (MonadThrow m, MonadIO m) => LType -> Ki m (LType, Kind)
infer ty = do
        exp_kn <- newKnVar
        ty' <- check ty exp_kn >>= (zonkType `traverse`)
        res_kn <- zonkKind exp_kn
        return (ty', res_kn)

check :: (MonadThrow m, MonadIO m) => LType -> Kind -> Ki m LType
check (L sp ty) exp_kn =
        writeErrLoc sp >> L sp <$> case ty of
                VarT tv -> do
                        kn <- lookupEnv (tyVarName tv)
                        unify kn exp_kn
                        return $ VarT tv
                ConT tc -> do
                        kn <- lookupEnv tc
                        unify kn exp_kn
                        return $ ConT tc
                ArrT arg res -> do
                        arg' <- check arg StarK
                        res' <- check res StarK
                        unify exp_kn StarK
                        return $ ArrT arg' res'
                AllT tvs body -> do
                        binds <- forM tvs $ \tv -> do
                                kv <- newKnVar
                                return (fst tv, kv)
                        body' <- extendEnvList (map (Bifunctor.first tyVarName) binds) $ check body exp_kn
                        return $ AllT tvs body'
                AbsT x _ body -> do
                        (arg_kn, res_kn) <- unifyFun exp_kn
                        body' <- extendEnv x arg_kn $ check body res_kn
                        return $ AbsT x (Just arg_kn) body'
                AppT fun arg -> do
                        (fun', fun_kn) <- infer fun
                        (arg_kn, res_kn) <- unifyFun fun_kn
                        arg' <- check arg arg_kn
                        unify res_kn exp_kn
                        return (AppT fun' arg')
                RecT x _ body -> do
                        kv <- newKnVar
                        body' <- extendEnv x kv $ check body exp_kn
                        -- unify exp_kn StarK -- temp: mutual recursive?
                        return $ RecT x (Just kv) body'
                SumT fields -> do
                        fields' <- forM fields $ \(con, tys) -> do
                                tys' <- forM tys $ \ty -> check ty StarK
                                return (con, tys')
                        unify exp_kn StarK
                        return $ SumT fields'
                RefT{} -> undefined
                MetaT{} -> unreachable "Plato.KindInfer.Ki.check received MetaT"
                RecordT{} -> unreachable "Plato.KindInfer.Ki.check received RecordT"