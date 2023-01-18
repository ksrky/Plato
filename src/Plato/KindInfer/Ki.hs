module Plato.KindInfer.Ki where

import Plato.KindInfer.Unify
import Plato.Syntax.Typing
import Plato.Types.Error
import Plato.Types.Location

import Control.Exception.Safe
import Control.Monad
import Control.Monad.IO.Class
import qualified Data.Bifunctor as Bifunctor
import Plato.KindInfer.Monad
import Plato.KindInfer.Utils

{-inferKind :: (MonadThrow m, MonadIO m) => KnEnv -> Type -> m (Type, Kind)
inferKind knenv ty = runKi knenv $ do
        (ty', kn) <- infer ty
        ty'' <- zonkType ty'
        kn' <- zonkKind kn
        return (ty'', kn')

checkKindStar :: (MonadThrow m, MonadIO m) => KnEnv -> Span -> Type -> m Type
checkKindStar knenv sp ty = runKi knenv $ do
        (ty', kn) <- infer ty
        ty'' <- zonkType ty'
        kn' <- zonkKind kn
        when (kn' /= StarK) $ lift $ throwLocErr sp $ sep [pretty ty, " doesn't have Kind *"]
        return ty''-}

inferKind :: (MonadThrow m, MonadIO m) => LType -> Ki m (LType, Kind)
inferKind ty = do
        exp_kn <- newKnVar
        ty' <- checkKind ty exp_kn >>= (zonkType `traverse`)
        res_kn <- zonkKind exp_kn
        return (ty', res_kn)

checkKind :: (MonadThrow m, MonadIO m) => LType -> Kind -> Ki m LType
checkKind (L sp ty) exp_kn =
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
                        arg' <- checkKind arg StarK
                        res' <- checkKind res StarK
                        unify exp_kn StarK
                        return $ ArrT arg' res'
                AllT tvs body -> do
                        binds <- forM tvs $ \tv -> do
                                kv <- newKnVar
                                return (fst tv, kv)
                        body' <- extendEnvList (map (Bifunctor.first tyVarName) binds) $ checkKind body exp_kn
                        return $ AllT tvs body'
                AbsT x _ body -> do
                        (arg_kn, res_kn) <- unifyFun exp_kn
                        body' <- extendEnv x arg_kn $ checkKind body res_kn
                        return $ AbsT x (Just arg_kn) body'
                AppT fun arg -> do
                        (fun', fun_kn) <- inferKind fun
                        (arg_kn, res_kn) <- unifyFun fun_kn
                        arg' <- checkKind arg arg_kn
                        unify res_kn exp_kn
                        return (AppT fun' arg')
                RecT x _ body -> do
                        kv <- newKnVar
                        body' <- extendEnv x kv $ checkKind body exp_kn
                        -- unify exp_kn StarK -- temp: mutual recursive?
                        return $ RecT x (Just kv) body'
                SumT fields -> do
                        fields' <- forM fields $ \(con, tys) -> do
                                tys' <- forM tys $ \ty -> checkKind ty StarK
                                return (con, tys')
                        unify exp_kn StarK
                        return $ SumT fields'
                MetaT{} -> unreachable "Plato.KindInfer.Ki.checkKind received MetaT"
                RecordT{} -> unreachable "Plato.KindInfer.Ki.checkKind received RecordT"