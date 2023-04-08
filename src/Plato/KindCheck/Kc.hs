{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}

module Plato.KindCheck.Kc where

import Control.Exception.Safe
import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Reader
import qualified Data.Bifunctor as Bifunctor

import Plato.Common.Error
import Plato.Common.Location
import Plato.KindCheck.Unify
import Plato.KindCheck.Utils
import Plato.Syntax.Typing.Ident
import Plato.Syntax.Typing.Kind
import Plato.Syntax.Typing.Path as Path
import Plato.Syntax.Typing.Type
import Plato.Typing.Env as Env
import Plato.Typing.Monad

newDataCon :: MonadIO m => [Ident] -> Typ m [(Ident, Kind)]
newDataCon = mapM (\con -> (con,) <$> newKnVar)

getDataCon :: (MonadThrow m, MonadIO m) => Path -> Typ m Kind
getDataCon = asksM . (Env.find @Kind) >=> zonkKind

inferDataKind :: (MonadThrow m, MonadIO m) => [Ident] -> [(Ident, LType)] -> Typ m [(Ident, Type)]
inferDataKind params constrs = do
        bnds <- forM params $ \x -> do
                kv <- newKnVar
                return (x, kv)
        constrs' <- local (Env.extendList bnds) $
                forM constrs $ \(con, body) -> do
                        body' <- checkKindStar body
                        return (con, body')
        bnds' <- mapM (\(x, kn) -> (x,) <$> zonkKind kn) bnds
        let tvs = map (Bifunctor.bimap BoundTv Just) bnds'
        let constrs'' = map (\(con, body) -> (con, AllT tvs body)) constrs'
        return constrs''

checkKindStar :: (MonadThrow m, MonadIO m) => LType -> Typ m LType
checkKindStar ty = do
        (ty', kn) <- inferKind ty
        unify kn StarK
        return ty'

inferKind :: (MonadThrow m, MonadIO m) => LType -> Typ m (LType, Kind)
inferKind ty = do
        exp_kn <- newKnVar
        ty' <- checkKind ty exp_kn >>= (zonkType `traverse`)
        res_kn <- zonkKind exp_kn
        return (ty', res_kn)

checkKind :: (MonadThrow m, MonadIO m) => LType -> Kind -> Typ m LType
checkKind (L sp ty) exp_kn =
        writeErrLoc sp >> L sp <$> case ty of
                VarT tv -> do
                        kn <- asksM $ Env.find $ Path.PIdent (tyVarName tv)
                        unify kn exp_kn
                        return $ VarT tv
                ConT tc -> do
                        kn <- asksM $ Env.find tc
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
                        body' <- local (Env.extendList (map (Bifunctor.first tyVarName) binds)) $ checkKind body exp_kn
                        return $ AllT tvs body'
                AbsT x _ body -> do
                        (arg_kn, res_kn) <- unifyFun exp_kn
                        body' <- local (Env.extend x arg_kn) $ checkKind body res_kn
                        return $ AbsT x (Just arg_kn) body'
                AppT fun arg -> do
                        (fun', fun_kn) <- inferKind fun
                        (arg_kn, res_kn) <- unifyFun fun_kn
                        arg' <- checkKind arg arg_kn
                        unify res_kn exp_kn
                        return (AppT fun' arg')
                {-RecT x _ body -> do
                        kv <- newKnVar
                        body' <- local (Env.extend x kv) $ checkKind body exp_kn
                        -- unify exp_kn StarK -- temp: mutual recursive?
                        return $ RecT x (Just kv) body'-}
                SumT fields -> do
                        fields' <- forM fields $ \(con, tys) -> do
                                tys' <- forM tys $ \ty -> checkKind ty StarK
                                return (con, tys')
                        unify exp_kn StarK
                        return $ SumT fields'
                MetaT{} -> unreachable "Plato.KindInfer.Typ.checkKind received MetaT"
                RecordT{} -> unreachable "Plato.KindInfer.Typ.checkKind received RecordT"