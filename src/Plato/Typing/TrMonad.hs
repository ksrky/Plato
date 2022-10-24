{-# LANGUAGE TupleSections #-}

module Plato.Typing.TrMonad where

import Plato.Common.Error
import Plato.Common.Name
import Plato.Common.SrcLoc
import Plato.Syntax.Typing
import Plato.Typing.TrTypes

import Control.Exception.Safe
import Control.Monad
import Control.Monad.IO.Class
import Data.IORef
import Data.List
import qualified Data.Map.Strict as M

data TrEnv = TrEnv
        { uniqs :: IORef Uniq
        , var_env :: M.Map Name Sigma
        }

newtype Tr m a = Tr (TrEnv -> m a)

unTr :: Monad m => Tr m a -> (TrEnv -> m a)
unTr (Tr a) = a

instance Monad m => Functor (Tr m) where
        fmap f (Tr x) = Tr $ \env -> do
                y <- x env
                return $ f y

instance Monad m => Applicative (Tr m) where
        pure x = Tr (\_ -> return x)
        m <*> x = Tr $ \env -> do
                m' <- unTr m env
                unTr (fmap m' x) env

instance Monad m => Monad (Tr m) where
        m >>= k = Tr $ \env -> do
                v <- unTr m env
                unTr (k v) env

failTr :: MonadThrow m => Span -> String -> Tr m a
failTr sp msg = Tr $ \_ -> throwLocatedErr sp msg

check :: MonadThrow m => Bool -> Span -> String -> Tr m ()
check True _ _ = return ()
check False sp d = failTr sp d

runTr :: MonadIO m => [(Name, Sigma)] -> Tr m a -> m a
runTr binds (Tr tr) = do
        ref <- liftIO $ newIORef 0
        let env = TrEnv{uniqs = ref, var_env = M.fromList binds}
        tr env

lift :: Monad m => m a -> Tr m a
lift st = Tr (const st)

newTrRef :: MonadIO m => a -> Tr m (IORef a)
newTrRef v = lift (liftIO $ newIORef v)

readTrRef :: MonadIO m => IORef a -> Tr m a
readTrRef r = lift (liftIO $ readIORef r)

writeTrRef :: MonadIO m => IORef a -> a -> Tr m ()
writeTrRef r v = lift (liftIO $ writeIORef r v)

----------------------------------------------------------------
-- Type Environment
----------------------------------------------------------------
extendVarEnv :: Name -> Sigma -> Tr m a -> Tr m a
extendVarEnv var ty (Tr m) = Tr (m . extend)
    where
        extend env = env{var_env = M.insert var ty (var_env env)}

extendVarEnvList :: [(Name, Sigma)] -> Tr m a -> Tr m a
extendVarEnvList binds tr = foldr (uncurry extendVarEnv) tr binds

getEnv :: Monad m => Tr m (M.Map Name Sigma)
getEnv = Tr (return . var_env)

lookupVar :: MonadThrow m => Located Name -> Tr m Sigma
lookupVar (L sp x) = do
        env <- getEnv
        case M.lookup x env of
                Just ty -> return ty
                Nothing -> failTr sp $ "Not in scope: " ++ show x

--------------------------------------------------
--      Creating, reading, writing MetaTvs      --
--------------------------------------------------
newTyVar :: MonadIO m => Tr m Tau
newTyVar = MetaT <$> newMetaTyVar

newMetaTyVar :: MonadIO m => Tr m MetaTv
newMetaTyVar = do
        uniq <- newUnique
        tref <- newTrRef Nothing
        return (Meta uniq tref)

newSkolemTyVar :: MonadIO m => TyVar -> Tr m TyVar
newSkolemTyVar tv = SkolemTv (tyVarName tv) <$> newUnique

readTv :: MonadIO m => MetaTv -> Tr m (Maybe Tau)
readTv (Meta _ ref) = readTrRef ref

writeTv :: MonadIO m => MetaTv -> Tau -> Tr m ()
writeTv (Meta _ ref) ty = writeTrRef ref (Just ty)

newUnique :: MonadIO m => Tr m Uniq
newUnique =
        Tr
                ( \TrEnv{uniqs = ref} -> liftIO $ do
                        uniq <- readIORef ref
                        writeIORef ref (uniq + 1)
                        return uniq
                )

----------------------------------------------------------------
-- Instantiation
----------------------------------------------------------------
instantiate :: MonadIO m => Sigma -> Tr m (Located Expr -> Located Expr, Rho)
instantiate (AllT tvs ty) = do
        tvs' <- mapM (const newMetaTyVar) tvs
        let coercion = \e -> cL e $ TAppE e (map MetaT tvs')
        return (coercion, substTy (map unLoc tvs) (map MetaT tvs') (unLoc ty))
instantiate ty = return (id, ty)

skolemise :: MonadIO m => Sigma -> Tr m (Located Expr -> Located Expr, [Located TyVar], Rho)
skolemise (AllT tvs ty) = do
        sks1 <- mapM newSkolemTyVar `traverse` tvs
        (coercion, sks2, ty') <- skolemise (substTy (map unLoc tvs) (map VarT sks1) (unLoc ty))
        let coercion' = \e ->
                cL e $
                        TAbsE
                                (map (tyVarName <$>) tvs)
                                (coercion $ cL e $ TAppE e (map VarT tvs))
        return (if null sks2 then id else coercion', sks1 ++ sks2, ty')
skolemise (ArrT arg_ty res_ty) = do
        (coercion, sks, res_ty') <- skolemise $ unLoc res_ty
        let coercion' = \e ->
                cL e $
                        AbsE
                                (noLoc $ str2varName "x")
                                (Just $ unLoc arg_ty)
                                ( coercion $
                                        cL e $
                                                TAbsE
                                                        (map (tyVarName <$>) sks)
                                                        (noLoc $ AppE (noLoc $ TAppE e (map VarT sks)) (noLoc $ VarE $ noLoc $ str2varName "x"))
                                )
        return (if null sks then coercion else coercion', sks, ArrT arg_ty (L (getSpan res_ty) res_ty'))
skolemise ty = return (id, [], ty)

----------------------------------------------------------------
-- Quantification
----------------------------------------------------------------
quantify :: MonadIO m => [MetaTv] -> Rho -> Tr m Sigma
quantify tvs ty = do
        mapM_ bind (tvs `zip` new_bndrs)
        ty' <- zonkType ty
        return (AllT (map noLoc new_bndrs) (noLoc ty'))
    where
        used_bndrs = tyVarBndrs ty
        new_bndrs = take (length tvs) (allBinders \\ used_bndrs)
        bind (tv, name) = writeTv tv (VarT $ noLoc name)

allBinders :: [TyVar]
allBinders =
        [BoundTv $ str2tyvarName [x] | x <- ['a' .. 'z']]
        ++ [BoundTv (str2tyvarName (x : show i)) | i <- [1 :: Integer ..], x <- ['a' .. 'z']]

getEnvTypes :: Monad m => Tr m [Type]
getEnvTypes = M.elems <$> getEnv

getMetaTyVars :: MonadIO m => [Type] -> Tr m [MetaTv]
getMetaTyVars tys = do
        tys' <- mapM zonkType tys
        return (metaTvs tys')

getFreeTyVars :: MonadIO m => [Type] -> Tr m [TyVar]
getFreeTyVars tys = do
        tys' <- mapM zonkType tys
        return (freeTyVars tys')

----------------------------------------------------------------
-- Zonking
----------------------------------------------------------------
zonkType :: MonadIO m => Type -> Tr m Type
zonkType (VarT x) = return (VarT x)
zonkType (ConT x) = return (ConT x)
zonkType (AllT ns ty) = do
        ty' <- zonkType `traverse` ty
        return (AllT ns ty')
zonkType (AbsT x ty) = AbsT x <$> (zonkType `traverse` ty)
zonkType (AppT fun arg) = do
        fun' <- zonkType `traverse` fun
        arg' <- zonkType `traverse` arg
        return (AppT fun' arg')
zonkType (ArrT arg res) = do
        arg' <- zonkType `traverse` arg
        res' <- zonkType `traverse` res
        return (ArrT arg' res')
zonkType (MetaT tv) = do
        mb_ty <- readTv tv
        case mb_ty of
                Nothing -> return (MetaT tv)
                Just ty -> do
                        ty' <- zonkType ty
                        writeTv tv ty'
                        return ty'
zonkType _ = unreachable "AbsType, RecordType, RecType, SumType"

zonkExpr :: MonadIO m => Expr -> Tr m Expr
zonkExpr (AbsE var mty expr) = AbsE var <$> (zonkType `traverse` mty) <*> zonkExpr `traverse` expr
zonkExpr (AppE fun arg) = AppE <$> zonkExpr `traverse` fun <*> zonkExpr `traverse` arg
zonkExpr (TAbsE ns expr) = TAbsE ns <$> zonkExpr `traverse` expr
zonkExpr (TAppE e tys) = TAppE e <$> mapM zonkType tys
zonkExpr (LetE decs body) =
        LetE
                <$> forM decs (\(FD var e ty) -> FD var <$> zonkExpr `traverse` e <*> zonkType `traverse` ty)
                        <*> zonkExpr `traverse` body
zonkExpr (CaseE e mty alts) =
        CaseE
                <$> zonkExpr `traverse` e
                <*> zonkType `traverse` mty
                <*> forM alts (\(pat, body) -> (pat,) <$> zonkExpr `traverse` body)
zonkExpr (AnnE e ty) = AnnE <$> zonkExpr `traverse` e <*> zonkType `traverse` ty
zonkExpr expr = return expr

----------------------------------------------------------------
-- Unification
----------------------------------------------------------------
unify :: (MonadIO m, MonadThrow m) => Tau -> Tau -> Tr m ()
unify ty1 ty2 | badType ty1 || badType ty2 = error ""
unify (VarT tv1) (VarT tv2) | tv1 == tv2 = return ()
unify (MetaT tv1) (MetaT tv2) | tv1 == tv2 = return ()
unify (MetaT tv) ty = unifyVar tv ty
unify ty (MetaT tv) = unifyVar tv ty
unify (ArrT arg1 res1) (ArrT arg2 res2) = do
        unify (unLoc arg1) (unLoc arg2)
        unify (unLoc res1) (unLoc res2)
unify (AppT fun1 arg1) (AppT fun2 arg2) = do
        unify (unLoc fun1) (unLoc fun2)
        unify (unLoc arg1) (unLoc arg2)
unify (ConT tc1) (ConT tc2) | tc1 == tc2 = return ()
unify ty1 ty2 = lift $ throwPlainErr $ "Cannot unify types: " ++ show ty1 ++ ", " ++ show ty2 --tmp: location

unifyVar :: (MonadIO m, MonadThrow m) => MetaTv -> Tau -> Tr m ()
unifyVar tv1 ty2 = do
        mb_ty1 <- readTv tv1
        case mb_ty1 of
                Just ty1 -> unify ty1 ty2
                Nothing -> unifyUnboundVar tv1 ty2

unifyUnboundVar :: (MonadIO m, MonadThrow m) => MetaTv -> Tau -> Tr m ()
unifyUnboundVar tv1 ty2@(MetaT tv2) = do
        mb_ty2 <- readTv tv2
        case mb_ty2 of
                Just ty2' -> unify (MetaT tv1) ty2'
                Nothing -> writeTv tv1 ty2
unifyUnboundVar tv1 ty2 = do
        tvs2 <- getMetaTyVars [ty2]
        if tv1 `elem` tvs2
                then occursCheckErr tv1 ty2
                else writeTv tv1 ty2

unifyFun :: (MonadIO m, MonadThrow m) => Rho -> Tr m (Sigma, Rho)
unifyFun (ArrT arg res) = return (unLoc arg, unLoc res)
unifyFun tau = do
        arg_ty <- newTyVar
        res_ty <- newTyVar
        unify tau (ArrT (noLoc arg_ty) (noLoc res_ty))
        return (arg_ty, res_ty)

occursCheckErr :: MonadThrow m => MetaTv -> Tau -> Tr m ()
occursCheckErr tv ty = lift $ throwPlainErr "Occurs check fail" --tmp

badType :: Tau -> Bool
badType (VarT (L _ (BoundTv _))) = True
badType _ = False