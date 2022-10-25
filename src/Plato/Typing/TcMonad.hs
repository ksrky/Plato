{-# LANGUAGE TupleSections #-}

module Plato.Typing.TcMonad where

import Plato.Common.Error
import Plato.Common.Name
import Plato.Common.SrcLoc
import Plato.Syntax.Typing
import Plato.Typing.TcTypes

import Control.Exception.Safe
import Control.Monad
import Control.Monad.IO.Class
import Data.IORef
import Data.List
import qualified Data.Map.Strict as M

data TcEnv = TcEnv
        { uniqs :: IORef Uniq
        , var_env :: M.Map Name Sigma
        }

newtype Tc m a = Tc (TcEnv -> m a)

unTc :: Monad m => Tc m a -> (TcEnv -> m a)
unTc (Tc a) = a

instance Monad m => Functor (Tc m) where
        fmap f (Tc x) = Tc $ \env -> do
                y <- x env
                return $ f y

instance Monad m => Applicative (Tc m) where
        pure x = Tc (\_ -> return x)
        m <*> x = Tc $ \env -> do
                m' <- unTc m env
                unTc (fmap m' x) env

instance Monad m => Monad (Tc m) where
        m >>= k = Tc $ \env -> do
                v <- unTc m env
                unTc (k v) env

failTc :: MonadThrow m => Span -> String -> Tc m a
failTc sp msg = Tc $ \_ -> throwLocatedErr sp msg

check :: MonadThrow m => Bool -> Span -> String -> Tc m ()
check True _ _ = return ()
check False sp d = failTc sp d

runTc :: MonadIO m => [(Name, Sigma)] -> Tc m a -> m a
runTc binds (Tc tc) = do
        ref <- liftIO $ newIORef 0
        let env = TcEnv{uniqs = ref, var_env = M.fromList binds}
        tc env

lift :: Monad m => m a -> Tc m a
lift st = Tc (const st)

newTcRef :: MonadIO m => a -> Tc m (IORef a)
newTcRef v = lift (liftIO $ newIORef v)

readTcRef :: MonadIO m => IORef a -> Tc m a
readTcRef r = lift (liftIO $ readIORef r)

writeTcRef :: MonadIO m => IORef a -> a -> Tc m ()
writeTcRef r v = lift (liftIO $ writeIORef r v)

----------------------------------------------------------------
-- Type Environment
----------------------------------------------------------------
extendVarEnv :: Name -> Sigma -> Tc m a -> Tc m a
extendVarEnv var ty (Tc m) = Tc (m . extend)
    where
        extend env = env{var_env = M.insert var ty (var_env env)}

extendVarEnvList :: [(Name, Sigma)] -> Tc m a -> Tc m a
extendVarEnvList binds tr = foldr (uncurry extendVarEnv) tr binds

getEnv :: Monad m => Tc m (M.Map Name Sigma)
getEnv = Tc (return . var_env)

lookupVar :: MonadThrow m => Located Name -> Tc m Sigma
lookupVar (L sp x) = do
        env <- getEnv
        case M.lookup x env of
                Just ty -> return ty
                Nothing -> failTc sp $ "Not in scope: " ++ show x

--------------------------------------------------
--      Creating, reading, writing MetaTvs      --
--------------------------------------------------
newTyVar :: MonadIO m => Tc m Tau
newTyVar = MetaT <$> newMetaTyVar

newMetaTyVar :: MonadIO m => Tc m MetaTv
newMetaTyVar = do
        uniq <- newUnique
        tref <- newTcRef Nothing
        return (Meta uniq tref)

newSkolemTyVar :: MonadIO m => TyVar -> Tc m TyVar
newSkolemTyVar tv = SkolemTv (tyVarName tv) <$> newUnique

readTv :: MonadIO m => MetaTv -> Tc m (Maybe Tau)
readTv (Meta _ ref) = readTcRef ref

writeTv :: MonadIO m => MetaTv -> Tau -> Tc m ()
writeTv (Meta _ ref) ty = writeTcRef ref (Just ty)

newUnique :: MonadIO m => Tc m Uniq
newUnique =
        Tc
                ( \TcEnv{uniqs = ref} -> liftIO $ do
                        uniq <- readIORef ref
                        writeIORef ref (uniq + 1)
                        return uniq
                )

----------------------------------------------------------------
-- Instantiation
----------------------------------------------------------------
instantiate :: MonadIO m => Sigma -> Tc m (Located Expr -> Located Expr, Rho)
instantiate (AllT tvs ty) = do
        tvs' <- mapM (const newMetaTyVar) tvs
        let coercion = \e -> cL e $ TAppE e (map MetaT tvs')
        return (coercion, substTy (map (unLoc . fst) tvs) (map MetaT tvs') (unLoc ty))
instantiate ty = return (id, ty)

skolemise :: MonadIO m => Sigma -> Tc m (Located Expr -> Located Expr, [Located TyVar], Rho)
skolemise (AllT tvs ty) = do
        sks1 <- mapM (\(tv, _) -> newSkolemTyVar `traverse` tv) tvs
        (coercion, sks2, ty') <- skolemise (substTy (map (unLoc . fst) tvs) (map VarT sks1) (unLoc ty))
        let coercion' = \e ->
                cL e $
                        TAbsE
                                (map ((tyVarName <$>) . fst) tvs)
                                (coercion $ cL e $ TAppE e (map (VarT . fst) tvs))
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
quantify :: MonadIO m => [MetaTv] -> Rho -> Tc m Sigma
quantify tvs ty = do
        mapM_ bind (tvs `zip` new_bndrs)
        ty' <- zonkType ty
        return (AllT (map (\tv -> (noLoc tv, Nothing)) new_bndrs) (noLoc ty'))
    where
        used_bndrs = tyVarBndrs ty
        new_bndrs = take (length tvs) (allBinders \\ used_bndrs)
        bind (tv, name) = writeTv tv (VarT $ noLoc name)

allBinders :: [TyVar]
allBinders =
        [BoundTv $ str2tyvarName [x] | x <- ['a' .. 'z']]
        ++ [BoundTv (str2tyvarName (x : show i)) | i <- [1 :: Integer ..], x <- ['a' .. 'z']]

getEnvTypes :: Monad m => Tc m [Type]
getEnvTypes = M.elems <$> getEnv

getMetaTvs :: MonadIO m => [Type] -> Tc m [MetaTv]
getMetaTvs tys = do
        tys' <- mapM zonkType tys
        return (metaTvs tys')

getFreeTyVars :: MonadIO m => [Type] -> Tc m [TyVar]
getFreeTyVars tys = do
        tys' <- mapM zonkType tys
        return (freeTyVars tys')

----------------------------------------------------------------
-- Zonking
----------------------------------------------------------------
zonkType :: MonadIO m => Type -> Tc m Type
zonkType (VarT x) = return (VarT x)
zonkType (ConT x) = return (ConT x)
zonkType (AllT ns ty) = do
        ty' <- zonkType `traverse` ty
        return (AllT ns ty')
zonkType (AbsT x mkn ty) = AbsT x mkn <$> zonkType `traverse` ty
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

zonkExpr :: MonadIO m => Expr -> Tc m Expr
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
unify :: (MonadIO m, MonadThrow m) => Tau -> Tau -> Tc m ()
unify ty1 ty2 | badType ty1 || badType ty2 = error ""
unify (VarT tv1) (VarT tv2) | unLoc tv1 == unLoc tv2 = return ()
unify (MetaT tv1) (MetaT tv2) | tv1 == tv2 = return ()
unify (MetaT tv) ty = unifyVar tv ty
unify ty (MetaT tv) = unifyVar tv ty
unify (ArrT arg1 res1) (ArrT arg2 res2) = do
        unify (unLoc arg1) (unLoc arg2)
        unify (unLoc res1) (unLoc res2)
unify (AppT fun1 arg1) (AppT fun2 arg2) = do
        unify (unLoc fun1) (unLoc fun2)
        unify (unLoc arg1) (unLoc arg2)
unify (ConT tc1) (ConT tc2) | unLoc tc1 == unLoc tc2 = return ()
unify ty1 ty2 = lift $ throwPlainErr $ "Cannot unify types: " ++ show ty1 ++ ", " ++ show ty2 --tmp: location

unifyVar :: (MonadIO m, MonadThrow m) => MetaTv -> Tau -> Tc m ()
unifyVar tv1 ty2 = do
        mb_ty1 <- readTv tv1
        case mb_ty1 of
                Just ty1 -> unify ty1 ty2
                Nothing -> unifyUnboundVar tv1 ty2

unifyUnboundVar :: (MonadIO m, MonadThrow m) => MetaTv -> Tau -> Tc m ()
unifyUnboundVar tv1 ty2@(MetaT tv2) = do
        mb_ty2 <- readTv tv2
        case mb_ty2 of
                Just ty2' -> unify (MetaT tv1) ty2'
                Nothing -> writeTv tv1 ty2
unifyUnboundVar tv1 ty2 = do
        tvs2 <- getMetaTvs [ty2]
        if tv1 `elem` tvs2
                then occursCheckErr tv1 ty2
                else writeTv tv1 ty2

unifyFun :: (MonadIO m, MonadThrow m) => Rho -> Tc m (Sigma, Rho)
unifyFun (ArrT arg res) = return (unLoc arg, unLoc res)
unifyFun tau = do
        arg_ty <- newTyVar
        res_ty <- newTyVar
        unify tau (ArrT (noLoc arg_ty) (noLoc res_ty))
        return (arg_ty, res_ty)

occursCheckErr :: MonadThrow m => MetaTv -> Tau -> Tc m ()
occursCheckErr tv ty = lift $ throwPlainErr "Occurs check fail" --tmp

badType :: Tau -> Bool
badType (VarT (L _ (BoundTv _))) = True
badType _ = False