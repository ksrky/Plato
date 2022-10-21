module Plato.Typing.Monad where

import Plato.Common.Error
import Plato.Common.Name
import Plato.Common.SrcLoc
import Plato.Syntax.Typing
import Plato.Typing.Error
import Plato.Typing.Types

import Control.Exception.Safe
import Control.Monad.IO.Class
import Data.IORef
import Data.List
import qualified Data.Map.Strict as M

data TypEnv = TypEnv
        { uniqs :: IORef Uniq
        , var_env :: M.Map Name Sigma
        }

newtype Typ m a = Typ (TypEnv -> m a)

unTyp :: Monad m => Typ m a -> (TypEnv -> m a)
unTyp (Typ a) = a

instance Monad m => Functor (Typ m) where
        fmap f (Typ x) = Typ $ \env -> do
                y <- x env
                return $ f y

instance Monad m => Applicative (Typ m) where
        pure x = Typ (\_ -> return x)
        m <*> x = Typ $ \env -> do
                m' <- unTyp m env
                unTyp (fmap m' x) env

instance Monad m => Monad (Typ m) where
        m >>= k = Typ $ \env -> do
                v <- unTyp m env
                unTyp (k v) env

failTyp :: MonadThrow m => Span -> String -> Typ m a
failTyp sp msg = Typ $ \_ -> throwLocatedErr sp msg

check :: MonadThrow m => Bool -> Span -> String -> Typ m ()
check True _ _ = return ()
check False sp d = failTyp sp d

runTyp :: MonadIO m => [(Name, Sigma)] -> Typ m a -> m a
runTyp binds (Typ typ) = do
        ref <- liftIO $ newIORef 0
        let env = TypEnv{uniqs = ref, var_env = M.fromList binds}
        typ env

lift :: Monad m => m a -> Typ m a
lift st = Typ (const st)

newTypRef :: MonadIO m => a -> Typ m (IORef a)
newTypRef v = lift (liftIO $ newIORef v)

readTypRef :: MonadIO m => IORef a -> Typ m a
readTypRef r = lift (liftIO $ readIORef r)

writeTypRef :: MonadIO m => IORef a -> a -> Typ m ()
writeTypRef r v = lift (liftIO $ writeIORef r v)

----------------------------------------------------------------
-- Type Environment
----------------------------------------------------------------
extendVarEnv :: Name -> Sigma -> Typ m a -> Typ m a
extendVarEnv var ty (Typ m) = Typ (m . extend)
    where
        extend env = env{var_env = M.insert var ty (var_env env)}

extendVarEnvList :: [(Name, Sigma)] -> Typ m a -> Typ m a
extendVarEnvList binds typ = foldr (uncurry extendVarEnv) typ binds

getEnv :: Monad m => Typ m (M.Map Name Sigma)
getEnv = Typ (return . var_env)

lookupVar :: MonadThrow m => Located Name -> Typ m Sigma
lookupVar (L sp x) = do
        env <- getEnv
        case M.lookup x env of
                Just ty -> return ty
                Nothing -> failTyp sp $ "Not in scope: " ++ show x

--------------------------------------------------
--      Creating, reading, writing MetaTvs      --
--------------------------------------------------
newTyVar :: MonadIO m => Typ m Tau
newTyVar = MetaT <$> newMetaTyVar

newMetaTyVar :: MonadIO m => Typ m MetaTv
newMetaTyVar = do
        uniq <- newUnique
        tref <- newTypRef Nothing
        return (Meta uniq tref)

newSkolemTyVar :: MonadIO m => TyVar -> Typ m TyVar
newSkolemTyVar tv = SkolemTv (tyVarName tv) <$> newUnique

readTv :: MonadIO m => MetaTv -> Typ m (Maybe Tau)
readTv (Meta _ ref) = readTypRef ref

writeTv :: MonadIO m => MetaTv -> Tau -> Typ m ()
writeTv (Meta _ ref) ty = writeTypRef ref (Just ty)

newUnique :: MonadIO m => Typ m Uniq
newUnique =
        Typ
                ( \TypEnv{uniqs = ref} -> liftIO $ do
                        uniq <- readIORef ref
                        writeIORef ref (uniq + 1)
                        return uniq
                )

----------------------------------------------------------------
-- Instantiation
----------------------------------------------------------------
instantiate :: MonadIO m => Sigma -> Typ m Rho
instantiate (AllT tvs ty) = do
        tvs' <- mapM (const newMetaTyVar) tvs
        return (substTy (map unLoc tvs) (map MetaT tvs') (unLoc ty))
instantiate ty = return ty

skolemise :: MonadIO m => Sigma -> Typ m ([TyVar], Rho, Expr -> Expr)
skolemise (AllT tvs ty) = do
        sks1 <- mapM (newSkolemTyVar . unLoc) tvs
        (sks2, ty', coercion) <- skolemise (substTy (map unLoc tvs) (map (VarT . noLoc) sks1) (unLoc ty))
        let coercion' = \x -> TAbsE (map (tyVarName . unLoc) tvs) (noLoc $ coercion $ TAppE (noLoc x) (map VarT tvs))
        return (sks1 ++ sks2, ty', coercion')
skolemise (ArrT arg_ty res_ty) = do
        (sks, res_ty', coercion) <- skolemise $ unLoc res_ty
        let coercion' = \x ->
                AbsE
                        (noLoc $ str2varName "x")
                        (Just $ unLoc arg_ty)
                        ( noLoc $
                                coercion $
                                        TAbsE
                                                (map tyVarName sks)
                                                (noLoc $ AppE (noLoc $ TAppE (noLoc x) (map (VarT . noLoc) sks)) (noLoc $ VarE $ noLoc $ str2varName "x"))
                        )
        return (sks, ArrT arg_ty (L (getSpan res_ty) res_ty'), coercion')
skolemise ty = return ([], ty, id)

----------------------------------------------------------------
-- Quantification
----------------------------------------------------------------
quantify :: MonadIO m => [MetaTv] -> Rho -> Typ m Sigma
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

getEnvTypes :: Monad m => Typ m [Type]
getEnvTypes = M.elems <$> getEnv

getMetaTyVars :: MonadIO m => [Type] -> Typ m [MetaTv]
getMetaTyVars tys = do
        tys' <- mapM zonkType tys
        return (metaTvs tys')

getFreeTyVars :: MonadIO m => [Type] -> Typ m [TyVar]
getFreeTyVars tys = do
        tys' <- mapM zonkType tys
        return (freeTyVars tys')

----------------------------------------------------------------
-- Zonking
----------------------------------------------------------------
zonkType :: MonadIO m => Type -> Typ m Type
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

----------------------------------------------------------------
-- Unification
----------------------------------------------------------------
unify :: (MonadIO m, MonadThrow m) => Tau -> Tau -> Typ m ()
unify ty1 ty2 | badType ty1 || badType ty2 = error ""
unify (VarT tv1) (VarT tv2) | tv1 == tv2 = return ()
unify (MetaT tv1) (MetaT tv2) | tv1 == tv2 = return ()
unify (MetaT tv) ty = unifyVar tv ty
unify ty (MetaT tv) = unifyVar tv ty
unify (ArrT arg1 res1) (ArrT arg2 res2) = do
        unify (unLoc arg1) (unLoc arg2)
        unify (unLoc res1) (unLoc res2)
unify (ConT tc1) (ConT tc2) | tc1 == tc2 = return ()
unify ty1 ty2 = error "Cannot unify types:"

unifyVar :: (MonadIO m, MonadThrow m) => MetaTv -> Tau -> Typ m ()
unifyVar tv1 ty2 = do
        mb_ty1 <- readTv tv1
        case mb_ty1 of
                Just ty1 -> unify ty1 ty2
                Nothing -> unifyUnboundVar tv1 ty2

unifyUnboundVar :: (MonadIO m, MonadThrow m) => MetaTv -> Tau -> Typ m ()
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

unifyFun :: (MonadIO m, MonadThrow m) => Rho -> Typ m (Sigma, Rho)
unifyFun (ArrT arg res) = return (unLoc arg, unLoc res)
unifyFun tau = do
        arg_ty <- newTyVar
        res_ty <- newTyVar
        unify tau (ArrT (noLoc arg_ty) (noLoc res_ty))
        return (arg_ty, res_ty)

occursCheckErr :: MonadThrow m => MetaTv -> Tau -> Typ m ()
occursCheckErr tv ty = lift $ throwPlainErr "Occurs check fail"

badType :: Tau -> Bool
badType (VarT (L _ (BoundTv _))) = True
badType _ = False