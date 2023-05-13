{-# LANGUAGE TupleSections #-}

module Plato.Typing.TcMonad where

import Plato.Common.Error
import Plato.Common.Location
import Plato.Common.Name
import Plato.Common.Name.Global
import Plato.Syntax.Typing
import Plato.Typing.TcTypes

import Control.Exception.Safe
import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Trans.Class
import Data.IORef
import Data.List ((\\))
import qualified Data.Map.Strict as M
import qualified Data.Set as S
import Prettyprinter

data TcEnv = TcEnv
        { tc_uniq :: IORef Uniq
        , tc_tyenv :: TyEnv
        }

newtype Tc m a = Tc (TcEnv -> m a)

unTc :: Tc m a -> TcEnv -> m a
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

instance MonadTrans Tc where
        lift m = Tc (const m)

runTc :: MonadIO m => TyEnv -> Tc m a -> m a
runTc binds (Tc tc) = do
        ref <- liftIO $ newIORef 0
        let env = TcEnv{tc_uniq = ref, tc_tyenv = binds}
        tc env

newTcRef :: MonadIO m => a -> Tc m (IORef a)
newTcRef v = lift (liftIO $ newIORef v)

readTcRef :: MonadIO m => IORef a -> Tc m a
readTcRef r = lift (liftIO $ readIORef r)

writeTcRef :: MonadIO m => IORef a -> a -> Tc m ()
writeTcRef r v = lift (liftIO $ writeIORef r v)

----------------------------------------------------------------
-- Type Environment
----------------------------------------------------------------
extendVarEnv :: Located Name -> Sigma -> Tc m a -> Tc m a
extendVarEnv var ty (Tc m) = Tc (m . extend)
    where
        extend env = env{tc_tyenv = M.insert (localName var) ty (tc_tyenv env)}

extendVarEnvList :: [(Located Name, Sigma)] -> Tc m a -> Tc m a
extendVarEnvList binds tc = foldr (uncurry extendVarEnv) tc binds

getEnv :: Monad m => Tc m (M.Map GlbName Sigma)
getEnv = Tc (return . tc_tyenv)

lookupVar :: MonadThrow m => GlbName -> Tc m Sigma
lookupVar x = do
        env <- getEnv
        case M.lookup x env of
                Just ty -> return ty
                Nothing -> lift $ throwLocErr (g_loc x) $ hsep ["Not in scope:", pretty x]

--------------------------------------------------
--      Creating, reading, writing MetaTvs      --
--------------------------------------------------
newTyVar :: MonadIO m => Tc m Tau
newTyVar = MetaT <$> newMetaTv

newSkolemTyVar :: MonadIO m => TyVar -> Tc m TyVar
newSkolemTyVar tv = SkolemTv (tyVarLName tv) <$> newUnique

newMetaTv :: MonadIO m => Tc m MetaTv
newMetaTv = Meta <$> newUnique <*> newTcRef Nothing

readMetaTv :: MonadIO m => MetaTv -> Tc m (Maybe Tau)
readMetaTv (Meta _ ref) = readTcRef ref

writeMetaTv :: MonadIO m => MetaTv -> Tau -> Tc m ()
writeMetaTv (Meta _ ref) ty = writeTcRef ref (Just ty)

newUnique :: MonadIO m => Tc m Uniq
newUnique =
        Tc
                ( \TcEnv{tc_uniq = ref} -> liftIO $ do
                        uniq <- readIORef ref
                        writeIORef ref (uniq + 1)
                        return uniq
                )

----------------------------------------------------------------
-- Instantiation
----------------------------------------------------------------
instantiate :: MonadIO m => Sigma -> Tc m (Expr -> Expr, Rho)
instantiate (AllT tvs tau) = do
        tvs' <- mapM (const newMetaTv) tvs
        let coercion = \e -> TAppE e (map MetaT tvs')
        return (coercion, subst (map fst tvs) (map MetaT tvs') (unLoc tau))
instantiate ty = return (id, ty)

skolemise :: MonadIO m => Sigma -> Tc m (Expr -> Expr, [Quant], Rho)
skolemise (AllT qnts rho) = do
        sks1 <- mapM (\(tv, mbkn) -> (,mbkn) <$> newSkolemTyVar tv) qnts
        (coercion, sks2, ty') <- skolemise (subst (map fst qnts) (map (VarT . fst) sks1) (unLoc rho))
        let coercion' = \e ->
                TAbsE
                        (map (unTyVar . fst) sks1)
                        (coercion $ TAppE e (map (VarT . fst) sks1))
        return (if null sks2 then id else coercion', sks1 ++ sks2, ty')
skolemise (ArrT arg_ty res_ty) = do
        (coercion, sks, res_ty') <- skolemise (unLoc res_ty)
        let coercion' = \e ->
                AbsE
                        (noLoc $ varName "?x")
                        (Just $ unLoc arg_ty)
                        ( coercion $
                                TAbsE
                                        (map (unTyVar . fst) sks)
                                        (AppE (TAppE e (map (VarT . fst) sks)) (VarE $ newGlbName Local (varName "?x")))
                        )
        return (if null sks then coercion else coercion', sks, ArrT arg_ty (L (getLoc res_ty) res_ty'))
skolemise ty = return (id, [], ty)

----------------------------------------------------------------
-- Quantification
----------------------------------------------------------------
quantify :: MonadIO m => [MetaTv] -> Rho -> Tc m Sigma
quantify tvs ty = do
        mapM_ bind (tvs `zip` new_bndrs)
        ty' <- zonkType ty
        return (AllT (map (,Nothing) new_bndrs) (noLoc ty'))
    where
        used_bndrs = tyVarBndrs ty
        new_bndrs = take (length tvs) (allBinders \\ used_bndrs)
        bind (tv, name) = writeMetaTv tv (VarT name)

allBinders :: [TyVar]
allBinders =
        [BoundTv $ noLoc $ str2tyvarName [x] | x <- ['a' .. 'z']]
                ++ [BoundTv $ noLoc $ str2tyvarName (x : show i) | i <- [1 :: Integer ..], x <- ['a' .. 'z']]

getEnvTypes :: Monad m => Tc m [Type]
getEnvTypes = M.elems <$> getEnv

getMetaTvs :: MonadIO m => Type -> Tc m (S.Set MetaTv)
getMetaTvs ty = do
        ty' <- zonkType ty
        return (metaTvs ty')

getFreeTvs :: MonadIO m => Type -> Tc m (S.Set TyVar)
getFreeTvs ty = do
        ty' <- zonkType ty
        return (freeTvs ty')

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
        mb_ty <- readMetaTv tv
        case mb_ty of
                Nothing -> return (MetaT tv)
                Just ty -> do
                        ty' <- zonkType ty
                        writeMetaTv tv ty'
                        return ty'
zonkType _ = unreachable "zonkType"

zonkExpr :: MonadIO m => Expr -> Tc m Expr
zonkExpr (AbsE var mty expr) = AbsE var <$> zonkType `traverse` mty <*> zonkExpr expr
zonkExpr (AppE fun arg) = AppE <$> zonkExpr fun <*> zonkExpr arg
zonkExpr (TAbsE ns expr) = TAbsE ns <$> zonkExpr expr
zonkExpr (TAppE e tys) = TAppE e <$> mapM zonkType tys
zonkExpr (LetE decs body) =
        LetE
                <$> forM decs (\(FuncD var e ty) -> FuncD var <$> zonkExpr e <*> zonkType `traverse` ty)
                <*> zonkExpr body
zonkExpr (CaseE e mty alts) =
        CaseE
                <$> zonkExpr e
                <*> zonkType
                `traverse` mty
                <*> forM alts (\(pat, body) -> (pat,) <$> zonkExpr body)
zonkExpr (AnnE e ty) = AnnE <$> zonkExpr e <*> zonkType ty
zonkExpr expr = return expr

----------------------------------------------------------------
-- Unification
----------------------------------------------------------------
unify :: (MonadIO m, MonadThrow m) => Tau -> Tau -> Tc m ()
unify ty1 ty2 | badType ty1 || badType ty2 = do
        lift $ throwError $ vsep ["Couldn't match type.", "Expected type:" <+> pretty ty2, indent 2 ("Actual type:" <+> pretty ty1)]
unify (VarT tv1) (VarT tv2) | tv1 == tv2 = return ()
unify (ConT tc1) (ConT tc2) | tc1 == tc2 = return ()
unify (ArrT arg1 res1) (ArrT arg2 res2) = do
        unify (unLoc arg1) (unLoc arg2)
        unify (unLoc res1) (unLoc res2)
unify (AppT fun1 arg1) (AppT fun2 arg2) = do
        unify (unLoc fun1) (unLoc fun2)
        unify (unLoc arg1) (unLoc arg2)
unify (MetaT tv1) (MetaT tv2) | tv1 == tv2 = return ()
unify (MetaT tv) ty = unifyVar tv ty
unify ty (MetaT tv) = unifyVar tv ty
unify ty1 ty2 = lift $ throwError $ vsep ["Couldn't match type.", "Expected type:" <+> pretty ty2, indent 2 ("Actual type:" <+> pretty ty1)]

{-unifyVar :: (MonadIO m, MonadThrow m) => MetaTv -> Tau -> Tc m ()
unifyVar tv1 ty2@(MetaT tv2) = do
        mb_ty1 <- readMetaTv tv1
        mb_ty2 <- readMetaTv tv2
        case (mb_ty1, mb_ty2) of
                (Just ty1, _) -> unify ty1 ty2
                (Nothing, Just ty2) -> unify (MetaT tv1) ty2
                (Nothing, Nothing) -> writeMetaTv tv1 ty2
unifyVar tv1 ty2 = do
        occursCheck tv1 ty2
        writeMetaTv tv1 ty2-}

unifyVar :: (MonadIO m, MonadThrow m) => MetaTv -> Tau -> Tc m ()
unifyVar tv1 ty2 = do
        mb_ty1 <- readMetaTv tv1
        case mb_ty1 of
                Just ty1 -> unify ty1 ty2
                Nothing -> unifyUnboundVar tv1 ty2

unifyUnboundVar :: (MonadIO m, MonadThrow m) => MetaTv -> Tau -> Tc m ()
unifyUnboundVar tv1 ty2@(MetaT tv2) = do
        mb_ty2 <- readMetaTv tv2
        case mb_ty2 of
                Just ty2' -> unify (MetaT tv1) ty2'
                Nothing -> writeMetaTv tv1 ty2
unifyUnboundVar tv1 ty2 = do
        tvs2 <- getMetaTvs ty2
        if tv1 `elem` tvs2
                then occursCheck tv1 ty2
                else writeMetaTv tv1 ty2

occursCheck :: (MonadThrow m, MonadIO m) => MetaTv -> Tau -> Tc m ()
occursCheck tv1 ty2 = do
        tvs2 <- getMetaTvs ty2
        when (tv1 `S.member` tvs2) $ lift $ throwError $ hsep ["Infinite type:", squotes $ pretty ty2]

badType :: Tau -> Bool
badType (VarT (BoundTv _)) = True
badType _ = False

unifyFun :: (MonadIO m, MonadThrow m) => Rho -> Tc m (Sigma, Rho)
unifyFun (ArrT arg res) = return (unLoc arg, unLoc res)
unifyFun tau = do
        arg_ty <- newTyVar
        res_ty <- newTyVar
        unify tau (ArrT (noLoc arg_ty) (noLoc res_ty))
        return (arg_ty, res_ty)