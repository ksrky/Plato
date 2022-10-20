module Plato.Typing.Monad where

import Plato.Common.Error
import Plato.Common.Name
import Plato.Common.SrcLoc
import Plato.Syntax.Typing
import Plato.Typing.Error
import Plato.Typing.Types

import Data.IORef
import Data.List
import qualified Data.Map.Strict as M

data TypEnv = TypEnv
        { uniqs :: IORef Uniq
        , var_env :: M.Map Name Sigma
        }

newtype Typ a = Typ (TypEnv -> IO (TypThrow a))

unTyp :: Typ a -> (TypEnv -> IO (TypThrow a))
unTyp (Typ a) = a

instance Functor Typ where
        fmap f (Typ x) = Typ $ \env -> do
                y <- x env
                return $ f <$> y

instance Applicative Typ where
        pure x = Typ (\_ -> return $ Right x)
        m <*> x = Typ $ \env -> do
                r1 <- unTyp m env
                case r1 of
                        Left err -> return $ Left err
                        Right f -> unTyp (fmap f x) env

instance Monad Typ where
        m >>= k = Typ $ \env -> do
                r1 <- unTyp m env
                case r1 of
                        Left err -> return (Left err)
                        Right v -> unTyp (k v) env

failTyp :: Span -> String -> Typ a
failTyp sp msg = Typ $ \_ -> return $ throwTypError sp msg

check :: Bool -> Span -> String -> Typ ()
check True _ _ = return ()
check False sp d = failTyp sp d

runTyp :: [(Name, Sigma)] -> Typ a -> IO (TypThrow a)
runTyp binds (Typ typ) = do
        ref <- newIORef 0
        let env = TypEnv{uniqs = ref, var_env = M.fromList binds}
        typ env

lift :: IO a -> Typ a
lift st = Typ (\_env -> do Right <$> st)

newTypRef :: a -> Typ (IORef a)
newTypRef v = lift (newIORef v)

readTypRef :: IORef a -> Typ a
readTypRef r = lift (readIORef r)

writeTypRef :: IORef a -> a -> Typ ()
writeTypRef r v = lift (writeIORef r v)

----------------------------------------------------------------
-- Type Environment
----------------------------------------------------------------
extendVarEnv :: Name -> Sigma -> Typ a -> Typ a
extendVarEnv var ty (Typ m) = Typ (m . extend)
    where
        extend env = env{var_env = M.insert var ty (var_env env)}

extendVarEnvList :: [(Name, Sigma)] -> Typ a -> Typ a
extendVarEnvList binds typ = foldr (uncurry extendVarEnv) typ binds

getEnv :: Typ (M.Map Name Sigma)
getEnv = Typ (return . Right . var_env)

lookupVar :: Located Name -> Typ Sigma
lookupVar (L sp x) = do
        env <- getEnv
        case M.lookup x env of
                Just ty -> return ty
                Nothing -> failTyp sp $ "Not in scope: " ++ show x

--------------------------------------------------
--      Creating, reading, writing MetaTvs      --
--------------------------------------------------
newTyVar :: Typ Tau
newTyVar = MetaT <$> newMetaTyVar

newMetaTyVar :: Typ MetaTv
newMetaTyVar = do
        uniq <- newUnique
        tref <- newTypRef Nothing
        return (Meta uniq tref)

newSkolemTyVar :: TyVar -> Typ TyVar
newSkolemTyVar tv = SkolemTv (tyVarName tv) <$> newUnique

readTv :: MetaTv -> Typ (Maybe Tau)
readTv (Meta _ ref) = readTypRef ref

writeTv :: MetaTv -> Tau -> Typ ()
writeTv (Meta _ ref) ty = writeTypRef ref (Just ty)

newUnique :: Typ Uniq
newUnique =
        Typ
                ( \TypEnv{uniqs = ref} -> do
                        uniq <- readIORef ref
                        writeIORef ref (uniq + 1)
                        return (Right uniq)
                )

----------------------------------------------------------------
-- Instantiation
----------------------------------------------------------------
instantiate :: Sigma -> Typ Rho
instantiate (AllT tvs ty) = do
        tvs' <- mapM (const newMetaTyVar) tvs
        return (substTy (map unLoc tvs) (map MetaT tvs') (unLoc ty))
instantiate ty = return ty

skolemise :: Sigma -> Typ ([TyVar], Rho, Expr -> Expr)
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
quantify :: [MetaTv] -> Rho -> Typ Sigma
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

getEnvTypes :: Typ [Type]
getEnvTypes = M.elems <$> getEnv

getMetaTyVars :: [Type] -> Typ [MetaTv]
getMetaTyVars tys = do
        tys' <- mapM zonkType tys
        return (metaTvs tys')

getFreeTyVars :: [Type] -> Typ [TyVar]
getFreeTyVars tys = do
        tys' <- mapM zonkType tys
        return (freeTyVars tys')

----------------------------------------------------------------
-- Zonking
----------------------------------------------------------------
zonkType :: Type -> Typ Type
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
unify :: Tau -> Tau -> Typ ()
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

unifyVar :: MetaTv -> Tau -> Typ ()
unifyVar tv1 ty2 = do
        mb_ty1 <- readTv tv1
        case mb_ty1 of
                Just ty1 -> unify ty1 ty2
                Nothing -> unifyUnboundVar tv1 ty2

unifyUnboundVar :: MetaTv -> Tau -> Typ ()
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

unifyFun :: Rho -> Typ (Sigma, Rho)
unifyFun (ArrT arg res) = return (unLoc arg, unLoc res)
unifyFun tau = do
        arg_ty <- newTyVar
        res_ty <- newTyVar
        unify tau (ArrT (noLoc arg_ty) (noLoc res_ty))
        return (arg_ty, res_ty)

occursCheckErr :: MetaTv -> Tau -> Typ ()
occursCheckErr tv ty = unreachable "Occurs check fail"

badType :: Tau -> Bool
badType (VarT (L _ (BoundTv _))) = True
badType _ = False