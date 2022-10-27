{-# LANGUAGE TupleSections #-}

module Plato.Typing.KindInfer where

import Plato.Common.Error
import Plato.Common.GenName
import Plato.Common.Name
import Plato.Common.Pretty
import Plato.Common.SrcLoc
import Plato.Syntax.Typing
import Plato.Typing.TcMonad (newTyVar)
import Plato.Typing.TcTypes (tyVarName)

import Control.Exception.Safe
import Control.Monad
import Control.Monad.IO.Class
import Data.Bifunctor (Bifunctor (first, second))
import Data.IORef
import qualified Data.Map.Strict as M
import qualified Data.Set as S

inferKind :: (MonadThrow m, MonadIO m) => KnTable -> Type -> m (Type, Kind)
inferKind knenv ty = runKi knenv $ do
        (ty', kn) <- infer ty
        ty'' <- zonkType ty'
        kn' <- zonkKind kn
        return (ty'', kn')

checkKindStar :: (MonadThrow m, MonadIO m) => KnTable -> Span -> Type -> m Type
checkKindStar knenv sp ty = runKi knenv $ do
        (ty', kn) <- infer ty
        ty'' <- zonkType ty'
        kn' <- zonkKind kn
        when (kn /= StarK) $ lift $ throwLocatedErr sp $ show ty ++ " doesn't have Kind *"
        return ty''

-- | kind Table
type KnTable = M.Map GenName Kind

emptyKnTable :: KnTable
emptyKnTable = M.empty

-- | Kinds
data MetaKv = MetaKv Uniq KnRef

type KnRef = IORef (Maybe Kind)

type Uniq = Int

instance Eq MetaKv where
        (MetaKv u1 _) == (MetaKv u2 _) = u1 == u2

instance Show MetaKv where
        show (MetaKv u _) = "$" ++ show u

metaKvs :: [Kind] -> [MetaKv]
metaKvs = foldr go []
    where
        go :: Kind -> [MetaKv] -> [MetaKv]
        go (MetaK kv) acc
                | kv `elem` acc = acc
                | otherwise = kv : acc
        go StarK acc = acc
        go (ArrK arg res) acc = go arg (go res acc)

-- | Kind environment
data KiEnv = KiEnv
        { uniqs :: IORef Uniq
        , var_env :: KnTable
        }

-- | Inference Monad
newtype Ki m a = Ki (KiEnv -> m a)

unKi :: Monad m => Ki m a -> (KiEnv -> m a)
unKi (Ki a) = a

instance Monad m => Functor (Ki m) where
        fmap f (Ki x) = Ki $ \env -> do
                y <- x env
                return $ f y

instance Monad m => Applicative (Ki m) where
        pure x = Ki (\_ -> return x)
        m <*> x = Ki $ \env -> do
                m' <- unKi m env
                unKi (fmap m' x) env

instance Monad m => Monad (Ki m) where
        m >>= k = Ki $ \env -> do
                v <- unKi m env
                unKi (k v) env

runKi :: MonadIO m => KnTable -> Ki m a -> m a
runKi knenv (Ki ki) = do
        ref <- liftIO $ newIORef 0
        let env = KiEnv{uniqs = ref, var_env = knenv}
        ki env

lift :: Monad m => m a -> Ki m a
lift st = Ki (const st)

newKiRef :: MonadIO m => a -> Ki m (IORef a)
newKiRef v = lift (liftIO $ newIORef v)

readKiRef :: MonadIO m => IORef a -> Ki m a
readKiRef r = lift (liftIO $ readIORef r)

writeKiRef :: MonadIO m => IORef a -> a -> Ki m ()
writeKiRef r v = lift (liftIO $ writeIORef r v)

-- | Kind environment
extendEnv :: GenName -> Kind -> Ki m a -> Ki m a
extendEnv x kn (Ki m) = Ki (m . extend)
    where
        extend env = env{var_env = M.insert x kn (var_env env)}

extendEnvList :: [(GenName, Kind)] -> Ki m a -> Ki m a
extendEnvList binds tr = foldr (uncurry extendEnv) tr binds

getEnv :: Monad m => Ki m KnTable
getEnv = Ki (return . var_env)

lookupVar :: MonadThrow m => GenName -> Ki m Kind
lookupVar x = do
        env <- getEnv
        case M.lookup x env of
                Just ty -> return ty
                Nothing -> lift $ throwLocatedErr (g_loc x) $ "Not in scope: " ++ show x

-- | KMeta
newKnVar :: MonadIO m => Ki m Kind
newKnVar = MetaK <$> newMetaKv

newMetaKv :: MonadIO m => Ki m MetaKv
newMetaKv = do
        uniq <- newUnique
        kref <- newKiRef Nothing
        return (MetaKv uniq kref)

readKv :: MonadIO m => MetaKv -> Ki m (Maybe Kind)
readKv (MetaKv _ ref) = readKiRef ref

writeKv :: MonadIO m => MetaKv -> Kind -> Ki m ()
writeKv (MetaKv _ ref) ty = writeKiRef ref (Just ty)

newUnique :: MonadIO m => Ki m Uniq
newUnique =
        Ki
                ( \KiEnv{uniqs = ref} -> liftIO $ do
                        uniq <- readIORef ref
                        writeIORef ref (uniq + 1)
                        return uniq
                )

getMetaKvs :: MonadIO m => [Kind] -> Ki m [MetaKv]
getMetaKvs kns = do
        tys' <- mapM zonkKind kns
        return (metaKvs tys')

-- | Zonking
zonkKind :: MonadIO m => Kind -> Ki m Kind
zonkKind StarK = return StarK
zonkKind (ArrK arg res) = do
        arg' <- zonkKind arg
        res' <- zonkKind res
        return (ArrK arg' res')
zonkKind (MetaK kv) = do
        mb_kn <- readKv kv
        case mb_kn of
                Nothing -> return (MetaK kv)
                Just kn -> do
                        kn' <- zonkKind kn
                        writeKv kv kn'
                        return kn'

zonkType :: MonadIO m => Type -> Ki m Type
zonkType (VarT tv) = return $ VarT tv
zonkType (ConT x) = return $ ConT x
zonkType (ArrT arg res) = ArrT <$> zonkType arg <*> zonkType res
zonkType (AllT tvs ty) = do
        tvs' <- forM tvs $ \(tv, mkn) -> (tv,) <$> zonkKind `traverse` mkn
        AllT tvs' <$> zonkType ty
zonkType (AbsT x mkn ty) = AbsT x <$> zonkKind `traverse` mkn <*> zonkType ty
zonkType (AppT fun arg) = AppT <$> zonkType fun <*> zonkType arg
zonkType (RecT x ty) = RecT x <$> zonkType ty
zonkType (RecordT fields) = do
        fields' <- forM fields $ \(x, ty) -> (x,) <$> zonkType ty
        return $ RecordT fields'
zonkType (SumT fields) = do
        fields' <- forM fields $ \(x, tys) -> (x,) <$> mapM zonkType tys
        return $ SumT fields'
zonkType MetaT{} = unreachable ""

-- | Unification
unify :: (MonadThrow m, MonadIO m) => Kind -> Kind -> Ki m ()
unify (MetaK kv) k = unifyVar kv k
unify k (MetaK kv) = unifyVar kv k
unify StarK StarK = return ()
unify (ArrK l r) (ArrK l' r') = do
        unify l l'
        unify r r'
unify k1 k2 = lift $ throwPlainErr $ "UnificationFail " ++ show k1 ++ ", " ++ show k2

unifyVar :: (MonadIO m, MonadThrow m) => MetaKv -> Kind -> Ki m ()
unifyVar kv1 kn2 = do
        mb_kn1 <- readKv kv1
        case mb_kn1 of
                Just kn1 -> unify kn1 kn2
                Nothing -> unifyUnboundVar kv1 kn2

unifyUnboundVar :: (MonadIO m, MonadThrow m) => MetaKv -> Kind -> Ki m ()
unifyUnboundVar kv1 kn2@(MetaK kv2) = do
        mb_ty2 <- readKv kv2
        case mb_ty2 of
                Just kn2' -> unify (MetaK kv1) kn2'
                Nothing -> writeKv kv1 kn2
unifyUnboundVar kv1 kn2 = do
        kvs2 <- getMetaKvs [kn2]
        if kv1 `elem` kvs2
                then occursCheckErr kv1 kn2
                else writeKv kv1 kn2

occursCheckErr :: MonadThrow m => MetaKv -> Kind -> Ki m ()
occursCheckErr tv ty = lift $ throwPlainErr "Occurs check fail" --tmp

-- | Inference
infer :: (MonadThrow m, MonadIO m) => Type -> Ki m (Type, Kind)
infer t = case t of
        VarT tv -> do
                kn <- lookupVar (tyVarName tv)
                return (VarT tv, kn)
        ConT x -> do
                kn <- lookupVar x
                return (ConT x, kn)
        ArrT ty1 ty2 -> do
                (ty1', kn1) <- infer ty1
                (ty2', kn2) <- infer ty2
                s3 <- unify kn1 StarK
                s4 <- unify kn2 StarK
                return (ArrT ty1' ty2', StarK)
        AllT tvs ty1 -> do
                binds <- forM tvs $ \tv -> do
                        kv <- newKnVar
                        return (fst tv, kv)
                (ty1', kn1) <- extendEnvList (map (first tyVarName) binds) (infer ty1)
                unify kn1 StarK
                return (AllT (map (second Just) binds) ty1', StarK)
        AbsT x _ ty1 -> do
                kv <- newKnVar
                (ty1', kn1) <- extendEnv x kv (infer ty1)
                return (AbsT x (Just kv) ty1', ArrK kv kn1)
        AppT ty1 ty2 -> do
                kv <- newKnVar
                (ty1', kn1) <- infer ty1
                (ty2', kn2) <- infer ty2
                unify kn1 (ArrK kn2 kv)
                return (AppT ty1' ty2', kv)
        RecT x ty1 -> do
                kv <- newKnVar
                (ty1', kn1) <- extendEnv x kv (infer ty1)
                s2 <- unify kn1 StarK
                return (RecT x ty1', StarK)
        RecordT fields -> do
                fields' <- forM fields $ \(x, ty1) -> do
                        (ty1', kn1) <- infer ty1
                        unify kn1 StarK
                        return (x, ty1')
                return (RecordT fields', StarK)
        SumT fields -> do
                fields' <- forM fields $ \(x, tys) -> do
                        tys' <- forM tys $ \ty1 -> do
                                (ty1', kn1) <- infer ty1
                                unify kn1 StarK
                                return ty1'
                        return (x, tys')
                return (SumT fields', StarK)
        MetaT{} -> unreachable ""
