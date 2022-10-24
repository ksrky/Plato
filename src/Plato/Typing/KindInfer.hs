module Plato.Typing.KindInfer where

import Control.Exception.Safe
import Control.Monad.State
import qualified Data.Map.Strict as M
import qualified Data.Set as S
import Plato.Common.Error
import Plato.Common.Name
import Plato.Common.Pretty
import Plato.Common.SrcLoc
import Plato.Syntax.Typing
import Plato.Typing.TrTypes

inferKind :: MonadThrow m => Type -> m (Subst, Kind)
inferKind ty = evalStateT (infer initialEnv ty) initUnique

checkKindStar :: MonadThrow m => Located Type -> m Subst
checkKindStar (L sp ty) = do
        (subst, kn) <- evalStateT (infer initialEnv ty) initUnique
        when (kn /= StarK) $ throwLocatedErr sp $ show ty ++ " doesn't have Kind *"
        return subst

-- | Context
newtype KindEnv = KindEnv (M.Map Name Kind)

extend :: KindEnv -> (Name, Kind) -> KindEnv
extend (KindEnv env) (x, s) = KindEnv $ M.insert x s env

initialEnv :: KindEnv
initialEnv = KindEnv M.empty

-- | Unique number
newtype Unique = Unique {count :: Int}

initUnique :: Unique
initUnique = Unique{count = 0}

-- | Inference Monad
type Infer m a = StateT Unique m a

-- | Substitution
type Subst = M.Map Name Kind

nullSubst :: Subst
nullSubst = M.empty

compose :: Subst -> Subst -> Subst
s1 `compose` s2 = M.map (apply s1) s2 `M.union` s1

class Substitutable a where
        apply :: Subst -> a -> a
        ftv :: a -> S.Set Name

instance Substitutable Kind where
        apply _ StarK = StarK
        apply s k@(VarK a) = M.findWithDefault k a s
        apply s (ArrK t1 t2) = ArrK (apply s t1) (apply s t2)
        ftv StarK{} = S.empty
        ftv (VarK a) = S.singleton a
        ftv (ArrK k1 k2) = ftv k1 `S.union` ftv k2

instance Substitutable a => Substitutable [a] where
        apply = fmap . apply
        ftv = foldr (S.union . ftv) S.empty

instance Substitutable KindEnv where
        apply s (KindEnv env) = KindEnv $ M.map (apply s) env
        ftv (KindEnv env) = ftv $ M.elems env

-- | fresh name generation
letters :: [String]
letters = ['k' : show i | i <- [0 :: Integer ..]]

fresh :: MonadThrow m => Infer m Kind
fresh = do
        s <- get
        put s{count = count s + 1}
        return $ VarK $ str2tyvarName (letters !! count s)

-- | Unification
occursCheck :: Substitutable a => Name -> a -> Bool
occursCheck a t = a `S.member` ftv t

unify :: MonadThrow m => Kind -> Kind -> Infer m Subst
unify (ArrK l r) (ArrK l' r') = do
        s1 <- unify l l'
        s2 <- unify (apply s1 r) (apply s1 r')
        return (s2 `compose` s1)
unify (VarK a) k = bind a k
unify k (VarK a) = bind a k
unify StarK StarK = return nullSubst
unify k1 k2 = throwPlainErr $ "UnificationFail " ++ show k1 ++ ", " ++ show k2

bind :: MonadThrow m => Name -> Kind -> Infer m Subst
bind a k
        | k == VarK a = return nullSubst
        | occursCheck a k = throwPlainErr $ "InfiniteKind " ++ show a ++ ", " ++ show k
        | otherwise = return $ M.singleton a k

-- | Inference
infer :: MonadThrow m => KindEnv -> Type -> Infer m (Subst, Kind)
infer env t = case t of
        VarT tv -> lookupEnv env (tyVarName $ unLoc tv)
        ConT x -> lookupEnv env (unLoc x)
        ArrT t1 t2 -> do
                (s1, k1) <- infer env (unLoc t1)
                (s2, k2) <- infer (apply s1 env) (unLoc t2)
                s3 <- unify (apply s2 k1) StarK
                s4 <- unify (apply s3 k2) StarK
                return (s4 `compose` s3 `compose` s2 `compose` s1, StarK)
        AllT tvs ty -> do
                binds <- forM tvs $ \tv -> do
                        kv <- fresh
                        return (tyVarName $ unLoc tv, kv)
                let env' = foldl extend env binds
                (s1, k1) <- infer env' (unLoc ty)
                s2 <- unify (apply s1 k1) StarK
                return (s2 `compose` s1, StarK)
        AbsT x ty -> do
                kv <- fresh
                let env' = env `extend` (unLoc x, kv)
                (s1, k1) <- infer env' (unLoc ty)
                return (s1, ArrK (apply s1 kv) k1)
        AppT t1 t2 -> do
                kv <- fresh
                (s1, k1) <- infer env (unLoc t1)
                (s2, k2) <- infer (apply s1 env) (unLoc t2)
                s3 <- unify (apply s2 k1) (ArrK k2 kv)
                return (s3 `compose` s2 `compose` s1, apply s3 kv)
        RecT x ty -> do
                kv <- fresh
                let env' = env `extend` (unLoc x, kv)
                (s1, k1) <- infer env' (unLoc ty)
                s2 <- unify (apply s1 k1) StarK
                return (s2 `compose` s1, StarK)
        RecordT fields -> do
                ss <- forM fields $ \(x, ty) -> do
                        (s1, k1) <- infer env (unLoc ty)
                        s2 <- unify (apply s1 k1) StarK
                        return $ s2 `compose` s1
                return (foldr compose M.empty ss, StarK)
        SumT fields -> do
                ss' <- forM fields $ \(x, tys) -> do
                        ss <- forM tys $ \ty -> do
                                (s1, k1) <- (infer env . unLoc) ty
                                s2 <- unify (apply s1 k1) StarK
                                return $ s2 `compose` s1
                        return $ foldr compose M.empty ss
                return (foldr compose M.empty ss', StarK)
        MetaT{} -> unreachable ""

lookupEnv :: MonadThrow m => KindEnv -> Name -> Infer m (Subst, Kind)
lookupEnv (KindEnv env) x = case M.lookup x env of
        Nothing -> throwPlainErr $ "UnboundVariable " ++ show x
        Just s -> return (nullSubst, s)
