module Plato.Typing.Recon where

import Control.Monad.State
import Plato.Common.SrcLoc
import Plato.Common.Table
import Plato.Syntax.Typing
import Plato.Typing.Env
import Plato.Typing.Error

import qualified Data.Map.Strict as M
import qualified Data.Set as S
import Plato.Common.Error
import Plato.Common.Name

-- Context
type Env = Table Type

emptyEnv :: Env
emptyEnv = M.empty

-- Unique number
newtype Unique = Unique {count :: Int}

initUnique :: Unique
initUnique = Unique{count = 0}

-- Inference Monad
type Infer a = StateT Unique TypThrow a

{-
runInfer :: Infer (Subst, Type) -> Either String Scheme
runInfer m = case evalState (runExceptT m) initUnique of
        Left err -> Left err
        Right (_, t) -> Right $ closeOver t

closeOver :: Type -> Scheme
closeOver = generalize initialEnv-}

-- Substitution
type Subst = Table Type

nullSubst :: Subst
nullSubst = M.empty

compose :: Subst -> Subst -> Subst
s1 `compose` s2 = M.map (apply s1) s2 `M.union` s1

apply :: Subst -> Type -> Type
apply s t@(VarType x) = M.findWithDefault t (unLoc x) s
apply s (ArrType t1 t2) = ArrType (apply s <$> t1) (apply s <$> t2)
apply s (AllType x k t) = AllType x k (apply s <$> t)
apply s (AbsType x k t) = AbsType x k (apply s <$> t)
apply s (AppType t1 t2) = AppType (apply s <$> t1) (apply s <$> t2)
apply s (RecType x t) = undefined
apply s (RecordType fields) = undefined
apply s (SumType fields) = undefined

ftv :: Type -> S.Set Name
ftv (VarType x) = S.singleton (unLoc x)
ftv (ArrType t1 t2) = ftv (unLoc t1) `S.union` ftv (unLoc t2)
ftv (AllType x _ t) = S.insert (unLoc x) (ftv $ unLoc t)
ftv (AbsType x _ t) = S.insert (unLoc x) (ftv $ unLoc t)
ftv (AppType t1 t2) = ftv (unLoc t1) `S.union` ftv (unLoc t2)
ftv (RecType x t) = undefined
ftv (RecordType fields) = undefined
ftv (SumType fields) = undefined

applyEnv :: Subst -> Env -> Env
applyEnv s = M.map (apply s)

-- fresh name generation
letters :: [String]
letters = [1 ..] >>= flip replicateM ['a' .. 'z']

fresh :: Infer Type
fresh = do
        s <- get
        put s{count = count s + 1}
        return $ VarType $ noLoc $ str2varName (letters !! count s)

-- Unification
occursCheck :: Name -> Type -> Bool
occursCheck x t = x `S.member` ftv t

unify :: Type -> Type -> Infer Subst
unify (ArrType l r) (ArrType l' r') = do
        s1 <- unify (unLoc l) (unLoc l')
        s2 <- unify (apply s1 (unLoc r)) (apply s1 (unLoc r'))
        return (s2 `compose` s1)
unify (VarType x) t = bind x t
unify t (VarType x) = bind x t
unify t1 t2 = lift $ throwTypError NoSpan "unification fail." --tmp: NoSpan

bind :: Located Name -> Type -> Infer Subst
bind x t
        | t == VarType x = return nullSubst
        | occursCheck (unLoc x) t = lift $ throwTypError NoSpan "infinite type."
        | otherwise = return $ M.singleton (unLoc x) t

-- Inference
infer :: Env -> Expr -> Infer (Subst, Type)
infer env exp = case exp of
        VarExpr x -> lookupEnv env x
        AbsExpr x Nothing e -> do
                tv <- fresh
                let env' = M.insert (unLoc x) tv env
                (s1, t1) <- infer env' (unLoc e)
                return (s1, ArrType (noLoc $ apply s1 tv) (noLoc t1))
        AbsExpr _ (Just _) _ -> unreachable ""
        AppExpr e1 e2 -> do
                tv <- fresh
                (s1, t1) <- infer env (unLoc e1)
                (s2, t2) <- infer (applyEnv s1 env) (unLoc e2)
                s3 <- unify (apply s2 t1) (ArrType (noLoc t2) (noLoc tv))
                return (s3 `compose` s2 `compose` s1, apply s3 tv)
        TAbsExpr{} -> unreachable ""
        TAppExpr{} -> unreachable ""
        LetExpr x t1 e1 e2 -> do
                (s1, t1) <- infer env (unLoc e1)
                let env' = applyEnv s1 env
                (s2, t2) <- infer (M.insert (unLoc x) t1 env') (unLoc e2)
                return (s1 `compose` s2, t2)
        ProjExpr e x -> undefined
        RecordExpr fields -> undefined
        CaseExpr e t alts -> undefined
        TagExpr x es t -> undefined

lookupEnv :: Env -> Located Name -> Infer (Subst, Type)
lookupEnv env (L sp x) = case M.lookup x env of
        Nothing -> lift $ throwTypError sp $ "unbound variable: " ++ show x
        Just t -> return (nullSubst, t)

{-}
recon :: Located Expr -> Located Type -> StateT TypState TypThrow (Located Expr)
recon exp ty =
        cL exp <$> case (unLoc exp, unLoc ty) of
                (_, AllType tyX kn1 ty2) -> do
                        e2' <- recon exp ty2
                        return $ TAbsExpr tyX kn1 e2'
                (VarExpr x, _) -> do
                        ty' <- getTypeFromContext $ unLoc x
                        bind (unLoc ty) ty'
                        return $ VarExpr x
                (AbsExpr x Nothing e2, ArrType ty1 ty2) -> do
                        e2' <- recon e2 ty2
                        return $ AbsExpr x (Just $ unLoc ty1) e2'
                (AppExpr e1 e2, _) -> do
                        ty0 <- freshTyvar
                        e1' <- recon e1 (cL ty $ ArrType (noLoc ty0) ty)
                        e2' <- recon e2 ty
                        return $ AppExpr e1' e2'
                (LetExpr d1 e2, _) -> do
                        d1' <- reconDecl d1
                        e2' <- recon e2 ty
                        return $ LetExpr d1' e2'
                (ProjExpr e1 l, _) -> do
                        e1' <- recon e1
                        return $ ProjExpr e1' l
                (RecordExpr fields, RecordType fieldtys) -> do
                        undefined
                (CaseExpr e Nothing alts, _) -> undefined
                (TagExpr l xs Nothing, _) -> return $ TagExpr l xs (Just $ unLoc ty)
                _ -> undefined
-}