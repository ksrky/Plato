{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs     #-}

module Plato.Typing.Misc
    ( getEnvTypes
    , getFreeTvs
    , getMetaKvs
    , getMetaTvs
    , substExpr
    , substTvs
    ) where

import Control.Monad.IO.Class
import Control.Monad.Reader
import Data.Map.Strict        qualified as M
import Data.Set               qualified as S
import GHC.Stack

import Plato.Common.Ident
import Plato.Common.Location
import Plato.Syntax.Typing
import Plato.Typing.Env
import Plato.Typing.Zonking

getEnvTypes :: (MonadReader e m, HasTypEnv e) => m [Type]
getEnvTypes = asks (envTypes . getTypEnv)

getMetaTvs :: MonadIO m => Type -> m (S.Set MetaTv)
getMetaTvs ty = do
    ty' <- zonk ty
    return (metaTvs ty')

metaTvs :: HasCallStack => Type -> S.Set MetaTv
metaTvs VarT{}         = S.empty
metaTvs ConT{}         = S.empty
metaTvs (ArrT arg res) = metaTvs (unLoc arg) `S.union` metaTvs (unLoc res)
metaTvs (AllT _ ty)    = metaTvs (unLoc ty)
metaTvs (AppT fun arg) = metaTvs (unLoc fun) `S.union` metaTvs (unLoc arg)
metaTvs (MetaT tv)     = S.singleton tv

getFreeTvs :: MonadIO m => Type -> m (S.Set TyVar)
getFreeTvs ty = do
    ty' <- zonk ty
    return $ runReader (freeTvs ty') S.empty

freeTvs :: HasCallStack => Type -> Reader (S.Set TyVar) (S.Set TyVar)
freeTvs (VarT tv) = do
    bounded <- asks (tv `elem`) -- Note: if bounded, tv must be BoundTv
    if bounded then return S.empty else return $ S.singleton tv
freeTvs ConT{} = return S.empty
freeTvs (ArrT arg res) = S.union <$> freeTvs (unLoc arg) <*> freeTvs (unLoc res)
freeTvs (AllT qnts body) = local ((flip . foldr) (S.insert . fst) qnts) $ freeTvs (unLoc body)
freeTvs (AppT fun arg) = S.union <$> freeTvs (unLoc fun) <*> freeTvs (unLoc arg)
freeTvs MetaT{} = return S.empty

substTvs :: MonadIO m => [TyVar] -> [Type] -> Type -> m Type
substTvs tvs tys ty = let s = M.fromList (zip tvs tys) in apply s <$> zonk ty

apply :: M.Map TyVar Tau -> Type -> Type
apply s ty@(VarT tv)    = M.findWithDefault ty tv s
apply _ ty@ConT{}       = ty
apply s (ArrT arg res)  = ArrT (apply s <$> arg) (apply s <$> res)
apply s (AllT tvs body) = AllT tvs $ apply (foldr (\(tv, _) -> M.delete tv) s tvs) <$> body
apply s (AppT fun arg)  = AppT (apply s <$> fun) (apply s <$> arg)
apply _ ty@MetaT{}      = ty

getMetaKvs :: MonadIO m => Kind -> m (S.Set MetaKv)
getMetaKvs kn = metaKvs <$> zonk kn

metaKvs :: Kind -> S.Set MetaKv
metaKvs StarK          = S.empty
metaKvs (ArrK kn1 kn2) = metaKvs kn1 `S.union` metaKvs kn2
metaKvs (MetaK kv)     = S.singleton kv

substExpr :: Ident -> Expr 'Typed -> Expr 'Typed -> Expr 'Typed
substExpr id exp body = subst' body
  where
    subst' :: Expr 'Typed -> Expr 'Typed
    subst' (VarE var)
        | var == id = exp
        | otherwise = VarE var
    subst' (AppE fun arg) = AppE (subst' fun) (subst' arg)
    subst' (AbsE var ty body) = AbsE var ty (subst' body)
    subst' (TAppE exp tyargs) = TAppE (subst' exp) tyargs
    subst' (TAbsE qnts body) = TAbsE qnts (subst' body)
    subst' (LetE bnds body) =
        LetE (fmap (\(Bind idty exp) -> Bind idty (subst' exp)) bnds) (subst' body)
    subst' (CaseE test ann_ty alts) =
        CaseE (subst' test) ann_ty (map (\(pat, exp) -> (pat, subst' exp)) alts)
