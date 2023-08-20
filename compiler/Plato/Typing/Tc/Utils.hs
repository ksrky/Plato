{-# LANGUAGE LambdaCase #-}

module Plato.Typing.Tc.Utils (
        getEnvTypes,
        getMetaTvs,
        getFreeTvs,
) where

import Control.Monad.IO.Class
import Control.Monad.Reader
import Data.Map.Strict qualified as M
import Data.Set qualified as S
import GHC.Stack

import Plato.Common.Location
import Plato.Syntax.Typing
import Plato.Typing.Env
import Plato.Typing.Zonking

getEnvTypes :: (MonadReader e m, HasTypEnv e) => m [Type]
getEnvTypes = do
        env <- asks getTypEnv
        return $ concat $ M.elems $ M.map (\case ValBind ty -> [ty]; _ -> []) env

getMetaTvs :: MonadIO m => Type -> m (S.Set MetaTv)
getMetaTvs ty = do
        ty' <- zonk ty
        return (metaTvs ty')

metaTvs :: HasCallStack => Type -> S.Set MetaTv
metaTvs VarT{} = S.empty
metaTvs ConT{} = S.empty
metaTvs (ArrT arg res) = metaTvs (unLoc arg) `S.union` metaTvs (unLoc res)
metaTvs (AllT _ ty) = metaTvs (unLoc ty)
metaTvs (AppT fun arg) = metaTvs (unLoc fun) `S.union` metaTvs (unLoc arg)
metaTvs (MetaT tv) = S.singleton tv

getFreeTvs :: MonadIO m => Type -> m (S.Set TyVar)
getFreeTvs ty = do
        ty' <- zonk ty
        return $ runReader (freeTvs ty') S.empty

freeTvs :: HasCallStack => Type -> Reader (S.Set TyVar) (S.Set TyVar)
freeTvs (VarT tv) = do
        bounded <- asks (\bound -> tv `elem` bound) -- bounded TyVar must be BoundTv
        if bounded then return S.empty else return $ S.singleton tv
freeTvs ConT{} = return S.empty
freeTvs (ArrT arg res) = S.union <$> freeTvs (unLoc arg) <*> freeTvs (unLoc res)
freeTvs (AllT qnts body) = local ((flip . foldr) (S.insert . fst) qnts) $ freeTvs (unLoc body)
freeTvs (AppT fun arg) = S.union <$> freeTvs (unLoc fun) <*> freeTvs (unLoc arg)
freeTvs MetaT{} = return S.empty