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

getEnvTypes :: (MonadReader ctx m, HasTypEnv ctx) => m [Type]
getEnvTypes = do
        env <- getEnv =<< ask
        return $ concat $ M.elems $ M.map (\case ValBinding ty -> [ty]; _ -> []) env

getMetaTvs :: MonadIO m => Type -> m (S.Set MetaTv)
getMetaTvs ty = do
        ty' <- zonkType ty
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
        ty' <- zonkType ty
        return (freeTvs ty')

freeTvs :: HasCallStack => Type -> S.Set TyVar
freeTvs (VarT tv) = S.singleton tv
freeTvs ConT{} = S.empty
freeTvs (ArrT arg res) = freeTvs (unLoc arg) `S.union` freeTvs (unLoc res)
freeTvs (AllT tvs ty) = S.fromList (map fst tvs) `S.union` freeTvs (unLoc ty)
freeTvs (AppT fun arg) = freeTvs (unLoc fun) `S.union` freeTvs (unLoc arg)
freeTvs MetaT{} = S.empty