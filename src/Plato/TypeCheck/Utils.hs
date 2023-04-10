module Plato.TypeCheck.Utils where

import Control.Monad.IO.Class
import Control.Monad.Reader
import qualified Data.Map.Strict as M
import qualified Data.Set as S

import Plato.Common.Error
import Plato.Common.Location
import Plato.Syntax.Typing.Type
import Plato.Typing.Env as Env
import Plato.Typing.Monad
import Plato.Typing.Zonking

getEnvTypes :: Monad m => Typ m [Type]
getEnvTypes = asks (concat . M.elems . M.map (\bndng -> case bndng of Env.ValueBinding ty -> [ty]; _ -> []))

getMetaTvs :: MonadIO m => Type -> Typ m (S.Set MetaTv)
getMetaTvs ty = do
        ty' <- zonkType ty
        return (metaTvs ty')

metaTvs :: Type -> S.Set MetaTv
metaTvs VarT{} = S.empty
metaTvs ConT{} = S.empty
metaTvs (ArrT arg res) = metaTvs (unLoc arg) `S.union` metaTvs (unLoc res)
metaTvs (AllT _ ty) = metaTvs (unLoc ty)
metaTvs (AppT fun arg) = metaTvs (unLoc fun) `S.union` metaTvs (unLoc arg)
metaTvs (MetaT tv) = S.singleton tv
metaTvs _ = unreachable "TypeCheck.Utils.metaTvs"

getFreeTvs :: MonadIO m => Type -> Typ m (S.Set TyVar)
getFreeTvs ty = do
        ty' <- zonkType ty
        return (freeTvs ty')

freeTvs :: Type -> S.Set TyVar
freeTvs (VarT tv) = S.singleton tv
freeTvs ConT{} = S.empty
freeTvs (ArrT arg res) = freeTvs (unLoc arg) `S.union` freeTvs (unLoc res)
freeTvs (AllT tvs ty) = S.fromList (map fst tvs) `S.union` freeTvs (unLoc ty)
freeTvs (AppT fun arg) = freeTvs (unLoc fun) `S.union` freeTvs (unLoc arg)
freeTvs MetaT{} = S.empty
freeTvs _ = unreachable "TypeCheck.Utils.freeTvs"