{-# LANGUAGE TupleSections #-}

module Plato.TypeCheck.Utils where

import Control.Monad.IO.Class
import Control.Monad.Reader
import qualified Data.Map.Strict as M
import qualified Data.Set as S

import Plato.Common.Error
import Plato.Common.Location
import Plato.Syntax.Typing
import Plato.Typing.Monad

zonkType :: MonadIO m => Type -> Typ m Type
zonkType (VarT tv) = return (VarT tv)
zonkType (ConT tc) = return (ConT tc)
zonkType (ArrT arg res) = ArrT <$> zonkType `traverse` arg <*> zonkType `traverse` res
zonkType (AllT tvs ty) = AllT tvs <$> zonkType `traverse` ty
zonkType (AppT fun arg) = AppT <$> zonkType `traverse` fun <*> zonkType `traverse` arg
zonkType (MetaT tv) = do
        mb_ty <- readMetaTv tv
        case mb_ty of
                Nothing -> return (MetaT tv)
                Just ty -> do
                        ty' <- zonkType ty
                        writeMetaTv tv ty'
                        return ty'
zonkType _ = unreachable "TypeCheck.Utils.zonkType"

zonkExpr :: MonadIO m => Expr -> Typ m Expr
zonkExpr (VarE n) = return (VarE n)
zonkExpr (AppE fun arg) = AppE <$> zonkExpr `traverse` fun <*> zonkExpr `traverse` arg
zonkExpr (AbsE var mty body) = AbsE var <$> zonkType `traverse` mty <*> zonkExpr `traverse` body
zonkExpr (TAppE body ty_args) = TAppE body <$> mapM zonkType ty_args
zonkExpr (TAbsE ty_vars body) = TAbsE ty_vars <$> zonkExpr `traverse` body
zonkExpr (LetE bnds decs body) = do
        bnds' <- mapM (\(x, FunBind e) -> (x,) <$> FunBind <$> zonkExpr e) bnds
        decs' <- mapM (\(x, ValDecl ty) -> (x,) <$> ValDecl <$> zonkType ty) decs
        LetE bnds' decs' <$> zonkExpr `traverse` body
zonkExpr (CaseE e mbty alts) =
        CaseE
                <$> zonkExpr `traverse` e
                <*> zonkType `traverse` mbty
                <*> forM alts (\(pat, body) -> (pat,) <$> zonkExpr `traverse` body)
zonkExpr _ = unreachable "TypeCheck.Utils.zonkExpr"

getEnvTypes :: Monad m => Typ m [Type]
getEnvTypes = asks (\(TypEnv tyenv _ _) -> M.elems tyenv)

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