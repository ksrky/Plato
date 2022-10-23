{-# LANGUAGE TupleSections #-}

module Plato.Transl.TypToCore where

import Plato.Common.Error
import Plato.Common.SrcLoc
import Plato.Core.Commands as C
import Plato.Core.Context
import qualified Plato.Syntax.Core as C
import qualified Plato.Syntax.Typing as T
import Plato.Typing.TrTypes

import Control.Exception.Safe
import Control.Monad
import Control.Monad.State
import Plato.Common.Name

transExpr = undefined

transType :: MonadThrow m => Context -> T.Type -> m C.Ty
transType ctx = tratype
    where
        tratype :: MonadThrow m => T.Type -> m C.Ty
        tratype (T.VarT tv) = do
                i <- getVarIndex ctx (tyVarName <$> tv)
                return $ C.TyVar i (length ctx)
        tratype (T.ConT x) = do
                i <- getVarIndex ctx x
                return $ C.TyVar i (length ctx)
        tratype (T.ArrT ty1 ty2) = do
                ty1' <- tratype (unLoc ty1)
                ty2' <- tratype (unLoc ty2)
                return $ C.TyArr ty1' ty2'
        tratype (T.AllT xs ty) = do
                let knK1 = C.KnStar -- tmp: forall x.t = forall x::*.t
                    xs' = map (tyVarName <$>) xs
                ctx' <- addNames (map (tyVarName <$>) xs) ctx
                ty' <- transType ctx' (unLoc ty)
                return $ foldr (`C.TyAll` knK1) ty' (map (tyVarName . unLoc) xs)
        tratype (T.AbsT x ty) = do
                let knK1 = C.KnStar -- tmp: \x.t = \x:*.t
                ctx' <- addName x ctx
                ty' <- transType ctx' (unLoc ty)
                return $ C.TyAbs (unLoc x) knK1 ty'
        tratype (T.AppT ty1 ty2) = do
                ty1' <- tratype $ unLoc ty1
                ty2' <- tratype $ unLoc ty2
                return $ C.TyApp ty1' ty2'
        tratype (T.RecT x ty) = do
                ctx' <- addName x ctx
                ty' <- transType ctx' (unLoc ty)
                return $ C.TyRec (unLoc x) C.KnStar ty' -- tmp: unused
        tratype (T.RecordT fieldtys) = do
                fields' <- forM fieldtys $ \(l, field) -> (unLoc l,) <$> tratype (unLoc field)
                return $ C.TyRecord fields'
        tratype (T.SumT fieldtys) = do
                fields' <- forM fieldtys $ \(l, field) -> (unLoc l,) <$> mapM (tratype . unLoc) field
                return $ C.TyVariant fields'
        tratype T.MetaT{} = throwUnexpectedErr "Zonking failed"

transDecl :: MonadThrow m => Located T.Decl -> StateT Context m (Located Name, C.Binding)
transDecl dec = StateT $ \ctx -> case unLoc dec of
        T.TypeD name ty -> do
                tyT <- transType ctx `traverse` ty
                ctx' <- addName name ctx
                return ((name, C.TyAbbBind tyT Nothing), ctx')
        T.VarD f ty -> do
                tyT <- transType ctx `traverse` ty
                ctx' <- addName f ctx
                return ((f, C.VarBind tyT), ctx')
        T.FuncD (T.FD f e ty) -> do
                t <- transExpr ctx ty e
                tyT <- transType ctx `traverse` ty
                ctx' <- addName f ctx
                return ((f, C.TmAbbBind t (Just tyT)), ctx')

typ2core :: MonadThrow m => Context -> T.Decls -> m Commands
typ2core ctx (T.Decls modns binds fundecs) = (`evalStateT` ctx) $ do
        binds' <- mapM transDecl binds
        ctx' <- get
        return $ Commands{C.imports = modns, C.binds = binds', C.body = undefined}
