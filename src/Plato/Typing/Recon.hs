module Plato.Typing.Recon where

import Control.Monad.State
import Plato.Common.SrcLoc
import Plato.Syntax.Typing
import Plato.Typing.Env
import Plato.Typing.Error

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
