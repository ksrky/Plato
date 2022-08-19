{-# LANGUAGE MultiParamTypeClasses #-}

module Plato.Translation.IRToCore where

import Plato.Common.Error
import Plato.Common.Info
import Plato.Common.Name
import Plato.Common.Pretty
import Plato.Core.Command as C
import Plato.Core.Context
import Plato.Core.Syntax
import Plato.IR.Syntax
import Plato.IR.Utils

import Control.Exception.Safe
import Control.Monad.State
import Data.Maybe (fromMaybe)

transExpr :: MonadThrow m => Context -> Type -> Expr -> m Term
transExpr ctx restty = traexpr
    where
        traexpr :: MonadThrow m => Expr -> m Term
        traexpr (VarExpr fi x) = do
                i <- getVarIndex fi ctx x
                return $ TmVar fi i (length ctx)
        traexpr (AppExpr fi e1 e2) = do
                t1 <- traexpr e1
                t2 <- traexpr e2
                return $ TmApp fi t1 t2
        traexpr (TAppExpr fi e1 t2) = do
                t1 <- traexpr e1
                tyT2 <- transType ctx t2
                return $ TmTApp fi t1 tyT2
        traexpr exp@(LamExpr fi x e) | nameSpace x == TyVarName = case restty of
                AllType _ tyX ty -> do
                        let knK1 = KnStar
                        ctx' <- addName fi tyX ctx
                        t2 <- transExpr ctx' ty e
                        return $ TmTAbs fi x knK1 t2
                _ -> throwError fi $ "Expected all type, but got " ++ pretty restty
        traexpr exp@(LamExpr fi x e) = case restty of
                ArrType _ ty1 ty2 -> do
                        tyT1 <- transType ctx ty1
                        ctx' <- addName fi x ctx
                        t2 <- transExpr ctx' ty2 e
                        return $ TmAbs fi x tyT1 t2
                AllType _ tyX ty -> do
                        let knK1 = KnStar -- tmp
                        ctx' <- addName fi tyX ctx
                        t2 <- transExpr ctx' ty exp
                        return $ TmTAbs fi tyX knK1 t2
                _ -> throwError fi $ "Expected arrow type, but got " ++ pretty restty ++ "\n" ++ show exp
        traexpr (LetExpr fi d e1) = case d of
                FuncDecl fi2 f e2 ty -> do
                        tyT <- transType ctx ty
                        ctx' <- addName fi2 f ctx
                        t1 <- transExpr ctx' ty e2
                        let r = TmFix dummyInfo (TmAbs dummyInfo f tyT t1)
                        t2 <- transExpr ctx' restty e1
                        return $ TmLet fi f r t2
                _ -> unreachable "Type declaration in let binding"
        traexpr (TagExpr fi l as) = do
                tyT <- transType ctx restty
                as' <- mapM traexpr as
                return $ TmTag fi l as' tyT
        traexpr (ProjExpr fi e l) = do
                t1 <- traexpr e
                return $ TmProj fi t1 l
        traexpr (RecordExpr fi fields) = do
                fields' <- forM fields $ \(li, ei) -> case restty of
                        RecordType _ fieldtys -> case lookup li fieldtys of
                                Just tyi -> do
                                        ti <- transExpr ctx tyi ei
                                        return (li, ti)
                                _ -> throwError fi $ "label " ++ show li ++ " not found in the type signature"
                        _ -> throwError fi $ "Expected record type, but got " ++ show restty
                return $ TmRecord fi fields'
        traexpr (CaseExpr fi e alts) = do
                t <- traexpr e
                alts' <- forM alts $ \(pat, body) -> case pat of
                        ConPat fi1 li ps -> do
                                ctx' <- (`execStateT` ctx) $
                                        forM_ ps $ \(AnyPat fi2 mx) -> StateT $ \ctx -> do
                                                let x = fromMaybe dummyVarName mx
                                                ctx' <- addName fi1 x ctx
                                                return ((), ctx')
                                ti <- transExpr ctx' restty body
                                return (li, (length ps, ti))
                        AnyPat fi1 (Just x) -> do
                                ctx' <- addName fi1 x ctx
                                ti <- traexpr body
                                return (str2varName "", (1, ti))
                        AnyPat fi1 Nothing -> do
                                ctx' <- addName fi1 dummyVarName ctx
                                ti <- traexpr body
                                return (str2varName "", (1, ti))
                return $ TmCase fi t alts'

transType :: MonadThrow m => Context -> Type -> m Ty
transType ctx = tratype
    where
        tratype :: MonadThrow m => Type -> m Ty
        tratype (VarType fi x) | not (isVarExist ctx x) = return $ TyId fi x
        tratype (VarType fi x) = do
                i <- getVarIndex fi ctx x
                return $ TyVar fi i (length ctx)
        tratype (ArrType fi ty1 ty2) = do
                ty1' <- tratype ty1
                ty2' <- tratype ty2
                return $ TyArr fi ty1' ty2'
        tratype (AllType fi x ty) = do
                let knK1 = KnStar -- tmp: forall x.t = forall x::*.t
                ctx' <- addName fi x ctx
                ty' <- transType ctx' ty
                return $ TyAll fi x knK1 ty'
        tratype (AbsType fi x ty) = do
                let knK1 = KnStar -- tmp: \x.t = \x:*.t
                ctx' <- addName fi x ctx
                ty' <- transType ctx' ty
                return $ TyAbs fi x knK1 ty'
        tratype (AppType fi ty1 ty2) = do
                ty1' <- tratype ty1
                ty2' <- tratype ty2
                return $ TyApp fi ty1' ty2'
        tratype (RecordType fi fieldtys) = do
                fields' <- forM fieldtys $ \(l, field) -> do
                        field' <- tratype field
                        return (l, field')
                return $ TyRecord fi fields'
        tratype (SumType fieldtys) = do
                fields' <- forM fieldtys $ \(_, l, field) -> do
                        field' <- mapM tratype field
                        return (l, field')
                return $ TyVariant dummyInfo fields' --tmp: dummyInfo

transDecl :: MonadThrow m => Decl -> StateT Context m (Name, Binding)
transDecl decl = StateT $ \ctx -> case decl of
        TypeDecl fi name ty -> do
                tyT <- transType ctx ty
                ctx' <- addName fi name ctx
                return ((name, TyAbbBind tyT Nothing), ctx')
        VarDecl fi f ty -> do
                tyT <- transType ctx ty
                ctx' <- addName fi f ctx
                return ((f, VarBind tyT), ctx')
        FuncDecl fi f e ty -> do
                t <- transExpr ctx ty e
                tyT <- transType ctx ty
                ctx' <- addName fi f ctx
                return ((f, TmAbbBind t (Just tyT)), ctx')

ir2core :: MonadThrow m => Context -> Decls -> m Commands
ir2core ctx (Decls modns decls (body, bodyty)) = (`evalStateT` ctx) $ do
        binds <- mapM transDecl decls
        ctx' <- get
        main <- transExpr ctx' bodyty body
        return $ Commands{C.imports = modns, C.binds = zip (map getInfo decls) binds, C.body = main}
