{-# LANGUAGE MultiParamTypeClasses #-}

module Plato.Translation.InternalToCore where

import Plato.Common.Error
import Plato.Core.Context
import Plato.Core.Syntax
import Plato.Internal.Syntax

import Control.Exception.Safe
import Control.Monad.State
import Plato.Common.Name

transExpr :: MonadThrow m => Type -> Expr -> Core m Term
transExpr restty exp = case restty of
        AllType fi x ty -> do
                let knK1 = KnStar -- tmp
                x' <- pickfreshname x (TyVarBind knK1)
                t2 <- transExpr ty exp
                return $ TmTAbs fi (x', knK1) t2
        _ -> traexpr exp
    where
        traexpr :: MonadThrow m => Expr -> Core m Term
        traexpr (VarExpr fi x as) = do
                ctx <- get
                i <- name2index fi ctx x
                let t1 = TmVar fi i (length ctx)
                foldM (\t a -> TmApp (getInfo a) t <$> traexpr a) t1 as
        traexpr (ConExpr fi c as) = do
                ctx <- get
                i <- name2index fi ctx c
                let t1 = TmVar fi i (length ctx)
                foldM (\t a -> TmApp (getInfo a) t <$> traexpr a) t1 as
        traexpr (FloatExpr fi f) = return $ TmFloat fi f
        traexpr (StringExpr fi s) = return $ TmString fi s
        traexpr (LamExpr fi x e) = do
                x' <- pickfreshname x NameBind
                case restty of
                        ArrType _ ty1 ty2 -> do
                                tyT1 <- evalCore $ transType ty1
                                t2 <- transExpr ty2 e
                                return $ TmAbs fi (x', tyT1) t2
                        _ -> throwError fi "Couldn't match expected type"
        traexpr (LetExpr fi d e1) = case d of
                FuncDecl _ f e2 ty -> do
                        f' <- pickfreshname f NameBind
                        t2 <- transExpr ty e2
                        t1 <- traexpr e1
                        return $ TmLet fi (f', t2) t1
                _ -> unreachable "Type declaration in let binding"
        traexpr (CaseExpr fi e alts) = do
                t <- traexpr e
                alts' <- forM alts $ \(fi1, pat, body) -> case pat of
                        VarExpr fi2 x as | null as -> do
                                ti <- traexpr body
                                return (x, (0, ti)) -- tmp
                        VarExpr fi2 _ _ -> throwError fi2 "function type cannot be a pattern"
                        ConExpr fi2 c as -> do
                                ti <- evalCore (mapM_ addarg as >> traexpr body)
                                tyT21 <- getVarBindFromName fi c
                                return (c, (length as, ti))
                            where
                                addarg :: MonadThrow m => Expr -> Core m ()
                                addarg (VarExpr fi3 x as) = do
                                        unless (null as) (throwError fi3 "function type cannot be a pattern")
                                        addbinding x NameBind
                                addarg _ = throwError fi2 "illegal pattern"
                        FloatExpr _ f -> undefined
                        StringExpr _ s -> undefined
                        _ -> throwError fi1 "illegal expression for pattern matching"
                return $ TmCase fi t alts'
        traexpr (TagExpr fi l as) = do
                as' <- mapM traexpr as
                tyT <- case restty of
                        SumType{} -> transType restty
                        AllType{} -> transType restty
                        ArrType{} -> transType restty
                        _ -> throwError fi $ "illegal type for TagExpr: " ++ show restty
                return $ TmTag fi l as' tyT

transType :: MonadThrow m => Type -> Core m Ty
transType (VarType fi x) = do
        ctx <- get
        i <- name2index fi ctx x
        return $ TyVar i (length ctx)
transType (AppType ty1 ty2) = do
        ty1' <- transType ty1
        ty2' <- transType ty2
        return $ TyApp ty1' ty2'
transType (ArrType fi ty1 ty2) = do
        ty1' <- transType ty1
        ty2' <- transType ty2
        return $ TyArr ty1' ty2'
transType (AbsType fi x ty) = do
        let knK1 = KnStar -- tmp: \x.t = \x:*.t
        x' <- pickfreshname x (TyVarBind knK1)
        ty' <- transType ty
        return $ TyAll (x', knK1) ty'
transType (AllType fi x ty) = do
        let knK1 = KnStar -- tmp: forall x.t = forall x::*.t
        x' <- pickfreshname x (TyVarBind knK1)
        ty' <- transType ty
        return $ TyAll (x', knK1) ty'
transType (SumType fields) = do
        fields' <- forM fields $ \(_, l, field) -> do
                field' <- mapM transType field
                return (l, field')
        return $ TyVariant fields'

entryPoint :: Name
entryPoint = str2name "main"

transDecl :: MonadThrow m => Decl -> Core m Command
transDecl (TypeDecl fi name ty) = do
        tyT <- evalCore $ transType ty
        addbinding name (TyAbbBind tyT Nothing)
        return $ Bind name (TyAbbBind tyT Nothing)
transDecl (FuncDecl fi f e ty) = do
        t <- evalCore $ transExpr ty e
        tyT <- evalCore $ transType ty
        addbinding f (VarBind tyT)
        return $
                if f == entryPoint
                        then Eval t
                        else Bind f (TmAbbBind t (Just tyT))

registerDecl :: MonadThrow m => Decl -> Core m Command
registerDecl (TypeDecl fi name ty) = do
        isuniquename fi name NameBind
        return $ Bind name NameBind
registerDecl (FuncDecl fi f e ty) = do
        isuniquename fi f NameBind
        return $ Bind f NameBind

internal2core :: MonadThrow m => Context -> [Decl] -> m [Command]
internal2core ctx decs = (`evalStateT` ctx) $ do
        cmd1 <- mapM registerDecl decs
        cmd2 <- mapM transDecl decs
        return $ cmd1 ++ cmd2