module Plato.Translation.AbstractToCore where

import Plato.Common.Error
import Plato.Common.Name
import Plato.Core.Context
import Plato.Core.Syntax
import Plato.Syntax.Abstract

import Control.Exception.Safe
import Control.Monad.State
import Control.Monad.Writer
import Plato.Common.Vect

transExpr :: (MonadThrow m, MonadFail m) => Ty -> Expr -> Core m Term
transExpr restty expr = case restty of
        TyAll (n, knK1) tyT2 -> do
                t2 <- transExpr tyT2 expr
                return $ TmTAbs (n, knK1) t2
        _ -> traexpr expr
    where
        traexpr :: (MonadThrow m, MonadFail m) => Expr -> Core m Term
        traexpr (VarExpr fi x as) = do
                ctx <- get
                i <- getVarIndex x ctx
                let t1 = TmVar i (length ctx)
                {-if (x == str2name "plus") && length ctx == 10 then error $ show i ++ show (vmap fst ctx) else-}
                foldM (\t a -> TmApp t <$> traexpr a) t1 as
        traexpr (ConExpr fi c as) = do
                ctx <- get
                i <- getVarIndex c ctx
                let t1 = TmVar i (length ctx)
                foldM (\t a -> TmApp t <$> traexpr a) t1 as
        traexpr (FloatExpr f) = return $ TmFloat f
        traexpr (StringExpr s) = return $ TmString s
        traexpr (LamExpr fi [x] e) = case typeShift 1 restty of
                TyArr tyT1 tyT2 -> do
                        x' <- pickfreshname x NameBind
                        t2 <- transExpr tyT2 e
                        return $ TmAbs (x', tyT1) t2
                _ -> throwError fi "array type required"
        traexpr (LetExpr fi [d] e1) = case d of
                FuncDecl fi x e2 -> do
                        x' <- pickfreshname x NameBind
                        t1 <- transExpr (typeShift 1 restty) e1
                        t2 <- transExpr (typeShift 1 restty) e2
                        return $ TmLet (x', t1) t2
                _ -> traexpr e1
        traexpr (CaseExpr fi e alts) = do
                t <- traexpr e
                alts' <- forM alts $ \(pat, body, fi1) -> case pat of
                        VarExpr fi2 x as | null as -> do
                                ti <- traexpr body
                                return (x, (0, ti)) -- tmp
                        VarExpr fi2 _ _ -> throwError fi2 "function type cannot be a pattern"
                        ConExpr fi2 c as -> do
                                ti <- evalLocal (mapM_ addarg as) (traexpr body)
                                bind <- getbindingFromName c
                                case bind of
                                        VarBind tyT21 -> do
                                                --tmp: Check whether it's tycon or function
                                                when (length as /= countTyArr tyT21) (throwError fi2 "Wrong number of arguments")
                                        _ -> throwError fi2 "Type constructor required"
                                return (c, (length as, ti))
                            where
                                addarg :: MonadThrow m => Expr -> Core m ()
                                addarg (VarExpr fi3 x as) = do
                                        unless (null as) (throwError fi3 "function type cannot be a pattern")
                                        addbinding x NameBind
                                addarg _ = throwError fi2 "illegal expression for pattern matching"
                        FloatExpr f -> undefined
                        StringExpr s -> undefined
                        _ -> throwError fi1 "illegal expression for pattern matching"
                return $ TmCase t alts'
        traexpr e = unreachable $ "canonicalize failed at " ++ show e

evalLocal :: MonadThrow m => Core m () -> Core m a -> Core m a
evalLocal local f = do
        ctx <- get
        ctx' <- lift $ local `execStateT` ctx
        val <- lift $ f `evalStateT` ctx'
        put ctx
        return val

transType :: MonadThrow m => Type -> Core m Ty
transType (ConType fi x) = do
        ctx <- get
        i <- getVarIndex x ctx
        return $ TyVar i (length ctx)
transType (VarType fi x) = do
        ctx <- get
        i <- getVarIndex x ctx
        return $ TyVar i (length ctx)
transType (AppType ty1 ty2) = do
        ty1' <- transType ty1
        ty2' <- transType ty2
        return $ TyApp ty1' ty2'
transType (ArrType fi ty1 ty2) = do
        ty1' <- transType ty1
        ty2' <- transType ty2
        return $ TyArr ty1' ty2'
transType (AllType fi [x] ty) = do
        x' <- pickfreshname x (TyVarBind KnStar) -- tmp: KnStar
        ty' <- transType ty
        return $ TyAll (x', KnStar) ty' -- tmp: forall x.t = forall x::*.t
transType ty = unreachable $ "canonicalize failed at " ++ show ty

entryPoint :: Name
entryPoint = str2name "main"

transDecl :: (MonadThrow m, MonadFail m) => TopDecl -> WriterT [Command] (Core m) ()
transDecl (Decl (FuncDecl fi n e)) = do
        bind <- lift $ getbindingFromName n
        case bind of
                VarBind ty -> do
                        ctx <- get
                        idx <- getVarIndex n ctx
                        let ty' = typeShift (idx + 1) ty
                        t <- lift $ lift $ transExpr ty' e `evalStateT` ctx
                        lift $ addbinding n NameBind
                        tell $
                                if n == entryPoint
                                        then [Eval t]
                                        else [Bind n (TmAbbBind t (Just ty'))]
                _ -> throwError fi "VarBind required"
transDecl _ = return ()

transTopDecl :: (MonadThrow m, MonadFail m) => TopDecl -> WriterT [Command] (Core m) ()
transTopDecl (DataDecl fi name params fields) = do
        -- Define a data type
        ctx <- get
        params' <- lift $ zipWithM pickfreshname params (repeat NameBind)
        fields' <- forM fields $ \(l, f) -> lift $ do
                paramtys <- mapM transType f
                return (l, paramtys)
        let tyT = foldr TyAbs (TyVariant fields') (zip params' (repeat KnStar)) -- tmp: param::*
        put ctx
        tell [Bind name (TyAbbBind tyT Nothing)] -- tmp: kind
        lift $ addbinding name (TyAbbBind tyT Nothing)
        -- Define constructors as function
        forM_ fields $ \(l, field) -> do
                ctx <- get
                params <- lift $
                        (`evalStateT` ctx) $
                                forM field $ \tyi -> do
                                        pi <- pickfreshname dummyName NameBind
                                        ptyi <- transType tyi
                                        return (pi, ptyi)
                TyAbbBind tyT _ <- lift $ getbindingFromName name
                let tag = TmTag l (reverse (map (\i -> TmVar i (length ctx)) [0 .. (length params -1)])) tyT
                    ctor = foldr TmAbs tag params
                    ctorty = foldr (TyArr . snd) tyT params
                tell [Bind l (TmAbbBind ctor (Just ctorty))]
                lift $ addbinding l (VarBind ctorty)
transTopDecl (TypeDecl fi name params ty) = do
        ctx <- get
        tybody <- lift $ lift $ transType ty `evalStateT` ctx
        let tyT = foldr TyAbs tybody (zip params (repeat KnStar)) --tmp: kind
        lift $ addbinding name (VarBind tybody)
        tell [Bind name (TyAbbBind tyT Nothing)] -- tmp: kind
transTopDecl (Decl (FuncTyDecl fi x ty)) = do
        ctx <- get
        tyT <- lift $ lift $ transType ty `evalStateT` ctx
        lift $ addbinding x (VarBind tyT)
        tell [Bind x (VarBind tyT)]
transTopDecl _ = return ()

registerTopDecl :: MonadThrow m => TopDecl -> WriterT [Command] (Core m) ()
registerTopDecl (DataDecl fi name params fields) = do
        let knK = foldr KnArr KnStar (replicate (length params) KnStar) --tmp
        lift $ isuniquename name (TyVarBind knK)
        tell [Bind name (TyVarBind knK)]
registerTopDecl _ = return ()

transProgram :: (MonadThrow m, MonadFail m) => [TopDecl] -> WriterT [Command] (Core m) ()
transProgram tds = do
        mapM_ registerTopDecl tds
        mapM_ transTopDecl tds
        mapM_ transDecl tds
