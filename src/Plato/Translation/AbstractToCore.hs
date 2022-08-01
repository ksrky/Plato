module Plato.Translation.AbstractToCore where

import Plato.Abstract.Syntax
import Plato.Common.Error
import Plato.Common.Name
import Plato.Core.Context
import Plato.Core.Syntax

import Control.Exception.Safe
import Control.Monad.State
import Control.Monad.Writer
import Plato.Common.Info
import Plato.Common.Vect

transExpr :: (MonadThrow m, MonadFail m) => Ty -> Expr -> Core m Term
transExpr restty expr = case restty of
        TyAll (x, knK1) tyT2 -> do
                pickfreshname x (TyVarBind knK1)
                t2 <- transExpr tyT2 expr
                return $ TmTAbs dummyInfo (x, knK1) t2
        _ -> traexpr expr
    where
        traexpr :: (MonadThrow m, MonadFail m) => Expr -> Core m Term
        traexpr (VarExpr fi x as) = do
                ctx <- get
                --when (x == str2name "fmap") (error $ show (vmap fst ctx))
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
        traexpr (LamExpr fi [x] e) = case typeShift 1 restty of
                TyArr tyT1 tyT2 -> do
                        x' <- pickfreshname x NameBind
                        t2 <- transExpr tyT2 e
                        return $ TmAbs fi (x', tyT1) t2
                _ -> throwError fi "Couldn't match expected type"
        traexpr (LetExpr fi [d] e1) = case d of
                FuncDecl fi f e2 -> do
                        f' <- pickfreshname f NameBind
                        t1 <- transExpr (typeShift 1 restty) e1
                        t2 <- transExpr (typeShift 1 restty) e2 --tmp
                        return $ TmLet fi (f', t1) t2
                FuncTyDecl fi f ty -> traexpr e1
        traexpr (CaseExpr fi e alts) = do
                t <- traexpr e
                alts' <- forM alts $ \(fi1, pat, body) -> case pat of
                        VarExpr fi2 x as | null as -> do
                                ti <- traexpr body
                                return (x, (0, ti)) -- tmp
                        VarExpr fi2 _ _ -> throwError fi2 "function type cannot be a pattern"
                        ConExpr fi2 c as -> do
                                ti <- evalLocal (mapM_ addarg as) (traexpr body)
                                tyT21 <- getVarBindFromName fi c
                                when (length as /= countTyArr tyT21) (throwError fi2 "Wrong number of arguments")
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
                --error $ show alts'
                return $ TmCase fi t alts'
        traexpr e = unreachable $ "canonicalize failed at " ++ show e

evalLocal :: MonadThrow m => Core m () -> Core m a -> Core m a
evalLocal local f = do
        ctx <- get
        local
        val <- f
        put ctx
        return val

transType :: MonadThrow m => Type -> Core m Ty
transType (ConType fi x) = do
        ctx <- get
        i <- name2index fi ctx x
        return $ TyVar i (length ctx)
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
transType (AllType fi [x] ty) = do
        x' <- pickfreshname x (TyVarBind KnStar) -- tmp: KnStar
        ty' <- transType ty
        return $ TyAll (x', KnStar) ty' -- tmp: forall x.t = forall x::*.t
transType ty = unreachable $ "canonicalize failed at " ++ show ty

entryPoint :: Name
entryPoint = str2name "main"

transDecl :: (MonadThrow m, MonadFail m) => TopDecl -> WriterT [Command] (Core m) ()
transDecl (Decl (FuncDecl fi f e)) = do
        ctx <- get
        tyT <- lift $ getVarBindFromName fi f
        when (f == str2name "id") $ error $ show (vmap fst ctx) ++ "\n" ++ show tyT
        t <- lift $ lift $ transExpr tyT e `evalStateT` ctx
        lift $ addbinding f NameBind
        tell $
                if f == entryPoint
                        then [Eval t]
                        else [Bind f (TmAbbBind t (Just tyT))]
transDecl _ = return ()

transTopDecl :: (MonadThrow m, MonadFail m) => TopDecl -> WriterT [Command] (Core m) ()
transTopDecl (DataDecl fi name params fields) = do
        -- Define a data type
        ctx <- get
        params' <- lift $ zipWithM pickfreshname params (repeat NameBind)
        fields' <- forM fields $ \(_, l, f) -> lift $ do
                paramtys <- mapM transType f
                return (l, paramtys)
        let tyT = foldr TyAbs (TyVariant fields') (zip params' (repeat KnStar)) -- tmp: param::*
        put ctx
        tell [Bind name (TyAbbBind tyT Nothing)] -- tmp: kind
        lift $ addbinding name (TyAbbBind tyT Nothing)
        -- Define constructors as function
        forM_ fields $ \(_, l, field) -> do
                ctx <- get
                ps <- lift $ do
                        zipWithM_ pickfreshname params (repeat NameBind)
                        forM field $ \tyi -> do
                                pi <- pickfreshname dummyName NameBind
                                ptyi <- transType tyi
                                return (pi, ptyi)
                tyT <- lift $ getTyAbbFromName fi name
                let arity = length field
                    tag = TmTag dummyInfo l (reverse (map (\i -> TmVar dummyInfo i (length ctx + arity)) [0 .. (arity -1)])) tyT
                    ctor = foldr (TmAbs dummyInfo) tag ps
                    ctorty = foldr (TyArr . snd) tyT ps
                put ctx
                tell [Bind l (TmAbbBind ctor (Just ctorty))]
                lift $ addbinding l (VarBind ctorty)
transTopDecl (TypeDecl fi name params ty) = do
        ctx <- get
        tybody <- lift $ lift $ transType ty `evalStateT` ctx
        let tyT = foldr TyAbs tybody (zip params (repeat KnStar)) --tmp: kind
        lift $ addbinding name (VarBind tybody)
        tell [Bind name (TyAbbBind tyT Nothing)] -- tmp: kind
transTopDecl (Decl (FuncTyDecl fi f ty)) = do
        ctx <- get
        tyT <- lift $ lift $ transType ty `evalStateT` ctx
        lift $ addbinding f (VarBind tyT)
        tell [Bind f (VarBind tyT)]
transTopDecl _ = return ()

registerTopDecl :: MonadThrow m => TopDecl -> WriterT [Command] (Core m) ()
registerTopDecl (DataDecl fi name params fields) = do
        let knK = foldr KnArr KnStar (replicate (length params) KnStar) --tmp
        lift $ isuniquename fi name (TyVarBind knK)
        tell [Bind name (TyVarBind knK)]
registerTopDecl _ = return ()

transProgram :: (MonadThrow m, MonadFail m) => [TopDecl] -> WriterT [Command] (Core m) ()
transProgram tds = do
        mapM_ registerTopDecl tds
        mapM_ transTopDecl tds
        mapM_ transDecl tds
