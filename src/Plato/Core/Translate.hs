module Plato.Core.Translate where

import Plato.Common.Name
import Plato.Core.Context
import Plato.Core.Error
import Plato.Core.Syntax
import Plato.Syntax.AST

import Control.Exception.Safe
import Control.Monad.State
import Control.Monad.Writer
import Data.List (elemIndex)

transExpr :: (MonadThrow m, MonadFail m) => Ty -> Expr -> Core m Term
transExpr restty expr = case restty of
        TyAll n knK1 tyT2 -> do
                t2 <- transExpr tyT2 expr
                return $ TmTAbs n knK1 t2
        _ -> traexpr expr
    where
        traexpr :: (MonadThrow m, MonadFail m) => Expr -> Core m Term
        traexpr (VarExpr fi x as) = do
                ctx <- get
                i <- getVarIndex x ctx
                let t1 = TmVar i (length ctx)
                foldM (\t a -> TmApp t <$> traexpr a) t1 as
        traexpr (ConExpr fi c as) = do
                ctx <- get
                i <- getVarIndex c ctx
                let t1 = TmVar i (length ctx)
                foldM (\t a -> TmApp t <$> traexpr a) t1 as
        traexpr (FloatExpr f) = return $ TmFloat f
        traexpr (StringExpr s) = return $ TmString s
        traexpr (LamExpr fi x e) = case restty of
                TyArr tyT1 tyT2 -> do
                        x' <- pickfreshname x (VarBind tyT1)
                        t2 <- transExpr tyT2 e
                        return $ TmAbs x' tyT1 t2
                _ -> throwError fi "array type required"
        traexpr (LetExpr fi ds e) = mkLet ds e
            where
                mkLet :: (MonadThrow m, MonadFail m) => [Decl] -> Expr -> Core m Term
                mkLet (FuncDecl fi x e : ds) body = do
                        x' <- pickfreshname x NameBind
                        e' <- traexpr e
                        TmLet x' e' <$> mkLet ds body
                mkLet _ body = traexpr body
        traexpr (CaseExpr fi e alts) = do
                t <- traexpr e
                alts' <- forM alts $ \(pat, body, fi1) -> case pat of
                        VarExpr fi2 x as | null as -> do
                                t <- traexpr e
                                ti <- traexpr body
                                return (x, ti) --tmp
                        VarExpr fi2 _ _ -> throwError fi2 "function type cannot be a pattern"
                        ConExpr fi2 c as -> do
                                mapM_ addarg as
                                ti <- traexpr body
                                bind <- getbindingFromName c
                                case bind of
                                        VarBind tyT21 -> do
                                                --tmp: Check whether it's tycon or function
                                                let tys = getTyArr tyT21
                                                when (length as /= length tys) $ throwError fi2 "Wrong number of arguments"
                                        _ -> throwError fi2 "Type constructor required"
                                return (c, ti)
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
transType (FunType fi ty1 ty2) = do
        ty1' <- transType ty1
        ty2' <- transType ty2
        return $ TyArr ty1' ty2'
transType (AllType x ty) = do
        x' <- pickfreshname x (TyVarBind KnStar) -- tmp
        ty' <- transType ty
        return $ TyAll x KnStar ty' -- tmp: forall x.t = forall x::*.t

getVarIndex :: MonadThrow m => Name -> Context -> Core m Int
getVarIndex var ctx = case elemIndex var (map fst ctx) of
        Just i -> return i
        Nothing -> throwString $ "Unbound variable name: " ++ name2str var

entryPoint :: Name
entryPoint = str2name "main"

transDecl :: (MonadThrow m, MonadFail m) => TopDecl -> WriterT [Command] (Core m) ()
transDecl (Decl (FuncDecl fi n e)) = do
        bind <- lift $ getbindingFromName n
        case bind of
                VarBind ty -> do
                        ctx <- get
                        t <- lift $ lift $ transExpr ty e `evalStateT` ctx
                        lift $ addbinding n NameBind
                        tell $
                                if n == entryPoint
                                        then [Eval t]
                                        else [Bind n (TmAbbBind t (Just ty))]
                _ -> throwError fi "VarBind required"
transDecl _ = return ()

transTopDecl :: (MonadThrow m, MonadFail m) => TopDecl -> WriterT [Command] (Core m) ()
transTopDecl (DataDecl fi name params fields) = do
        ctx <- get
        fields' <- lift $
                forM fields $ \(l, f) -> do
                        tyargs <- lift $ mapM transType f `evalStateT` ctx
                        return (l, tyargs)
        let tybody = addbinders (zip params (repeat KnStar)) (TyVariant fields') -- tmp: param::*
        tell [Bind name (TyAbbBind tybody Nothing)] --tmp: kind
        lift $ addbinding name (TyAbbBind tybody Nothing)
        ctx <- get
        forM_ fields $ \(l, f) -> do
                ctx <- get
                tyargs <- lift $ mapM transType f `evalStateT` ctx
                TyAbbBind tybody _ <- lift $ getbindingFromName name
                let mkCtor :: [Ty] -> State Context Term
                    mkCtor [] = do
                        let args = reverse (map (\i -> TmVar i (length ctx - i)) [0 .. (length tyargs -1)])
                        return (TmTag l args tybody)
                    mkCtor (a : as) = do
                        x <- pickfreshname dummyName (VarBind a) -- tmp: dummyName
                        TmAbs x a <$> mkCtor as
                    ctor = mkCtor tyargs `evalState` ctx
                    tyctor = foldr TyArr tybody tyargs
                tell [Bind l (TmAbbBind ctor (Just tyctor))]
                lift $ addbinding l (VarBind tyctor)
transTopDecl (TypeDecl fi name params ty) = do
        ctx <- get
        tybody <- lift $ lift $ transType ty `evalStateT` ctx
        let tyT = addbinders (zip params (repeat KnStar)) tybody --tmp: kind
        lift $ addbinding name NameBind
        tell [Bind name (TyAbbBind tyT Nothing)] -- tmp: kind
transTopDecl (Decl (FuncTyDecl fi x ty)) = do
        ctx <- get
        tyT <- lift $ lift $ transType ty `evalStateT` ctx
        lift $ addbinding x (VarBind tyT)
        tell [Bind x (VarBind tyT)]
transTopDecl _ = return ()

addbinders :: [(Name, Kind)] -> Ty -> Ty
addbinders [] ty = ty
addbinders ((x, k) : ps) ty = TyAbs x k (addbinders ps ty)

registerTopDecl :: MonadThrow m => TopDecl -> Core m ()
registerTopDecl (DataDecl fi name params fields) = do
        let knK = foldr KnArr KnStar (replicate (length params) KnStar) --tmp
        isuniquename name NameBind
registerTopDecl _ = return ()

transProgram :: (MonadThrow m, MonadFail m) => [TopDecl] -> WriterT [Command] (Core m) ()
transProgram tds = do
        lift $ mapM_ registerTopDecl tds
        mapM_ transTopDecl tds
        mapM_ transDecl tds
