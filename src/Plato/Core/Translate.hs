module Plato.Core.Translate where

import Plato.Common.Name
import qualified Plato.Common.Name as N
import Plato.Core.Context
import Plato.Core.Error
import Plato.Core.Syntax
import Plato.Syntax.AST

import Control.Monad.State
import Control.Monad.Writer
import Data.List (elemIndex)

transExpr :: Ty -> Expr -> State Context Term
transExpr restty expr = case restty of
        TyAll n knK1 tyT2 -> do
                t2 <- transExpr tyT2 expr
                return $ TmTAbs n knK1 t2
        _ -> traexpr expr
    where
        traexpr :: Expr -> State Context Term
        traexpr (VarExpr fi x as) = do
                ctx <- get
                let t1 = TmVar (getVarIndex x ctx) (length ctx)
                foldM (\t a -> TmApp t <$> traexpr a) t1 as
        traexpr (ConExpr fi c as) = do
                ctx <- get
                let t1 = TmVar (getVarIndex c ctx) (length ctx)
                foldM (\t a -> TmApp t <$> traexpr a) t1 as
        traexpr (FloatExpr f) = do
                return $ TmFloat f
        traexpr (StringExpr s) = return $ TmString s
        traexpr (LamExpr fi x e) = do
                case restty of
                        TyArr tyT1 tyT2 -> do
                                x' <- pickfreshname x (VarBind tyT1)
                                t2 <- transExpr tyT2 e
                                return $ TmAbs x' tyT1 t2
                        _ -> error "array type required"
        traexpr (LetExpr fi ds e) = mkLet ds e
            where
                mkLet :: [Decl] -> Expr -> State Context Term
                mkLet (FuncDecl fi x e : ds) body = do
                        x' <- pickfreshname x NameBind
                        e' <- traexpr e
                        TmLet x' e' <$> mkLet ds body
                mkLet _ body = traexpr body
        traexpr (CaseExpr fi e pats) = do
                e' <- traexpr e
                pats' <- forM pats $ \(pat, body, _) -> do
                        pat' <- trapat pat
                        body' <- traexpr body
                        return (pat', body')
                return $ TmCase e' pats'

        trapat :: Expr -> State Context Term
        trapat (VarExpr fi x as) =
                if null as
                        then undefined
                        else error "function type cannot be pattern"
        trapat (ConExpr fi c as) = do
                t1 <- mapM trapat as
                bind <- getbindingFromName c
                case bind of
                        VarBind tyT2 -> return $ TmTag c t1 tyT2
                        _ -> error "type constructor required"
        trapat (FloatExpr f) = undefined
        trapat (StringExpr s) = undefined
        trapat _ = error "invalid expression for pattern matching"

transType :: Type -> State Context Ty
transType (ConType fi x) = do
        ctx <- get
        return $ TyVar (getVarIndex x ctx) (length ctx)
transType (VarType fi x) = do
        ctx <- get
        return $ TyVar (getVarIndex x ctx) (length ctx)
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

getVarIndex :: N.Name -> Context -> Int
getVarIndex var ctx = case elemIndex var (map fst ctx) of
        Just i -> i
        Nothing -> error $ "Unbound variable name: " ++ name2str var

entryPoint :: N.Name
entryPoint = str2name "main"

transDecl :: TopDecl -> WriterT [Command] (State Context) ()
transDecl (Decl (FuncDecl fi n e)) = do
        bind <- lift $ getbindingFromName n
        case bind of
                VarBind ty -> do
                        ctx <- get
                        let t = transExpr ty e `evalState` ctx
                        lift $ addbinding n NameBind
                        tell $
                                if n == entryPoint
                                        then [Eval t]
                                        else [Bind n (TmAbbBind t (Just ty))]
                _ -> error "VarBind required"
transDecl _ = return ()

transTopDecl :: TopDecl -> WriterT [Command] (State Context) ()
transTopDecl (DataDecl fi name params fields) = do
        ctx <- get
        fields' <- lift $
                forM fields $ \(l, f) -> do
                        let tyargs = mapM transType f `evalState` ctx
                        return (l, tyargs)
        tybody <- lift $ addbinders (zip params (repeat KnStar)) (TyVariant fields') -- tmp: param::*
        forM_ fields' $ \(l, tyargs) -> do
                let mkCtor :: [Ty] -> State Context Term
                    mkCtor [] = do
                        ctx <- get
                        let args = reverse (map (\i -> TmVar i (length ctx - i)) [1 .. (length tyargs)])
                        return (TmTag l args tybody)
                    mkCtor (a : as) = do
                        x <- pickfreshname dummyName (VarBind a) -- tmp: dummyName
                        TmAbs x a <$> mkCtor as
                    ctor = mkCtor tyargs `evalState` ctx
                    tyctor = foldr TyArr tybody tyargs
                tell [Bind l (TmAbbBind ctor (Just tyctor))]
                lift $ addbinding l (VarBind tyctor)
        tell [Bind name (TyAbbBind tybody Nothing)] --tmp: kind
        lift $ addbinding name NameBind
transTopDecl (TypeDecl fi name params ty) = do
        ctx <- get
        let tybody = transType ty `evalState` ctx
        tyT <- lift $ addbinders (zip params (repeat KnStar)) tybody --tmp: kind
        lift $ addbinding name NameBind
        tell [Bind name (TyAbbBind tyT Nothing)] -- tmp: kind
transTopDecl (Decl (FuncTyDecl fi x ty)) = do
        ctx <- get
        let tyT = transType ty `evalState` ctx
        lift $ addbinding x (VarBind tyT)
        tell [Bind x (VarBind tyT)]
transTopDecl _ = return ()

addbinders :: [(Name, Kind)] -> Ty -> State Context Ty
addbinders [] ty = return ty
addbinders ((x, k) : ps) ty = TyAbs x k <$> addbinders ps ty

registerTopDecl :: TopDecl -> State Context ()
registerTopDecl (DataDecl fi name params fields) = do
        let knK = foldr KnArr KnStar (replicate (length params) KnStar) --tmp
        isuniquename name NameBind
registerTopDecl _ = return ()

transProgram :: [TopDecl] -> WriterT [Command] (State Context) ()
transProgram tds = do
        lift $ mapM_ registerTopDecl tds
        mapM_ transTopDecl tds
        mapM_ transDecl tds
