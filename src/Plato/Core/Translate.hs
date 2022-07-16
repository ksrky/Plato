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
                x' <- pickfreshname x NameBind
                case restty of
                        TyArr tyT1 tyT2 -> do
                                t <- transExpr tyT2 e
                                return $ TmAbs x' tyT1 t
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
                        pat' <- traexpr pat
                        body' <- traexpr body
                        return (pat', body')
                return $ TmCase e' pats'

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

transDecl :: TopDecl -> State Context [Command]
transDecl (Decl (FuncDecl fi n e)) = do
        bind <- getbindingFromName n
        case bind of
                VarBind ty -> do
                        t <- transExpr ty e
                        addbinding n (TmAbbBind t (Just ty))
                        return $
                                if n == entryPoint
                                        then [Eval t]
                                        else [Bind n (TmAbbBind t (Just ty))]
                _ -> error "VarBind required"
transDecl _ = return []

transTopDecl :: TopDecl -> State Context [Command]
transTopDecl (DataDecl fi name params constrs) = do
        constrs' <- forM constrs $ \(con, field) -> do
                field' <- mapM transType field
                return (con, field')
        tyT <- addbinders (zip params (repeat KnStar)) (TyVariant constrs') -- tmp: param::*
        forM_ constrs' $ \(n, tys) -> do
                let aty = foldr TyArr tyT tys
                addbinding n (VarBind aty)
        addbinding name (TyAbbBind tyT Nothing)
        return [Bind name (TyAbbBind tyT Nothing)]
transTopDecl (TypeDecl fi name params t) = do
        ty <- transType t
        ty' <- addbinders (zip params (repeat KnStar)) ty --tmp
        addbinding name (TyAbbBind ty' Nothing)
        return [Bind name (TyAbbBind ty' Nothing)]
transTopDecl (Decl (FuncTyDecl fi n t)) = do
        ty <- transType t
        addbinding n (VarBind ty)
        return [Bind n (VarBind ty)]
transTopDecl _ = return []

addbinders :: [(Name, Kind)] -> Ty -> State Context Ty
addbinders [] ty = return ty
addbinders ((x, k) : ps) ty = TyAbs x k <$> addbinders ps ty

transProgram :: [TopDecl] -> State Context [Command]
transProgram tds = do
        bs1 <- mapM transTopDecl tds
        bs2 <- mapM transDecl tds
        return $ concat bs1 ++ concat bs2
