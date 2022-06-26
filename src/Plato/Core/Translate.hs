module Plato.Core.Translate where

import Plato.Common.Name
import qualified Plato.Common.Name as N
import Plato.Core.Context
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
        traexpr (VarExpr x p) = do
                ctx <- get
                return $ TmVar (getVarIndex x ctx) (length ctx)
        traexpr (FloatExpr f) = do
                return $ TmFloat f
        traexpr (StringExpr s) = return $ TmString s
        traexpr (CallExpr x as p) = do
                v <- traexpr (VarExpr x p)
                app v as
            where
                app t [] = return t
                app t (a : as) = do
                        a' <- traexpr a
                        app (TmApp t a') as
        traexpr (LamExpr x e p) = do
                x' <- pickfreshname x NameBind
                case restty of
                        TyArr tyT1 tyT2 -> do
                                t <- transExpr tyT2 e
                                return $ TmAbs x' tyT1 t
                        _ -> error "array type required"
        traexpr (LetExpr ds e p) = do
                e' <- traexpr e
                mkLet e' ds
            where
                mkLet t (FuncDecl x e p : ds) = do
                        e' <- traexpr e
                        mkLet (TmLet x e' t) ds
                mkLet t _ = return t
        traexpr (CaseExpr e pats p) = do
                e' <- traexpr e
                pats' <- forM pats $ \(pat, body, pos) -> do
                        pat' <- traexpr pat
                        body' <- traexpr body
                        return (pat', body')
                return $ TmCase e' pats'

transType :: Type -> State Context Ty
transType (ConType x p) = do
        ctx <- get
        return $ TyVar (getVarIndex x ctx) (length ctx)
transType (VarType x p) = do
        ctx <- get
        return $ TyVar (getVarIndex x ctx) (length ctx)
transType (AppType ty1 ty2) = do
        ty1' <- transType ty1
        ty2' <- transType ty2
        return $ TyApp ty1' ty2'
transType (FunType ty1 ty2 p) = do
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

transDecl :: TopDecl -> State Context [(Name, Term)]
transDecl (Decl (FuncDecl n e p)) = do
        bind <- getbindingFromName n
        case bind of
                VarBind ty -> do
                        t <- transExpr ty e
                        return [(n, t)]
                _ -> error "VarBind required"
transDecl _ = return []

transTopDecl :: TopDecl -> State Context ()
transTopDecl (DataDecl n params constrs p) = do
        constrs' <- forM constrs $ \(con, field) -> do
                field' <- mapM transType field
                return (con, field')
        ty <- addbinders (zip params (repeat KnStar)) (TyVariant constrs')
        addbinding n (TyAbbBind ty Nothing)
transTopDecl (TypeDecl n params t p) = do
        ty <- transType t
        ty' <- addbinders (zip params (repeat KnStar)) ty --tmp
        addbinding n (TyAbbBind ty' Nothing)
transTopDecl (Decl (FuncTyDecl n t p)) = do
        ty <- transType t
        addbinding n (VarBind ty)
        return ()
transTopDecl _ = return ()

addbinders :: [(Name, Kind)] -> Ty -> State Context Ty
addbinders [] ty = return ty
addbinders ((x, k) : ps) ty = TyAbs x k <$> addbinders ps ty

transProgram :: [TopDecl] -> State Context [(Name, Term)]
transProgram tds = do
        mapM_ transTopDecl tds
        concat <$> mapM transDecl tds
