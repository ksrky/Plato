{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TupleSections #-}

module Plato.Transl.TypToCore where

import Control.Exception.Safe
import Control.Monad
import Control.Monad.RWS
import Data.Maybe (fromMaybe)

import Plato.Common.Error
import Plato.Common.Location
import Plato.Common.Monad
import Plato.Common.Name
import Plato.Core.Context
import Plato.Core.Debug

import qualified Plato.Syntax.Core as C
import qualified Plato.Syntax.Typing as T

transExpr :: MonadThrow m => C.Context -> T.Expr -> m C.Term
transExpr ctx = trexpr
    where
        trexpr :: MonadThrow m => T.Expr -> m C.Term
        trexpr (T.VarE x) = do
                i <- getVarIndex ctx x
                return $ C.TmVar i (mkInfo x)
        trexpr (T.AppE e1 e2) = do
                t1 <- trexpr (unLoc e1)
                t2 <- trexpr (unLoc e2)
                return $ C.TmApp t1 t2
        trexpr (T.AbsE x (Just ty1) e2) = do
                tyT1 <- transType ctx ty1
                let ctx' = addName (unLoc x) ctx
                t2 <- transExpr ctx' (unLoc e2)
                return $ C.TmAbs (mkInfo x) tyT1 t2
        trexpr (T.PAbsE p (Just ty1) e2) = do
                tyT1 <- transType ctx ty1
                let ctx' = foldl (\ctx _ -> addName dummyVN ctx) ctx [1 .. pat2argnum (unLoc p)]
                t2 <- transExpr ctx' (unLoc e2)
                return $ C.TmAbs (mkInfoFromSpan (getLoc p)) tyT1 t2
        trexpr (T.TAppE e1 tys) = do
                t1 <- trexpr (unLoc e1)
                tyTs' <- mapM (transType ctx) tys
                return $ foldl C.TmTApp t1 tyTs'
        trexpr (T.TAbsE tvs e1) = do
                args <- forM tvs $ \case
                        (tv, Just kn) -> do
                                knK <- transKind kn
                                return (T.tyVarName tv, knK)
                        (_, Nothing) -> unreachable "Kind inference failed"
                let ctx' = foldl (flip addName) ctx (map (unLoc . fst) args)
                t1 <- transExpr ctx' (unLoc e1)
                return $ foldr (\(x, kn) -> C.TmTAbs (mkInfo x) kn) t1 args
        trexpr (T.LetE bnds decs e2) = do
                fieldtys <- forM decs $ \(xi, tyi) -> do
                        tyTi <- transType ctx tyi
                        return (unLoc xi, tyTi)
                let ctx' = addName dummyVN ctx
                fields <- forM bnds $ \(xi, ei) -> do
                        ti <- transExpr ctx' (unLoc ei) -- temp: renaming
                        return (unLoc xi, ti)
                let t1 = C.TmFix $ C.TmAbs dummyInfo (C.TyRecord fieldtys) (C.TmRecord fields) -- temp: substitution
                t2 <- transExpr ctx' (unLoc e2)
                return $ C.TmLet dummyInfo t1 t2
        trexpr (T.CaseE e1 (Just ty2) alts) = do
                t1 <- trexpr (unLoc e1)
                tyT2 <- transType ctx ty2
                (alts', def) <- transAlts [] alts
                let def' = fromMaybe (unreachable "Non-exhaustive error") def -- temp
                return $ C.TmCase t1 tyT2 alts' def'
            where
                transAlts :: MonadThrow m => [(Name, C.Term)] -> [(T.LPat, T.LExpr)] -> m ([(Name, C.Term)], Maybe C.Term)
                transAlts acc [] = return (acc, Nothing)
                transAlts acc ((L _ (T.ConP con pats), body) : rest) = do
                        xs <- (concat <$>) $
                                forM pats $ \(L sp p) -> case p of
                                        T.VarP x -> return [x]
                                        T.WildP -> return [noLoc wildcard]
                                        T.ConP{} -> throwLocErr sp "pattern argument" --tmp
                        let ctx' = addNameList (map unLoc xs) ctx
                        ti <- transExpr ctx' (unLoc body)
                        let ti' = foldl (\e x -> C.TmAbs (mkInfo x) undefined e) ti xs --tmp
                        transAlts ((con, ti') : acc) rest
                transAlts acc ((L _ (T.VarP x), body) : _) = do
                        let ctx' = addName (unLoc x) ctx
                        ti <- transExpr ctx' (unLoc body)
                        let ti' = C.TmAbs (mkInfo x) undefined ti
                        return (acc, Just ti')
                transAlts acc ((L _ T.WildP, body) : _) = do
                        let ctx' = addName wildcard ctx
                        ti <- transExpr ctx' (unLoc body)
                        let ti' = C.TmAbs (mkInfo $ noLoc wildcard) undefined ti
                        return (acc, Just ti')
        trexpr _ = unreachable "Plato.Transl.TypToCore.transExpr"

pat2argnum :: T.Pat -> Int
pat2argnum (T.ConP _ pats) = sum (map (pat2argnum . unLoc) pats)
pat2argnum T.VarP{} = 1
pat2argnum T.WildP = 0

transType :: MonadThrow m => C.Context -> T.Type -> m C.Type
transType ctx = trtype
    where
        trtype :: MonadThrow m => T.Type -> m C.Type
        trtype (T.VarT tv) = do
                i <- getVarIndex ctx (T.tyVarName tv)
                return $ C.TyVar i (mkInfo $ T.tyVarName tv)
        trtype (T.ConT tc) = do
                i <- getVarIndex ctx tc
                return $ C.TyVar i (mkInfo tc)
        trtype (T.ArrT ty1 ty2) = do
                tyT1 <- trtype (unLoc ty1)
                tyT2 <- trtype (unLoc ty2)
                return $ C.TyArr tyT1 tyT2
        trtype (T.AllT tvs ty) = do
                args <- forM tvs $ \case
                        (tv, Just kn) -> do
                                knK <- transKind kn
                                return (T.tyVarName tv, knK)
                        _ -> unreachable "Kind inference failed"
                let ctx' = foldl (flip addName) ctx (map (unLoc . fst) args)
                tyT2 <- transType ctx' (unLoc ty)
                return $ foldr (\(x, knK1) -> C.TyAll (mkInfo x) knK1) tyT2 args
        trtype (T.AppT ty1 ty2) = do
                tyT1 <- trtype (unLoc ty1)
                tyT2 <- trtype (unLoc ty2)
                return $ C.TyApp tyT1 tyT2
        trtype (T.AbsT x (Just kn) ty) = do
                knK1 <- transKind kn
                let ctx' = addName (unLoc x) ctx
                tyT2 <- transType ctx' (unLoc ty)
                return $ C.TyAbs (mkInfo x) knK1 tyT2
        trtype (T.RecT x (Just kn) ty) = do
                knK1 <- transKind kn
                let ctx' = addName (unLoc x) ctx
                tyT2 <- transType ctx' (unLoc ty)
                return $ C.TyRec (mkInfo x) knK1 tyT2
        trtype (T.SumT fields) = do
                fields' <- forM fields $ \(l, tys) -> (unLoc l,) <$> mapM (trtype . unLoc) tys
                return $ C.TyVariant fields'
        trtype T.MetaT{} = unreachable "Zonking failed"
        trtype _ = unreachable "Plato.Transl.TypToCore.transType"

transKind :: MonadThrow m => T.Kind -> m C.Kind
transKind T.StarK = return C.KnStar
transKind T.MetaK{} = unreachable "Kind inference failed"
transKind (T.ArrK kn1 kn2) = C.KnArr <$> transKind kn1 <*> transKind kn2

{-}
transDecl :: MonadThrow m => C.Context -> (T.LName, T.Decl) -> m (Name, C.Binding)
transDecl _ (con, T.TypDecl kn) = do
        knK <- transKind kn
        return (unLoc con, C.TyVarBind knK)
transDecl ctx (con, T.ValDecl ty) = do
        tyT <- transType ctx ty
        let con_ty = case lookupContext (unLoc con) ctx of
                Just (C.TyAbbBind ty _) -> ty
                _ -> unreachable "Plato.Transl.TypToCore.transDecl"
        t <- conTerm tyT con_ty
        return (unLoc con, C.TmAbbBind t tyT)

conTerm :: C.Type -> C.Type -> m C.Term
conTerm = undefined

transDecls :: MonadThrow m => C.Context -> T.Decls -> m (C.Context, [(Name, C.Binding)])
transDecls ctx [] = return (ctx, [])
transDecls ctx (dec : rest) = do
        bind <- transDecl ctx dec
        let ctx' = uncurry addBinding bind ctx
        (ctx'', binds) <- transDecls ctx' rest
        return (ctx'', bind : binds)-}

transTypDecls ctx [] = (ctx, [])
transTypDecls ctx decs = undefined

transBinds :: MonadThrow m => C.Context -> T.LName -> T.Binds -> T.Decls -> m [(Name, C.Term)]
transBinds ctx name bnds decs = do
        fieldtys <- forM decs $ \(xi, tyi) -> do
                tyTi <- transType ctx (unLoc tyi)
                return (unLoc xi, tyTi)
        let ctx' = addName dummyVN ctx
        fields <- forM bnds $ \(xi, ei) -> do
                ti <- transExpr ctx' (unLoc ei) -- temp: renaming
                return (unLoc xi, ti)
        let fix = C.TmFix (C.TmAbs (mkInfo name) (C.TyRecord fieldtys) (C.TmRecord fields))
        idx <- getVarIndex ctx name
        let ts = map (\(x, ty) -> (x, C.TmProj (C.TmVar idx (mkInfo name)) x, ty)) fieldtys
        return [(unLoc name, fix)]

{-
transMod :: C.Context -> T.Mod -> T.Binds
transMod ctx (T.ModName x) = undefined
transMod ctx (T.ModBinds bnds) = bnds

transBind :: MonadThrow m => C.Context -> T.Bind -> m C.Binding
transBind ctx (T.FunBind e) = do
        t <- transExpr ctx (unLoc e)
        undefined
transBind ctx (T.TypBind ty) = undefined
transBind ctx (T.ModBind sig mod) = undefined

transSig :: C.Context -> T.Sig -> T.Decls
transSig ctx (T.SigName x) = undefined
transSig ctx (T.SigDecls decs) = undefined

transEval :: MonadThrow m => C.Context -> (Located T.Expr, T.Type) -> m C.Term
transEval ctx (e, ty) = do
        t <- transExpr ctx (unLoc e) -- temp: renaming
        _ <- transType ctx ty
        return t

transDecl :: MonadThrow m => C.Context -> T.Decl -> m C.Binding
transDecl ctx (T.ValDecl e) = undefined
transDecl ctx (T.TypDecl e) = undefined-}

typ2core :: MonadThrow m => T.Module -> Plato m C.Module
typ2core (T.Module modn bnds decs tydecs body) = do
        ctx <- gets plt_context
        (ctx', tybinds) <- transDecls ctx decs -- temp: type decl before con decl
        funbind <- transTopBinds modn ctx' binds
        let ctx'' = uncurry addBinding funbind ctx
        evals <- mapM (transEval ctx'') exps
        return (C.Module modn (tybinds ++ [funbind]) evals)