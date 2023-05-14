{-# LANGUAGE LambdaCase #-}

module Plato.TypToCore (typ2core) where
{-}
import Control.Exception.Safe
import Control.Monad
import Control.Monad.RWS
import Control.Monad.Reader
import Data.Maybe (fromMaybe)

import Plato.Common.Error
import Plato.Common.Location
import Plato.Common.Monad
import Plato.Common.Name
import Plato.Common.Path
import Plato.Core.Context
import Plato.Core.Debug
import Plato.Syntax.Core qualified as C
import Plato.Syntax.Typing qualified as T

elabPath :: Path -> Reader Context C.Term
elabPath (PIdent id) = do
        i <- getVarIndex id
        return $ C.TmVar i (mkInfo id)
elabPath (PDot root field) = do
        root' <- elabPath root
        return $ C.TmProj root' (unLoc field)

elabExpr :: T.Expr -> Reader Context C.Term
elabExpr (T.VarE p) = elabPath p
    where
        elabPath :: Path -> Reader Context C.Term
        elabPath (PIdent id) = do
                i <- getVarIndex id
                return $ C.TmVar i (mkInfo id)
        elabPath (PDot root field) = do
                root' <- elabPath root
                return $ C.TmProj root' (unLoc field)
elabExpr (T.AppE fun arg) = do
        t1 <- elabExpr (unLoc fun)
        t2 <- elabExpr (unLoc arg)
        return $ C.TmApp t1 t2
elabExpr (T.AbsE id (Just ty) body) = do
        tyT1 <- elabType ty
        t2 <- local (addName id) $ elabExpr (unLoc body)
        return $ C.TmAbs (mkInfo id) tyT1 t2
elabExpr (T.AbsE _ Nothing _) = unreachable ""
elabExpr (T.PAbsE pat (Just ty) body) = undefined
elabExpr (T.PAbsE _ Nothing _) = unreachable ""
elabExpr (T.TAppE fun argtys) = do
        t1 <- elabExpr (unLoc fun)
        tys2 <- mapM elabType argtys
        return $ foldl C.TmTApp t1 tys2
elabExpr (T.TAbsE qnts body) = do
        qnts' <- forM qnts $ \case
                (tv, Just kn) -> do
                        let knK = elabKind kn
                        return (T.unTyVar tv, knK)
                (_, Nothing) -> unreachable "Kind inference failed"
        t1 <- local (\ctx -> foldr addName ctx (map fst qnts')) $ elabExpr (unLoc body)
        return $ foldr (\(x, kn) -> C.TmTAbs (mkInfo x) kn) t1 qnts'
elabExpr (T.LetE decs e2) = undefined
elabExpr T.MatchE{} = undefined

elabType :: T.Type -> Reader Context C.Type
elabType (T.VarT tv) = do
        i <- getVarIndex (T.unTyVar tv)
        return $ C.TyVar i (mkInfo $ T.unTyVar tv)
elabType (T.ConT tc) = elabPath tc
    where
        elabPath :: Path -> Reader Context C.Type
        elabPath (PIdent id) = do
                i <- getVarIndex id
                return $ C.TyVar i (mkInfo id)
        elabPath (PDot root field) = do
                root' <- elabPath root
                return $ C.TmProj root' (unLoc field)
elabType (T.ArrT ty1 ty2) = do
        tyT1 <- elabType (unLoc ty1)
        tyT2 <- elabType (unLoc ty2)
        return $ C.TyArr tyT1 tyT2
elabType (T.AllT tvs ty) = do
        args <- forM tvs $ \case
                (tv, Just kn) -> do
                        knK <- transKind kn
                        return (T.unTyVar tv, knK)
                _ -> unreachable "Kind inference failed"
        let ctx' = foldl (flip addName) ctx (map (unLoc . fst) args)
        tyT2 <- transType ctx' (unLoc ty)
        return $ foldr (\(x, knK1) -> C.TyAll (mkInfo x) knK1) tyT2 args
elabType (T.AppT ty1 ty2) = do
        tyT1 <- elabType (unLoc ty1)
        tyT2 <- elabType (unLoc ty2)
        return $ C.TyApp tyT1 tyT2
elabType (T.AbsT x (Just kn) ty) = do
        knK1 <- transKind kn
        let ctx' = addName (unLoc x) ctx
        tyT2 <- transType ctx' (unLoc ty)
        return $ C.TyAbs (mkInfo x) knK1 tyT2
elabType (T.AbsT x Nothing ty) = undefined
{-elabType (T.RecT x (Just kn) ty) = do
        knK1 <- transKind kn
        let ctx' = addName (unLoc x) ctx
        tyT2 <- transType ctx' (unLoc ty)
        return $ C.TyRec (mkInfo x) knK1 tyT2-}
elabType T.MetaT{} = unreachable "Zonking failed"

elabKind :: T.Kind -> C.Kind
elabKind = undefined

typ2core = undefined-}

{-transExpr :: MonadThrow m => C.Context -> T.Expr -> m C.Term
transExpr ctx = trexpr
    where
        trexpr :: MonadThrow m => T.Expr -> m C.Term
        trexpr (T.VarE p) = do
                i <- getVarIndex ctx p
                return $ C.TmVar i (mkInfo p)
        trexpr (T.AppE fun arg) = do
                t1 <- trexpr (unLoc fun)
                t2 <- trexpr (unLoc arg)
                return $ C.TmApp t1 t2
        trexpr (T.AbsE x (Just ty1) e2) = do
                tyT1 <- transType ctx ty1
                let ctx' = addName (unLoc x) ctx
                t2 <- transExpr ctx' (unLoc e2)
                return $ C.TmAbs (mkInfo x) tyT1 t2
        trexpr (T.PAbsE p (Just ty1) e2) = do
                return $ C.TmAbs (mkInfoFromSpan (getLoc p)) tyT1 t2
        trexpr (T.TAppE e1 tys) = do
                t1 <- trexpr (unLoc e1)
                tyTs' <- mapM (transType ctx) tys
                return $ foldl C.TmTApp t1 tyTs'
        trexpr (T.TAbsE qnts e1) = do
                args <- forM qnts $ \case
                        (tv, Just kn) -> do
                                knK <- transKind kn
                                return (T.unTyVar tv, knK)
                        (_, Nothing) -> unreachable "Kind inference failed"
                let ctx' = foldl (flip addName) ctx (map (unLoc . fst) args)
                t1 <- transExpr ctx' (unLoc e1)
                return $ foldr (\(x, kn) -> C.TmTAbs (mkInfo x) kn) t1 args
        trexpr (T.LetE decs e2) = do
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
        trexpr (T.MatchE matches alts) = undefined
        {-do
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
                transAlts ((unLoc con, ti') : acc) rest
        transAlts acc ((L _ (T.VarP x), body) : _) = do
                let ctx' = addName (unLoc x) ctx
                ti <- transExpr ctx' (unLoc body)
                let ti' = C.TmAbs (mkInfo x) undefined ti
                return (acc, Just ti')
        transAlts acc ((L _ T.WildP, body) : _) = do
                let ctx' = addName wildcard ctx
                ti <- transExpr ctx' (unLoc body)
                let ti' = C.TmAbs (mkInfo $ noLoc wildcard) undefined ti
                return (acc, Just ti')-}
        trexpr _ = unreachable "Plato.Transl.TypToCore.transExpr"

transType :: MonadThrow m => C.Context -> T.Type -> m C.Type
transType ctx = undefined

transKind :: MonadThrow m => T.Kind -> m C.Kind
transKind = undefined

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
        {-trtype (T.RecT x (Just kn) ty) = do
                knK1 <- transKind kn
                let ctx' = addName (unLoc x) ctx
                tyT2 <- transType ctx' (unLoc ty)
                return $ C.TyRec (mkInfo x) knK1 tyT2-}
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
                tyTi <- transType ctx tyi
                return (unLoc xi, tyTi)
        let ctx' = addName dummyVN ctx
        fields <- forM bnds $ \(xi, ei) -> do
                ti <- transExpr ctx' (unLoc ei) -- temp: renaming
                return (unLoc xi, ti)
        let fix = C.TmFix (C.TmAbs (mkInfo name) (C.TyRecord fieldtys) (C.TmRecord fields))
        idx <- getVarIndex ctx name
        let ts = map (\(x, ty) -> (x, C.TmProj (C.TmVar idx (mkInfo name)) x, ty)) fieldtys
        return [(unLoc name, fix)]

typ2core :: MonadThrow m => T.Module -> Plato m C.Module
typ2core (T.Module modn bnds decs tydecs body) = do
        ctx <- gets plt_context
        (ctx', tybinds) <- transDecls ctx decs -- temp: type decl before con decl
        funbind <- transTopBinds modn ctx' binds
        let ctx'' = uncurry addBinding funbind ctx
        evals <- mapM (transEval ctx'') exps
        return (C.Module modn (tybinds ++ [funbind]) evals)-}