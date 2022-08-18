{-# LANGUAGE LambdaCase #-}

module Plato.Translation.AbstractToIR where

import qualified Plato.Abstract.Syntax as A
import Plato.Common.Error
import Plato.Common.Info
import Plato.Common.Name
import Plato.Common.Vect
import Plato.IR.Rename
import qualified Plato.IR.Syntax as I
import Plato.IR.Utils as I

import Control.Exception.Safe
import Control.Monad
import Control.Monad.Writer
import Data.List (partition, union)

transExpr :: MonadThrow m => Memo -> A.Expr -> m I.Expr
transExpr memo = traexpr
    where
        traexpr :: MonadThrow m => A.Expr -> m I.Expr
        traexpr (A.VarExpr fi x) = return $ case look x (store memo) of
                Just r -> I.ProjExpr fi (I.VarExpr fi r) x
                Nothing -> I.VarExpr fi x
        traexpr (A.AppExpr e1 e2) = do
                e1' <- traexpr e1
                e2' <- traexpr e2
                return $ I.AppExpr (getInfo e2') e1' e2'
        traexpr (A.TAppExpr fi e1 t2) = do
                e1' <- traexpr e1
                t2' <- transType t2
                return $ I.TAppExpr fi e1' t2'
        traexpr (A.LamExpr fi xs e) = do
                e' <- traexpr e
                return $ foldr (I.LamExpr fi) e' xs
        traexpr (A.LetExpr fi ds e) = do
                e' <- traexpr e
                (d, _) <- transDecls memo (map A.Decl ds)
                return $ I.LetExpr fi d e'
        traexpr (A.CaseExpr fi e alts) = do
                e' <- traexpr e
                alts' <-
                        execWriterT $
                                let transAlts :: MonadThrow m => [(A.Pat, A.Expr)] -> WriterT [(I.Pat, I.Expr)] m ()
                                    transAlts [] = return ()
                                    transAlts (alt@(pi, ei) : alts) =
                                        traexpr ei >>= \ei' -> case pi of
                                                A.ConPat fi1 l ps -> do
                                                        ps' <- mapM transPat ps
                                                        tell [(I.ConPat fi1 l ps', ei')]
                                                        transAlts alts
                                                A.VarPat fi1 x -> tell [(I.AnyPat fi1 (Just x), ei')]
                                                A.WildPat fi1 -> tell [(I.AnyPat fi1 Nothing, ei')]
                                 in transAlts alts
                return $ I.CaseExpr fi e' alts'

transPat :: MonadThrow m => A.Pat -> m I.Pat
transPat (A.ConPat fi c ps) = do
        ps' <- mapM transPat ps
        return $ I.ConPat fi c ps'
transPat (A.VarPat fi x) = do
        return $ I.AnyPat fi (Just x)
transPat (A.WildPat fi) = do
        return $ I.AnyPat fi Nothing

transType :: MonadThrow m => A.Type -> m I.Type
transType (A.ConType fi x) = return $ I.VarType fi x
transType (A.VarType fi x) = return $ I.VarType fi x
transType (A.AppType ty1 ty2) = do
        ty1' <- transType ty1
        ty2' <- transType ty2
        return $ I.AppType (getInfo ty2') ty1' ty2'
transType (A.ArrType fi ty1 ty2) = do
        ty1' <- transType ty1
        ty2' <- transType ty2
        return $ I.ArrType fi ty1' ty2'
transType (A.AllType fi xs ty) = do
        ty' <- transType ty
        return $ foldr (I.AllType fi) ty' xs

entry :: Name
entry = str2varName "main"

transDecls :: MonadThrow m => Memo -> [A.TopDecl] -> m (I.Decl, Memo)
transDecls memo tds = do
        fs <- execWriterT $
                forM tds $ \case
                        A.Decl (A.FuncDecl fi f e) | f /= entry -> tell [f]
                        _ -> return ()
        let rcd = fresh memo
            memo' = Memo{store = foldr cons (store memo) (zip fs (repeat rcd)), level = level memo + 1}
        fieldtys <- execWriterT $
                forM tds $ \case
                        A.Decl (A.FuncTyDecl fi1 f ty) | f `elem` fs -> do
                                ty' <- transType ty
                                tell [(f, ty')]
                        _ -> return ()
        fields <- execWriterT $
                forM tds $ \case
                        A.Decl (A.FuncDecl fi f e) | f /= entry -> do
                                e' <- transExpr memo' e
                                tell [(f, e')]
                        _ -> return ()
        return (I.FuncDecl dummyInfo rcd (I.RecordExpr dummyInfo fields) (I.RecordType dummyInfo fieldtys), memo')

transFuncTyDecls :: MonadThrow m => [A.TopDecl] -> m [I.Decl]
transFuncTyDecls tds = do
        fs <- execWriterT $
                forM tds $ \case
                        A.Decl (A.FuncDecl fi f e) -> tell [f]
                        _ -> return ()
        execWriterT $
                forM tds $ \case
                        A.Decl (A.FuncTyDecl fi f ty) | f `notElem` fs -> do
                                ty' <- transType ty
                                tell [I.VarDecl dummyInfo f ty']
                        _ -> return ()

transTopDecl :: MonadThrow m => A.TopDecl -> WriterT [I.Decl] m ()
transTopDecl (A.DataDecl fi1 name params fields) = do
        fields' <- forM fields $ \(fi2, l, tys) -> do
                tys' <- mapM transType tys
                return (fi2, l, tys')
        let fieldty = I.SumType fields'
        tell [I.TypeDecl fi1 name (foldr (I.AbsType fi1) fieldty params)]
        forM_ fields' $ \(fi2, l, field) -> do
                let bodyty = foldl (I.AppType fi2) (I.VarType dummyInfo name) (map (I.VarType dummyInfo) params)
                    ty = foldr (I.AllType fi2) (foldr (I.ArrType fi2) bodyty field) params
                    tyargs = map (str2tyVarName . show) [1 .. length params]
                    args = map (str2varName . show) [length params + 1 .. length params + length field]
                    tag = I.TagExpr fi2 l (map (I.VarExpr fi2) args)
                    exp = foldr (I.LamExpr fi2) tag (tyargs ++ args)
                tell [I.FuncDecl fi2 l exp ty]
transTopDecl (A.TypeDecl fi name params ty) = do
        ty' <- transType ty
        tell [I.TypeDecl fi name (foldr (I.AbsType fi) ty' params)]
transTopDecl _ = return ()

abstract2ir :: MonadThrow m => ([A.ImpDecl], [A.TopDecl]) -> m I.Decls
abstract2ir (ids, tds) = do
        let modns = map (\(A.ImpDecl mn) -> mn) ids
        decls <- execWriterT $ mapM_ transTopDecl tds
        vardecls <- transFuncTyDecls tds
        (bind, memo) <- transDecls emptyMemo tds
        main <- transEntry memo bind
        return $ I.Decls modns (decls ++ vardecls) main -- tmp: order of I.TypeDecl and I.FuncDecl
    where
        transEntry :: MonadThrow m => Memo -> I.Decl -> m (I.Expr, I.Type)
        transEntry memo bind = do
                body <- execWriterT $
                        forM tds $ \case
                                A.Decl (A.FuncDecl fi f e) | f == entry -> do
                                        e' <- transExpr memo e
                                        tell [e']
                                _ -> return ()
                bodyty <- execWriterT $
                        forM tds $ \case
                                A.Decl (A.FuncTyDecl fi f ty) | f == entry -> do
                                        ty' <- transType ty
                                        tell [ty']
                                _ -> return ()
                case (body, bodyty) of
                        ([], []) -> return (I.LetExpr dummyInfo bind (I.RecordExpr dummyInfo []), I.RecordType dummyInfo [])
                        ([e], [ty]) -> return (I.LetExpr dummyInfo bind e, ty)
                        ([_], []) -> throwString "main function lacks a type signature"
                        ([], [_]) -> throwString " main function lacks a binding"
                        _ -> throwString "duplicate main function"
