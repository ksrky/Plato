{-# LANGUAGE LambdaCase #-}

module Plato.Translation.AbstractToInternal where

import qualified Plato.Abstract.Syntax as A
import Plato.Common.Error
import Plato.Common.Info
import Plato.Common.Name
import qualified Plato.Internal.Syntax as I

import Control.Exception.Safe
import Control.Monad
import Control.Monad.Writer

transExpr :: MonadThrow m => A.Expr -> m I.Expr
transExpr (A.VarExpr fi x es) = do
        es' <- mapM transExpr es
        return $ foldl (\l r -> I.AppExpr (I.getInfo l) l r) (I.VarExpr fi x) es'
transExpr (A.ConExpr fi x es) = do
        es' <- mapM transExpr es
        return $ foldl (\l r -> I.AppExpr (I.getInfo l) l r) (I.ConExpr fi x) es'
transExpr (A.FloatExpr fi f) = return $ I.FloatExpr fi f
transExpr (A.StringExpr fi s) = return $ I.StringExpr fi s
transExpr (A.LamExpr fi xs e) = do
        e' <- transExpr e
        return $ foldr (I.LamExpr fi) e' xs
transExpr (A.LetExpr fi ds e) = do
        e' <- transExpr e
        ds' <- execWriterT $ transDecls (map A.Decl ds)
        return $ foldr (I.LetExpr fi) e' ds'
transExpr (A.CaseExpr fi1 e alts) = undefined {-do
                                              e' <- transExpr e
                                              alts' <- forM alts $ \(fi2, pat, body) -> do
                                                      pat' <- transExpr pat
                                                      body' <- transExpr body
                                                      return (fi2, pat', body')
                                              return $ I.CaseExpr fi1 e' alts'-}

{-
canonCase :: MonadThrow m => A.Expr -> [(Info, I.Pat, I.Expr)] -> m A.Expr
canonCase e alts = do
        wild <- execWriterT $ let
                transAlts [] = return ()
                transAlts (alt@(_, pat, _): alts) = case pat of
                        A.WildPat{} -> tell [alt]
                        _ -> tell [alt] >> transAlts alts
                 in transAlts alts
        undefined

transAlts :: MonadThrow m => [(Info, A.Pat, A.Expr)] -> WriterT [(Info, I.Pat, I.Expr)] m ()
transAlts [] = return ()
transAlts ((fi, pat, body) : alts) = do
        body' <- transExpr body
    where
        transPat :: MonadThrow m => A.Pat -> WriterT [(Info, I.Pat, I.Expr)] m ()
        transPat pat = case pat of
                A.ConPat fi1 c ps -> do
                        forM ps $ \p -> do
                                transPat p
                        transAlts alts
                A.VarPat fi1 x -> do
                        tell [(fi, I.AnyPat fi1 (Just x), body')]
                A.WildPat fi1 -> do
                        tell [(fi, I.AnyPat fi1 Nothing, body')]
-}

transType :: MonadThrow m => A.Type -> m I.Type
transType (A.ConType fi x) = return $ I.VarType fi x
transType (A.VarType fi x) = return $ I.VarType fi x
transType (A.AppType ty1 ty2) = do
        ty1' <- transType ty1
        ty2' <- transType ty2
        return $ I.AppType (I.getInfo ty2') ty1' ty2'
transType (A.ArrType fi ty1 ty2) = do
        ty1' <- transType ty1
        ty2' <- transType ty2
        return $ I.ArrType fi ty1' ty2'
transType (A.AllType fi xs ty) = do
        ty' <- transType ty
        return $ foldr (I.AllType fi) ty' xs

transDecls :: MonadThrow m => [A.TopDecl] -> WriterT [I.Decl] m ()
transDecls tds = do
        decs <- execWriterT $
                forM tds $ \case
                        A.Decl (A.FuncDecl fi f e) -> tell [(f, (fi, e))]
                        _ -> return ()
        forM_ tds $ \case
                A.Decl (A.FuncTyDecl fi1 f ty) -> case lookup f decs of -- tmp: multiple FuncDecl
                        Just (fi2, e) -> do
                                e' <- transExpr e
                                ty' <- transType ty
                                tell [I.FuncDecl fi1 f e' ty']
                        Nothing -> throwError fi1 $ name2str f ++ " lacks a binding"
                _ -> return ()

transTopDecl :: MonadThrow m => A.TopDecl -> WriterT [I.Decl] m ()
transTopDecl (A.DataDecl fi1 name params fields) = do
        fields' <- forM fields $ \(fi2, l, tys) -> do
                tys' <- mapM transType tys
                return (fi2, l, tys')
        let fieldty = I.SumType fields'
        tell [I.TypeDecl fi1 name (foldr (I.AbsType fi1) fieldty params)]
        forM_ fields' $ \(fi2, l, field) -> do
                let ty = foldr (I.AllType fi2) (foldr (I.ArrType fi2) fieldty field) params
                    args = map (str2varName . show) [1 .. length field]
                    tag = I.TagExpr fi2 l (map (I.VarExpr fi2) args)
                    exp = foldr (I.LamExpr fi2) tag args
                tell [I.FuncDecl fi2 l exp ty]
transTopDecl (A.TypeDecl fi name params ty) = do
        ty' <- transType ty
        tell [I.TypeDecl fi name (foldr (I.AbsType fi) ty' params)]
transTopDecl _ = return ()

transImpDecl :: MonadThrow m => A.ImpDecl -> WriterT [ModuleName] m ()
transImpDecl = undefined

transProgram :: MonadThrow m => [A.TopDecl] -> WriterT [I.Decl] m ()
transProgram tds = do
        mapM_ transTopDecl tds
        transDecls tds

abstract2internal :: MonadThrow m => ([A.ImpDecl], [A.TopDecl]) -> m I.Decls
abstract2internal (ids, tds) = do
        let imps = map (\(A.ImpDecl mn) -> mn) ids
        ds <- execWriterT $ transProgram tds
        return $ I.Decls{I.imports = imps, I.decls = ds}
