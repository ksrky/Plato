{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns #-}

module Plato.Transl.PsToTyp where

import Plato.Common.Error
import Plato.Common.GlbName
import Plato.Common.Name
import Plato.Common.SrcLoc
import qualified Plato.Syntax.Parsing as P
import Plato.Syntax.Typing
import qualified Plato.Syntax.Typing as T
import Plato.Typing.TypeCheck

import Control.Exception.Safe
import Control.Monad.IO.Class
import Control.Monad.Writer as Writer
import qualified Data.Map.Strict as M
import Prettyprinter

transName :: Located Name -> GlbName
transName (L sp n) = GlbName Internal n sp

transExpr :: MonadThrow m => Located P.Expr -> m T.Expr
transExpr = traexpr
    where
        traexpr :: MonadThrow m => Located P.Expr -> m T.Expr
        traexpr (L _ (P.VarE x)) = return $ T.VarE $ transName x
        traexpr (L _ (P.AppE e1 e2)) = do
                e1' <- traexpr e1
                e2' <- traexpr e2
                return $ T.AppE e1' e2'
        traexpr (L _ (P.OpE e1 op e2)) = do
                e1' <- traexpr e1
                e2' <- traexpr e2
                return $ T.AppE (T.AppE (T.VarE $ transName op) e1') e2'
        traexpr (L _ (P.LamE xs e1)) = do
                e1' <- traexpr e1
                return $ foldr (\x -> T.AbsE (transName x) Nothing) e1' xs
        traexpr (L _ (P.LetE ds e)) = do
                (fds, _) <- transDecls ds
                e' <- transExpr e
                return $ T.LetE fds e'
        traexpr (L _ (P.CaseE e alts)) = do
                e' <- traexpr e
                alts' <-
                        execWriterT $
                                let transAlts :: MonadThrow m => [(Located P.Pat, Located P.Expr)] -> WriterT [(T.Pat, T.Expr)] m ()
                                    transAlts [] = return ()
                                    transAlts ((pi, ei) : alts) =
                                        traexpr ei >>= \ei' -> case unLoc pi of
                                                P.ConP l ps -> do
                                                        ps' <- mapM transPat ps
                                                        tell [(T.ConP (transName l) ps', ei')]
                                                        transAlts alts
                                                P.VarP x -> tell [(T.VarP (transName x), ei')]
                                                P.WildP -> tell [(T.WildP, ei')]
                                 in transAlts alts
                return $ T.CaseE e' Nothing alts'
        traexpr (L _ (P.FactorE e)) = unreachable $ "fixity resolution failed\n" ++ show e

transPat :: MonadThrow m => Located P.Pat -> m T.Pat
transPat (L _ (P.ConP c ps)) = do
        ps' <- mapM transPat ps
        return $ T.ConP (transName c) ps'
transPat (L _ (P.VarP x)) = do
        return $ T.VarP $ transName x
transPat (L _ P.WildP) = do
        return T.WildP

transType :: MonadThrow m => Located P.Type -> m T.Type
transType (L _ (P.VarT x)) = return $ T.VarT $ BoundTv $ transName x
transType (L _ (P.ConT x)) = return $ T.ConT $ transName x
transType (L _ (P.AppT ty1 ty2)) = do
        ty1' <- transType ty1
        ty2' <- transType ty2
        return $ T.AppT ty1' ty2'
transType (L _ (P.ArrT ty1 ty2)) = do
        ty1' <- transType ty1
        ty2' <- transType ty2
        return $ T.ArrT ty1' ty2'
transType (L _ (P.AllT xs ty1)) = do
        ty1' <- transType ty1
        return $ T.AllT (map (\x -> (BoundTv $ transName x, Nothing)) xs) ty1'

transDecls :: MonadThrow m => [Located P.Decl] -> m ([T.FuncD], [Located T.Decl])
transDecls decs = do
        let fns = [unLoc x | (unLoc -> P.FuncD x _ _) <- decs]
            ftns = [unLoc x | (unLoc -> P.FuncTyD x _) <- decs]
        (fieldtys, vardecls) <- execWriterT $
                forM decs $ \case
                        L sp (P.FuncTyD x ty)
                                | unLoc x `elem` fns -> do
                                        ty' <- Writer.lift $ transType ty
                                        tell ([(x, ty')], mempty)
                                | otherwise -> do
                                        ty' <- Writer.lift $ transType ty
                                        tell (mempty, [L sp $ T.VarD (transName x) ty'])
                        _ -> return ()
        fields <- execWriterT $
                forM decs $ \case
                        L sp (P.FuncD x args e)
                                | unLoc x `elem` ftns -> do
                                        e' <- Writer.lift $ transExpr $ if null args then e else cLnL args e $ P.LamE args e
                                        tell [(x, e')]
                                | otherwise -> do
                                        Writer.lift $ throwLocErr sp "lacks type signature"
                        _ -> return ()
        when (length fields /= length fieldtys) $ throwUnexpectedErr "number of function bodies and signatures are not match"
        return ([T.FuncD (transName var1) exp ty | (var1, exp) <- fields, (var2, ty) <- fieldtys, unLoc var1 == unLoc var2], vardecls)

transTopDecl :: MonadThrow m => Located P.TopDecl -> WriterT ([Located T.Decl], [Located T.FuncD], [Located T.Expr]) m ()
transTopDecl (L sp (P.DataD name params fields)) = do
        fields' <- Writer.lift $
                forM fields $ \(l, tys) -> do
                        tys' <- mapM transType tys
                        return (transName l, tys')
        let fieldty = T.SumT fields'
            bodyty = T.RecT (transName name) fieldty
        tell ([L sp (T.TypeD (transName name) (foldr (\x -> T.AbsT (transName x) Nothing) bodyty params))], [], [])
        forM_ fields' $ \(l, field) -> do
                let res_ty = foldl T.AppT (T.ConT $ transName name) (map (T.VarT . BoundTv . transName) params)
                    rho_ty = foldr T.ArrT res_ty field
                    sigma_ty =
                        if null params
                                then rho_ty
                                else T.AllT (map (\x -> (BoundTv $ transName x, Nothing)) params) rho_ty
                    tyargs = params
                    args = map (noLoc . str2varName . show) [length params + 1 .. length params + length field]
                    tag = T.TagE l (map (T.VarE . transName) args) fieldty
                    foldtag = T.AppE (T.FoldE res_ty) tag
                    exp = foldr (\(x, ty) -> T.AbsE (transName x) (Just ty)) foldtag (zip args field)
                    exp' = if null tyargs then exp else T.TAbsE (map transName tyargs) exp
                tell ([], [L sp $ T.FuncD l exp' sigma_ty], [])
transTopDecl (L sp (P.TypeD name params ty1)) = do
        ty1' <- Writer.lift $ transType ty1
        tell ([L sp (T.TypeD (transName name) (foldr (\x -> T.AbsT (transName x) Nothing) ty1' params))], [], [])
transTopDecl (L sp (P.Eval exp)) = do
        exp' <- transExpr exp
        tell ([], [], [L sp exp'])
transTopDecl _ = return ()

getDecls :: [Located P.TopDecl] -> [Located P.Decl]
getDecls tds = execWriter $
        forM tds $ \case
                L _ (P.Decl d) -> tell [d]
                _ -> return ()

ps2typ :: (MonadIO m, MonadThrow m) => T.TypEnv -> P.Program -> m (T.Program, T.TypEnv)
ps2typ env (P.Program modn _ topds) = do
        (tydecs, condecs, exps) <- execWriterT $ mapM_ transTopDecl topds
        (fundecs, vardecs) <- transDecls (getDecls topds)
        let env' = M.fromList [(var, ty) | T.FuncD var _ ty <- map unLoc condecs ++ fundecs] `M.union` env
        fundecs' <- mapM (typeCheck env') fundecs
        exps' <- forM exps $ \(L sp e) -> do
                (e', ty) <- typeInfer env' e
                unless (isBasicType ty) $ throwLocErr sp $ hsep ["Invalid type for evaluation expression:", pretty ty]
                return (L sp e', ty)
        return
                ( T.Program
                        { T.mmodule = unLoc <$> modn
                        , T.decls = tydecs ++ map (T.ConD <$>) condecs ++ vardecs
                        , T.binds = fundecs'
                        , T.body = exps'
                        }
                , env'
                )
