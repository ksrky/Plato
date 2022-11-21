{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ViewPatterns #-}

module Plato.Transl.PsToTyp where

import Plato.Types.Error
import Plato.Types.Location
import Plato.Types.Monad
import Plato.Types.Name
import Plato.Types.Name.Global

import Plato.Typing.TypeCheck

import qualified Plato.Syntax.Parsing as P
import qualified Plato.Syntax.Typing as T

import Control.Exception.Safe
import Control.Monad.IO.Class
import Control.Monad.RWS
import Control.Monad.Writer as Writer
import qualified Data.Map.Strict as M
import Prettyprinter

transExpr :: MonadThrow m => P.LExpr GlbName -> m T.Expr
transExpr = traexpr
    where
        traexpr :: MonadThrow m => P.LExpr GlbName -> m T.Expr
        traexpr (L _ (P.VarE x)) = return $ T.VarE $ unLoc x
        traexpr (L _ (P.AppE e1 e2)) = do
                e1' <- traexpr e1
                e2' <- traexpr e2
                return $ T.AppE e1' e2'
        traexpr (L _ (P.OpE e1 op e2)) = do
                e1' <- traexpr e1
                e2' <- traexpr e2
                return $ T.AppE (T.AppE (T.VarE $ unLoc op) e1') e2'
        traexpr (L _ (P.LamE xs e1)) = do
                e1' <- traexpr e1
                return $ foldr (`T.AbsE` Nothing) e1' xs
        traexpr (L _ (P.LetE ds e)) = do
                (fds, _) <- transDecls ds --tmp:vardecls
                e' <- transExpr e
                return $ T.LetE fds e'
        traexpr (L _ (P.CaseE e alts)) = do
                e' <- traexpr e
                alts' <- execWriterT $ transAlts alts
                return $ T.CaseE e' Nothing alts'
            where
                transAlts :: MonadThrow m => [(P.LPat GlbName, P.LExpr GlbName)] -> WriterT [(T.Pat, T.Expr)] m ()
                transAlts [] = return ()
                transAlts ((pi, ei) : alts) =
                        traexpr ei >>= \ei' -> case unLoc pi of
                                P.ConP l ps -> do
                                        ps' <- mapM transPat ps
                                        tell [(T.ConP (unLoc l) ps', ei')]
                                        transAlts alts
                                P.VarP x -> tell [(T.VarP x, ei')]
                                P.WildP -> tell [(T.WildP, ei')]
        traexpr (L _ (P.FactorE e)) = unreachable $ "fixity resolution failed\n" ++ show e

transPat :: MonadThrow m => P.LPat GlbName -> m T.Pat
transPat (L _ (P.ConP c ps)) = do
        ps' <- mapM transPat ps
        return $ T.ConP (unLoc c) ps'
transPat (L _ (P.VarP x)) = return $ T.VarP x
transPat (L _ P.WildP) = return T.WildP

transType :: MonadThrow m => P.LType GlbName -> m T.Type
transType (L _ (P.VarT x)) = return $ T.VarT $ T.BoundTv x
transType (L _ (P.ConT x)) = return $ T.ConT $ unLoc x
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
        return $ T.AllT (map (\x -> (T.BoundTv x, Nothing)) xs) ty1'

transDecls :: MonadThrow m => [P.LDecl GlbName] -> m ([T.FuncD], [Located T.Decl])
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
                                        tell (mempty, [L sp $ T.VarD x ty'])
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
        when (length fields /= length fieldtys) $ throwUnexpErr "number of function bodies and signatures are not match"
        return ([T.FuncD var1 exp ty | (var1, exp) <- fields, (var2, ty) <- fieldtys, unLoc var1 == unLoc var2], vardecls)

transTopDecl :: MonadThrow m => ModuleName -> P.LTopDecl GlbName -> WriterT ([Located T.Decl], [Located T.FuncD], [Located T.Expr]) m ()
transTopDecl modn (L sp (P.DataD name params fields)) = do
        fields' <- Writer.lift $
                forM fields $ \(l, tys) -> do
                        tys' <- mapM transType tys
                        return (l, tys')
        let fieldty = T.SumT fields'
            kn = foldr T.ArrK T.StarK (replicate (length params) T.StarK)
            bodyty = T.RecT name kn fieldty
        tell ([L sp (T.TypeD name (foldr (`T.AbsT` Nothing) bodyty params))], [], [])
        forM_ fields' $ \(l, field) -> do
                let res_ty = foldl T.AppT (T.ConT $ internalName (DefTop $ Just modn) name) (map (T.VarT . T.BoundTv) params)
                    rho_ty = foldr T.ArrT res_ty field
                    sigma_ty =
                        if null params
                                then rho_ty
                                else T.AllT (map (\x -> (T.BoundTv x, Nothing)) params) rho_ty
                    tyargs = params
                    args = map (str2varName . show) [length params + 1 .. length params + length field]
                    tag = T.TagE (localName l) (map (T.VarE . newGlbName Local) args) fieldty
                    foldtag = T.AppE (T.FoldE res_ty) tag
                    exp = foldr (\(x, ty) -> T.AbsE (noLoc x) (Just ty)) foldtag (zip args field)
                    exp' = if null tyargs then exp else T.TAbsE tyargs exp
                tell ([], [L sp $ T.FuncD l exp' sigma_ty], [])
transTopDecl _ (L sp (P.TypeD name params ty1)) = do
        ty1' <- Writer.lift $ transType ty1
        tell ([L sp (T.TypeD name (foldr (`T.AbsT` Nothing) ty1' params))], [], [])
transTopDecl _ (L sp (P.Eval exp)) = do
        exp' <- transExpr exp
        tell ([], [], [L sp exp'])
transTopDecl _ _ = return ()

getDecls :: [P.LTopDecl GlbName] -> [P.LDecl GlbName]
getDecls tds = execWriter $
        forM tds $ \case
                L _ (P.Decl d) -> tell [d]
                _ -> return ()

ps2typ :: (MonadIO m, MonadThrow m) => P.Program GlbName -> Plato m T.Program
ps2typ (P.Program Nothing _ _) = unreachable "Renaming failed"
ps2typ (P.Program (Just modn) _ topds) = do
        (tydecs, condecs, exps) <- execWriterT $ mapM_ (transTopDecl $ unLoc modn) topds
        (fundecs, vardecs) <- transDecls (getDecls topds)
        env <- gets plt_tyEnv
        let env' =
                M.fromList
                        ( [(externalName (unLoc modn) var, ty) | T.FuncD var _ ty <- map unLoc condecs ++ fundecs]
                          ++ [(externalName (unLoc modn) var, ty) | L _ (T.VarD var ty) <- vardecs]
                        )
                        `M.union` env
        fundecs' <- mapM (typeCheck env') fundecs
        exps' <- forM exps $ \(L sp e) -> do
                (e', ty) <- typeInfer env' e
                unless (isBasicType ty) $ throwLocErr sp $ hsep ["Invalid type for evaluation expression:", pretty ty]
                return (L sp e', ty)
        modify $ \s -> s{plt_tyEnv = updateTyEnv env'}
        return $
                T.Program
                        { T.typ_modn = unLoc modn
                        , T.typ_decls = tydecs ++ map (T.ConD <$>) condecs ++ vardecs
                        , T.typ_binds = fundecs'
                        , T.typ_body = exps'
                        }