{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ViewPatterns #-}

module Plato.Transl.PsToTyp where

import Plato.Common.Error
import Plato.Common.Location
import Plato.Common.Monad
import Plato.Common.Name

import qualified Plato.Syntax.Parsing as P
import qualified Plato.Syntax.Typing as T

import Control.Exception.Safe
import Control.Monad.IO.Class
import Control.Monad.RWS
import Control.Monad.Writer as Writer
import qualified Data.Map.Strict as M
import Prettyprinter

transName :: MonadThrow m => P.PsName -> m T.Expr
transName = undefined

transExpr :: MonadThrow m => P.LExpr -> m T.LExpr
transExpr (L sp exp) = L sp <$> traexpr exp
    where
        traexpr :: MonadThrow m => P.Expr -> m T.Expr
        traexpr (P.VarE (L _ (P.Unqual x))) = return $ T.VarE x
        traexpr (P.VarE (L _ (P.Qual modn x))) = return $ T.RefE (modn2name <$> modn) x
        traexpr (P.AppE e1 e2) = do
                e1' <- transExpr e1
                e2' <- transExpr e2
                return $ T.AppE e1' e2'
        traexpr (P.OpE e1 op e2) = do
                e1' <- transExpr e1
                e2' <- transExpr e2
                return $ T.AppE (T.AppE (T.VarE $ unLoc op) e1') e2'
        traexpr (P.LamE xs e1) = do
                e1' <- transExpr e1
                return $ unLoc $ foldr (\x e -> cLL x e $ T.AbsE x Nothing e) e1' xs
        traexpr (P.LetE ds e) = do
                binds <- transDecls ds -- tmp: vardecls
                e' <- transExpr e
                return $ T.LetE binds e'
        traexpr (P.CaseE e alts) = do
                e' <- transExpr e
                alts' <- execWriterT $ transAlts alts
                return $ T.CaseE e' Nothing alts'
            where
                transAlts :: MonadThrow m => [(P.LPat, P.LExpr)] -> WriterT [(T.Pat, T.Expr)] m ()
                transAlts [] = return ()
                transAlts ((pi, ei) : alts) =
                        traexpr ei >>= \ei' -> case unLoc pi of
                                P.ConP l ps -> do
                                        ps' <- mapM transPat ps
                                        tell [(T.ConP (unLoc l) ps', ei')]
                                        transAlts alts
                                P.VarP x -> tell [(T.VarP x, ei')]
                                P.WildP -> tell [(T.WildP, ei')]
        traexpr (P.FactorE e) = unreachable $ "fixity resolution failed"

transPat :: MonadThrow m => P.LPat -> m T.Pat
transPat (L _ (P.ConP c ps)) = do
        ps' <- mapM transPat ps
        return $ T.ConP c ps'
transPat (L _ (P.VarP x)) = return $ T.VarP x
transPat (L _ P.WildP) = return T.WildP

transType :: MonadThrow m => P.LType -> m T.Type
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

transDecls :: MonadThrow m => [P.LDecl] -> m T.Binds
transDecls decs = do
        (binds, sigs) <- execWriterT $
                forM decs $ \case
                        L sp (P.FuncD x args e) -> do
                                e' <- Writer.lift $ transExpr $ if null args then e else cLnL args e $ P.LamE args e
                                tell ([(x, e')], mempty)
                        L sp (P.FuncTyD x ty) -> do
                                ty' <- Writer.lift $ transType ty
                                tell (mempty, [(x, ty')])
                        _ -> return ()
        return $ T.Binds binds sigs

transTopDecl :: MonadThrow m => ModuleName -> P.LTopDecl -> WriterT ([Located T.Decl], [Located T.FuncD], [Located T.Expr]) m ()
transTopDecl modn (L sp (P.DataD name params fields)) = do
        fields' <- Writer.lift $
                forM fields $ \(l, tys) -> do
                        tys' <- mapM transType tys
                        return (l, tys')
        let fieldty = T.SumT fields'
            kn = foldr T.ArrK T.StarK (replicate (length params) T.StarK)
            bodyty = T.RecT name kn (foldr (`T.AbsT` Nothing) fieldty params)
        tell ([L sp (T.TypeD name bodyty)], [], [])
        forM_ fields' $ \(l, field) -> do
                let res_ty = foldl T.AppT (T.ConT $ toplevelName modn name) (map (T.VarT . T.BoundTv) params)
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
transTopDecl _ (L sp (P.Eval exp)) = do
        exp' <- transExpr exp
        tell ([], [], [L sp exp'])
transTopDecl _ _ = return ()

getDecls :: [P.LTopDecl] -> [P.LDecl]
getDecls tds = execWriter $
        forM tds $ \case
                L _ (P.Decl d) -> tell [d]
                _ -> return ()

ps2typ :: (MonadIO m, MonadThrow m) => P.Program -> Plato m T.Module
ps2typ (P.Program modn _ topds) = undefined {- do
                                            (tydecs, condecs, exps) <- execWriterT $ mapM_ (transTopDecl $ unLoc modn) topds
                                            (fundecs, vardecs) <- transDecls (getDecls topds)
                                            env <- gets plt_tyEnv
                                            let env' = undefined
                                            {-M.fromList
                                                    ( [(toplevelName (unLoc modn) var, ty) | T.FuncD var _ ty <- map unLoc condecs ++ fundecs]
                                                      ++ [(toplevelName (unLoc modn) var, ty) | L _ (T.VarD var ty) <- vardecs]
                                                    )
                                                    `M.union` env-}
                                            fundecs' <- mapM (typeCheck env') fundecs
                                            exps' <- forM exps $ \(L sp e) -> do
                                                    (e', ty) <- typeInfer env' e
                                                    unless (isBasicType ty) $ throwLocErr sp $ hsep ["Invalid type for evaluation expression:", pretty ty]
                                                    return (L sp e', ty)
                                            -- modify $ \s -> s{plt_tyEnv = env'}
                                            return $
                                                    T.Module
                                                            { T.typ_modn = unLoc modn
                                                            , T.typ_decls = tydecs ++ map (T.ConD <$>) condecs ++ vardecs
                                                            , T.typ_binds = fundecs'
                                                            , T.typ_body = exps'
                                                            -}