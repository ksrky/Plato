{-# LANGUAGE LambdaCase #-}

module Plato.Transl.PsToTyp where

import Control.Exception.Safe
import Control.Monad.IO.Class
import Control.Monad.RWS
import Control.Monad.Writer as Writer
import Prettyprinter

import Plato.Common.Error
import Plato.Common.Location
import Plato.Common.Monad
import Plato.Common.Name
import Plato.KindCheck.Kc
import Plato.Parsing.FixResol
import Plato.TypeCheck.Tc
import Plato.Typing.Env
import Plato.Typing.Monad

import qualified Data.Bifunctor as Bifunctor
import qualified Plato.Syntax.Parsing as P
import qualified Plato.Syntax.Typing as T

transName :: P.LPsName -> T.LPath
transName (L sp (P.Unqual x)) = L sp (T.Path [] x)
transName (L sp1 (P.Qual (L sp2 modn) x)) = L sp1 (T.Path (map (L sp2) (modn2names modn)) x)

name2path :: P.LName -> Located T.Path
name2path x@(L sp _) = L sp (T.Path (map (L sp) []) x)

transExpr :: (MonadThrow m, MonadIO m) => P.LExpr -> Typ m T.LExpr
transExpr (L sp exp) = L sp <$> traexpr exp
    where
        traexpr :: (MonadThrow m, MonadIO m) => P.Expr -> Typ m T.Expr
        traexpr (P.VarE v) = return $ T.VarE $ transName v
        traexpr (P.AppE e1 e2) = do
                e1' <- transExpr e1
                e2' <- transExpr e2
                return $ T.AppE e1' e2'
        traexpr (P.OpE e1 op e2) = do
                e1' <- transExpr e1
                e2' <- transExpr e2
                return $ T.AppE (cL e1 $ T.AppE (cL op $ T.VarE $ transName op) e1') e2'
        traexpr (P.LamE xs e1) = do
                e1' <- transExpr e1
                return $ unLoc $ foldr (\x e -> cLL x e $ T.AbsE x Nothing e) e1' xs
        traexpr (P.LetE ds e) = do
                (bnds, decs) <- transDecls ds
                -- temp: extend env
                e' <- transExpr e
                return $ T.LetE bnds decs e'
        traexpr (P.CaseE e alts) = do
                e' <- transExpr e
                alts' <- mapM transAlt alts
                return $ T.CaseE e' Nothing alts'
        traexpr P.FactorE{} = unreachable "fixity resolution failed"

transAlt :: (MonadThrow m, MonadIO m) => (P.LPat, P.LExpr) -> Typ m (T.LPat, T.LExpr)
transAlt (L sp pi, ei) = do
        ei' <- transExpr ei
        case pi of
                P.ConP con ps -> do
                        ps' <- mapM (lift . transPat) ps
                        return (L sp (T.ConP (transName con) ps'), ei')
                P.VarP x -> return (L sp (T.VarP x), ei')
                P.WildP -> return (L sp T.WildP, ei')

transPat :: MonadThrow m => P.LPat -> m T.LPat
transPat (L sp (P.ConP con ps)) = do
        ps' <- mapM transPat ps
        return $ L sp (T.ConP (transName con) ps')
transPat (L sp (P.VarP x)) = return $ L sp (T.VarP x)
transPat (L sp P.WildP) = return $ L sp T.WildP

transType :: MonadThrow m => P.LType -> m T.LType
transType (L sp ty) = L sp <$> tratype ty
    where
        tratype :: MonadThrow m => P.Type -> m T.Type
        tratype (P.VarT x) = return $ T.VarT (T.BoundTv x)
        tratype (P.ConT c) = return $ T.ConT (transName c)
        tratype (P.AppT ty1 ty2) = do
                ty1' <- transType ty1
                ty2' <- transType ty2
                return $ T.AppT ty1' ty2'
        tratype (P.ArrT ty1 ty2) = do
                ty1' <- transType ty1
                ty2' <- transType ty2
                return $ T.ArrT ty1' ty2'
        tratype (P.AllT xs ty1) = do
                ty1' <- transType ty1
                return $ T.AllT (map (\x -> (T.BoundTv x, Nothing)) xs) ty1'

transDecls :: (MonadThrow m, MonadIO m) => [P.LDecl] -> Typ m (T.Binds, T.Decls)
transDecls decs = do
        (binds, sigs) <- execWriterT $
                forM decs $ \case
                        L _ (P.FuncD x args exp) -> do
                                -- fixenv <- gets plt_fixityEnv --temp
                                -- exp_r <- resolveFixity fixenv exp
                                let exp' = if null args then exp else cLnL args exp $ P.LamE args exp
                                exp'' <- lift $ transExpr exp'
                                tell ([(x, exp'')], mempty)
                        L _ (P.FuncTyD x ty) -> do
                                ty' <- lift $ lift $ transType ty
                                tell (mempty, [(x, ty')])
                        _ -> return ()
        let extend = extendEnvList (map (Bifunctor.second unLoc) sigs)
        -- modify $ \ps -> ps{plt_tyEnv = tyenv'}
        (binds', sigs') <- local extend $
                execWriterT $
                        forM binds $ \(x, exp) ->
                                case lookup x sigs of
                                        Nothing -> lift $ lift $ throwLocErr (getLoc x) $ "no type signature for" <+> squotes (pretty x)
                                        Just ty -> do
                                                ty' <- lift $ checkKindStar ty
                                                exp' <- lift $ checkType exp (unLoc ty')
                                                tell ([(x, exp')], [(x, unLoc ty')])
        return (binds', sigs')

transTopDecl :: (MonadThrow m, MonadIO m) => P.LTopDecl -> WriterT (T.TypDecls, T.Binds, T.Decls) (Typ m) ()
transTopDecl (L sp (P.DataD name params fields)) = do
        fields' <- forM fields $ \(l, tys) -> do
                tys' <- mapM (lift . lift . transType) tys
                return (l, tys')
        let sum_ty = L sp $ T.SumT fields'
        let body_ty = foldr (\x ty -> L sp $ T.AbsT x Nothing ty) sum_ty params
        (_, kn) <- lift $ inferKind body_ty
        -- lift $ modifyTypEnv $ extendEnv name kn
        tell ([(name, kn)], [], [])
        con_sigs <- mapM transCon fields'
        tell ([], [], map (\(lab, sig) -> (lab, unLoc sig)) con_sigs)
    where
        res_ty =
                foldl
                        (\ty1 ty2 -> cLL ty1 ty2 $ T.AppT ty1 ty2)
                        (cL name $ T.ConT $ name2path name)
                        (map (\p -> cL p $ T.VarT $ T.BoundTv p) params)
        quantify :: T.LType -> T.LType
        quantify ty | null params = ty
        quantify ty = cL ty $ T.AllT (map (\x -> (T.BoundTv x, Nothing)) params) ty
        transCon :: Monad m => (T.LName, [T.LType]) -> m (T.LName, T.LType)
        transCon (lab, tys) = do
                let rho_ty = foldr (\ty1 ty2 -> cLL ty1 ty2 $ T.ArrT ty1 ty2) res_ty tys
                return (lab, quantify rho_ty)
transTopDecl _ = return ()

transTopDecls :: (MonadThrow m, MonadIO m) => [P.LTopDecl] -> WriterT (T.TypDecls, T.Binds, T.Decls) (Typ m) ()
transTopDecls decs = do
        (fields, tybnds) <- lift $ mconcat <$> mapM transData decs
        tell (tybnds, [], [])
        local (extendEnvList tybnds) $ do
                let res_ty =
                        foldl
                                (\ty1 ty2 -> cLL ty1 ty2 $ T.AppT ty1 ty2)
                                (cL name $ T.ConT $ name2path name)
                                (map (\p -> cL p $ T.VarT $ T.BoundTv p) params)
                    quantify :: T.LType -> T.LType
                    quantify ty | null params = ty
                    quantify ty = cL ty $ T.AllT (map (\x -> (T.BoundTv x, Nothing)) params) ty
                    transCon :: Monad m => (T.LName, [T.LType]) -> m (T.LName, T.LType)
                    transCon (lab, tys) = do
                        let rho_ty = foldr (\ty1 ty2 -> cLL ty1 ty2 $ T.ArrT ty1 ty2) res_ty tys
                        return (lab, quantify rho_ty)
                con_tys <- mapM transCon fields
                tell ([], [], con_tys)
    where
        transData :: (MonadThrow m, MonadIO m) => P.LTopDecl -> Typ m ([(T.LName, [T.LType])], [(T.LName, T.Kind)])
        transData (L sp (P.DataD name params fields)) = do
                fields' <- forM fields $ \(l, tys) -> do
                        tys' <- mapM (lift . transType) tys
                        return (l, tys')
                let sum_ty = L sp $ T.SumT fields'
                let body_ty = foldr (\x ty -> L sp $ T.AbsT x Nothing ty) sum_ty params
                (_, kn) <- inferKind body_ty
                return (fields', [(name, kn)])
        transData _ = return ([], [])

transEvals :: (MonadThrow m, MonadIO m) => [Located P.TopDecl] -> Typ m [T.Expr]
transEvals topds = do
        let exps = [exp | L _ (P.Eval exp) <- topds]
        res <- forM exps $ \exp -> do
                exp' <- transExpr exp
                inferType exp'
        return $ map (unLoc . fst) res

ps2typ :: (MonadIO m, MonadThrow m) => P.Program -> Plato m T.Module
ps2typ (P.Program modn _ topds) = do
        fixenv <- gets plt_fixityEnv
        let resolveDecls :: MonadThrow m => [P.LDecl] -> m [P.LDecl]
            resolveDecls decs = do
                concat
                        <$> forM
                                decs
                                ( \case
                                        L sp (P.FuncD x args exp) -> do
                                                exp' <- resolveFixity fixenv exp
                                                return [L sp (P.FuncD x args exp')]
                                        _ -> return []
                                )
            ps2typ' :: (MonadIO m, MonadThrow m) => Typ m (T.TypDecls, T.Binds, T.Decls, [T.Expr])
            ps2typ' = do
                (tydecs, bnds1, decs1) <- execWriterT $ mapM_ transTopDecl topds
                (bnds2, decs2) <- (lift . resolveDecls >=> transDecls) [d | L _ (P.Decl d) <- topds]
                body <- transEvals topds
                return (tydecs, bnds1 ++ bnds2, decs1 ++ decs2, body)
        typenv <- gets plt_typEnv
        (tydecs, bnds, decs, body) <- runTyp ps2typ' =<< initContext typenv
        let typenv' = newTypEnv modn typenv -- temp: updating typenv
        modify $ \ps -> ps{plt_typEnv = typenv'}
        return $
                T.Module
                        { T.typ_modn = modn
                        , T.typ_binds = bnds
                        , T.typ_decls = decs
                        , T.typ_typdecs = tydecs
                        , T.typ_body = body
                        }