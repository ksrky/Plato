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
import Plato.KindInfer.Ki
import Plato.Parsing.FixResol
import Plato.TypeCheck.Tc

import qualified Data.Bifunctor as Bifunctor
import qualified Data.Map.Strict as M
import qualified Plato.Syntax.Parsing as P
import qualified Plato.Syntax.Typing as T

transVar :: P.PsName -> T.Expr
transVar (P.Unqual x) = T.VarE x
transVar (P.Qual modn x) = T.RefE (modn2name <$> modn) x

transExpr :: (MonadThrow m, MonadIO m) => P.LExpr -> Plato m T.LExpr
transExpr (L sp exp) = L sp <$> traexpr exp
    where
        traexpr :: (MonadThrow m, MonadIO m) => P.Expr -> Plato m T.Expr
        traexpr (P.VarE v) = return $ transVar (unLoc v)
        traexpr (P.AppE e1 e2) = do
                e1' <- transExpr e1
                e2' <- transExpr e2
                return $ T.AppE e1' e2'
        traexpr (P.OpE e1 op e2) = do
                e1' <- transExpr e1
                e2' <- transExpr e2
                return $ T.AppE (cL e1 $ T.AppE (cL op $ transVar (unLoc op)) e1') e2'
        traexpr (P.LamE xs e1) = do
                e1' <- transExpr e1
                return $ unLoc $ foldr (\x e -> cLL x e $ T.AbsE x Nothing e) e1' xs
        traexpr (P.LetE ds e) = do
                binds <- transDecls ds
                e' <- transExpr e
                return $ T.LetE binds e'
        traexpr (P.CaseE e alts) = do
                e' <- transExpr e
                alts' <- mapM transAlt alts
                return $ T.CaseE e' Nothing alts'
        traexpr P.FactorE{} = unreachable "fixity resolution failed"

transAlt :: (MonadThrow m, MonadIO m) => (P.LPat, P.LExpr) -> Plato m (T.LPat, T.LExpr)
transAlt (L sp pi, ei) = do
        ei' <- transExpr ei
        case pi of
                P.ConP con ps -> do
                        ps' <- mapM transPat ps
                        return $ case unLoc con of
                                P.Unqual c -> (L sp (T.ConP Nothing c ps'), ei')
                                P.Qual modn c -> (L sp (T.ConP (Just modn) c ps'), ei')
                P.VarP x -> return (L sp (T.VarP x), ei')
                P.WildP -> return (L sp T.WildP, ei')

transPat :: MonadThrow m => P.LPat -> m T.LPat
transPat (L sp (P.ConP con ps)) = do
        ps' <- mapM transPat ps
        return $ case unLoc con of
                P.Unqual c -> L sp (T.ConP Nothing c ps')
                P.Qual modn c -> L sp (T.ConP (Just modn) c ps')
transPat (L sp (P.VarP x)) = return $ L sp (T.VarP x)
transPat (L sp P.WildP) = return $ L sp T.WildP

transType :: MonadThrow m => P.LType -> m T.LType
transType (L sp ty) = L sp <$> tratype ty
    where
        tratype :: MonadThrow m => P.Type -> m T.Type
        tratype (P.VarT x) = return $ T.VarT (T.BoundTv x)
        tratype (P.ConT (L _ (P.Unqual c))) = return $ T.ConT c
        tratype (P.ConT (L _ (P.Qual modn c))) = return $ T.RefT (modn2name <$> modn) c
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

transDecls :: (MonadThrow m, MonadIO m) => [P.LDecl] -> Plato m T.Binds
transDecls decs = do
        (binds, sigs) <- execWriterT $
                forM decs $ \case
                        L _ (P.FuncD x args exp) -> do
                                fixenv <- gets plt_fixityEnv
                                exp_r <- resolveFixity fixenv exp
                                let exp_c = if null args then exp_r else cLnL args exp_r $ P.LamE args exp_r
                                exp' <- Writer.lift $ transExpr exp_c
                                tell ([(x, exp')], mempty)
                        L _ (P.FuncTyD x ty) -> do
                                ty' <- Writer.lift $ transType ty
                                tell (mempty, [(x, ty')])
                        _ -> return ()
        tyenv <- gets plt_tyEnv
        knenv <- gets plt_knEnv
        let tyenv' = tyenv `M.union` M.fromList (map (Bifunctor.bimap unLoc unLoc) sigs)
        modify $ \ps -> ps{plt_tyEnv = tyenv'}
        binds' <- forM binds $ \(x, exp) ->
                case lookup x sigs of
                        Nothing -> throwLocErr (getLoc x) $ "no type signature for" <+> squotes (pretty x)
                        Just ty -> do
                                ty' <- lift $ checkKindStar knenv ty
                                exp' <- checkType tyenv' exp (unLoc ty')
                                return (x, exp')
        return $ T.Binds binds' sigs

transTopDecl :: (MonadThrow m, MonadIO m) => P.LTopDecl -> WriterT [T.Decl] (Plato m) ()
transTopDecl (L sp (P.DataD name params fields)) = do
        fields' <- forM fields $ \(l, tys) -> do
                tys' <- mapM transType tys
                return (l, tys')
        let sum_ty = L sp $ T.SumT fields'
        let body_ty = L sp $ T.RecT name Nothing (foldr (\x ty -> L sp $ T.AbsT x Nothing ty) sum_ty params)
        knenv <- gets plt_knEnv
        (body_ty', kn) <- inferKind knenv body_ty
        modify $ \ps -> ps{plt_knEnv = M.insert (unLoc name) kn knenv}
        tell [T.TypeD name (unLoc body_ty') kn]
        con_sigs <- mapM transCon fields'
        tell (map (\(lab, sig) -> T.ConD lab (unLoc sig)) con_sigs)
    where
        res_ty =
                foldl
                        (\ty1 ty2 -> cLL ty1 ty2 $ T.AppT ty1 ty2)
                        (cL name $ T.ConT name)
                        (map (\p -> cL p $ T.VarT $ T.BoundTv p) params)
        quantify :: T.LType -> T.LType
        quantify ty | null params = ty
        quantify ty = cL ty $ T.AllT (map (\x -> (T.BoundTv x, Nothing)) params) ty
        transCon :: Monad m => (T.LName, [T.LType]) -> m (T.LName, T.LType)
        transCon (lab, tys) = do
                let rho_ty = foldr (\ty1 ty2 -> cLL ty1 ty2 $ T.ArrT ty1 ty2) res_ty tys
                return (lab, quantify rho_ty)
transTopDecl _ = return ()

transEvals :: (MonadThrow m, MonadIO m) => [Located P.TopDecl] -> Plato m [(T.LExpr, T.Type)]
transEvals topds = do
        let exps = [exp | L _ (P.Eval exp) <- topds]
        tyenv <- gets plt_tyEnv
        forM exps $ \exp -> do
                exp' <- transExpr exp
                inferType tyenv exp'

ps2typ :: (MonadIO m, MonadThrow m) => P.Program -> Plato m T.Module
ps2typ (P.Program modn _ topds) = do
        decls <- execWriterT $ mapM_ transTopDecl topds
        binds <- transDecls [d | L _ (P.Decl d) <- topds]
        body <- transEvals topds
        return $
                T.Module
                        { T.typ_modn = modn
                        , T.typ_decls = decls
                        , T.typ_binds = binds
                        , T.typ_body = body
                        }