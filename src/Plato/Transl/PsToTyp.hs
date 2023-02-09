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
import qualified Data.Map.Strict as M
import qualified Plato.Syntax.Parsing as P
import qualified Plato.Syntax.Typing as T

transName :: P.LPsName -> T.LTypName
transName (L sp (P.Unqual x)) = L sp (T.TypName [] x)
transName (L sp1 (P.Qual (L sp2 modn) x)) = L sp1 (T.TypName (map (L sp2) (modn2names modn)) x)

typName :: P.LName -> Located T.TypName
typName x@(L sp _) = L sp (T.TypName (map (L sp) []) x)

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
                binds <- transDecls ds
                -- temp: extend env
                e' <- transExpr e
                return $ T.LetE binds e'
        traexpr (P.CaseE e alts) = do
                e' <- transExpr e
                alts' <- mapM transAlt alts
                return $ T.CaseE e' Nothing alts'
        traexpr P.FactorE{} = unreachable "fixity resolution failed"

transAlt :: (MonadThrow m, MonadIO m) => (P.LPat, P.LExpr) -> Typ m ([T.LPat], T.LExpr)
transAlt (L sp pi, ei) = do
        ei' <- transExpr ei
        case pi of
                P.ConP con ps -> do
                        ps' <- mapM transPat ps
                        return ([L sp (T.ConP (transName con) ps')], ei')
                P.VarP x -> return ([L sp (T.VarP x)], ei')
                P.WildP -> return ([L sp T.WildP], ei')

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

transDecls :: (MonadThrow m, MonadIO m) => [P.LDecl] -> Typ m T.Binds
transDecls decs = do
        (binds, sigs) <- execWriterT $
                forM decs $ \case
                        L _ (P.FuncD x args exp) -> do
                                -- fixenv <- gets plt_fixityEnv --temp
                                -- exp_r <- resolveFixity fixenv exp
                                let exp_c = if null args then exp_r else cLnL args exp_r $ P.LamE args exp_r
                                exp' <- Writer.lift $ transExpr exp_c
                                tell ([(x, exp')], mempty)
                        L _ (P.FuncTyD x ty) -> do
                                ty' <- Writer.lift $ transType ty
                                tell (mempty, [(x, ty')])
                        _ -> return ()
        let extend = extendEnvList (map (Bifunctor.second unLoc) sigs)
        -- modify $ \ps -> ps{plt_tyEnv = tyenv'}
        (binds', sigs') <- local extend $
                execWriterT $
                        forM binds $ \(x, exp) ->
                                case lookup x sigs of
                                        Nothing -> throwLocErr (getLoc x) $ "no type signature for" <+> squotes (pretty x)
                                        Just ty -> do
                                                ty' <- undefined $ checkKindStar ty
                                                exp' <- undefined $ checkType exp (unLoc ty')
                                                tell ([(x, exp')], [(x, unLoc ty')])
        return $ T.Binds binds' sigs'

transTopDecl :: (MonadThrow m, MonadIO m) => P.LTopDecl -> WriterT [T.Decl] (Plato m) ()
transTopDecl (L sp (P.DataD name params fields)) = do
        fields' <- forM fields $ \(l, tys) -> do
                tys' <- mapM transType tys
                return (l, tys')
        let sum_ty = L sp $ T.SumT fields'
        let body_ty = L sp $ T.RecT name Nothing (foldr (\x ty -> L sp $ T.AbsT x Nothing ty) sum_ty params)
        (body_ty', kn) <- undefined $ inferKind body_ty
        lift $ modifyTypEnv $ extendEnv name kn
        tell [T.TypeD name (unLoc body_ty') kn]
        con_sigs <- mapM transCon fields'
        tell (map (\(lab, sig) -> T.ConD lab (unLoc sig)) con_sigs)
    where
        res_ty =
                foldl
                        (\ty1 ty2 -> cLL ty1 ty2 $ T.AppT ty1 ty2)
                        (cL name $ T.ConT $ typName name)
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
        T.TypEnv tyenv _ _ <- gets plt_typEnv
        forM exps $ \exp -> do
                exp' <- transExpr exp
                inferType tyenv exp'

ps2typ :: (MonadIO m, MonadThrow m) => P.Program -> Plato m T.Module
ps2typ (P.Program modn _ topds) = do
        decls <- execWriterT $ mapM_ transTopDecl topds
        (binds, body) <- ps2typ'
        return $
                T.Module
                        { T.typ_modn = modn
                        , T.typ_decls = decls
                        , T.typ_binds = binds
                        , T.typ_body = body
                        }
    where
        ps2typ' :: (MonadIO m, MonadThrow m) => Typ m (T.Binds, [(T.LExpr, T.Type)])
        ps2typ' = do
                binds <- transDecls [d | L _ (P.Decl d) <- topds]
                body <- transEvals topds
                return (binds, body)