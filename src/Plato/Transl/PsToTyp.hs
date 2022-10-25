{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ViewPatterns #-}

module Plato.Transl.PsToTyp where

import Plato.Common.Error
import Plato.Common.Name
import Plato.Common.SrcLoc
import qualified Plato.Syntax.Parsing as P
import qualified Plato.Syntax.Typing as T
import Plato.Typing.TcMonad
import Plato.Typing.TcTypes
import Plato.Typing.TypeCheck

import Control.Exception.Safe
import Control.Monad.IO.Class
import Control.Monad.Writer as Writer
import Data.List ((\\))
import qualified Data.Text as T

transExpr :: MonadThrow m => Located P.Expr -> m (Located T.Expr)
transExpr = traexpr
    where
        traexpr :: MonadThrow m => Located P.Expr -> m (Located T.Expr)
        traexpr (L sp (P.VarE x)) = return $ L sp (T.VarE x)
        traexpr (L sp (P.AppE e1 e2)) = do
                e1' <- traexpr e1
                e2' <- traexpr e2
                return $ L sp (T.AppE e1' e2')
        traexpr (L sp (P.OpE e1 op e2)) = do
                e1' <- traexpr e1
                e2' <- traexpr e2
                return $ L sp (T.AppE (L sp $ T.AppE (noLoc $ T.VarE op) e1') e2')
        traexpr (L sp (P.LamE xs e1)) = do
                e1' <- traexpr e1
                return $ foldr (\x e -> cLL x e $ T.AbsE x Nothing e) e1' xs
        traexpr (L sp (P.LetE ds e)) = do
                (fds, _) <- transDecls ds
                e' <- transExpr e
                return $ L sp (T.LetE fds e')
        traexpr (L sp (P.CaseE e alts)) = do
                e' <- traexpr e
                alts' <-
                        execWriterT $
                                let transAlts :: MonadThrow m => [(Located P.Pat, Located P.Expr)] -> WriterT [(Located T.Pat, Located T.Expr)] m ()
                                    transAlts [] = return ()
                                    transAlts (alt@(pi, ei) : alts) =
                                        traexpr ei >>= \ei' -> case unLoc pi of
                                                P.ConP l ps -> do
                                                        ps' <- mapM transPat ps
                                                        tell [(L (getSpan pi) $ T.ConP l ps', ei')]
                                                        transAlts alts
                                                P.VarP x -> tell [(L sp $ T.VarP x, ei')]
                                                P.WildP -> tell [(L sp T.WildP, ei')]
                                 in transAlts alts
                return $ L sp (T.CaseE e' Nothing alts')
        traexpr (L _ (P.FactorE _)) = unreachable "fixity resolution failed"

transPat :: MonadThrow m => Located P.Pat -> m (Located T.Pat)
transPat (L sp (P.ConP c ps)) = do
        ps' <- mapM transPat ps
        return $ L sp (T.ConP c ps')
transPat (L sp (P.VarP x)) = do
        return $ L sp (T.VarP x)
transPat (L sp P.WildP) = do
        return $ L sp T.WildP

transType :: MonadThrow m => Located P.Type -> m (Located T.Type)
transType (L sp (P.VarT x)) = return $ L sp (varType x)
transType (L sp (P.ConT x)) = return $ L sp (T.ConT x)
transType (L sp (P.AppT ty1 ty2)) = do
        ty1' <- transType ty1
        ty2' <- transType ty2
        return $ L sp (T.AppT ty1' ty2')
transType (L sp (P.ArrT ty1 ty2)) = do
        ty1' <- transType ty1
        ty2' <- transType ty2
        return $ L sp (T.ArrT ty1' ty2')
transType (L sp (P.AllT xs ty1)) = do
        ty1' <- transType ty1
        return $ L sp $ T.AllT (map (\x -> (BoundTv <$> x, Nothing)) xs) ty1'

transDecls :: MonadThrow m => [Located P.Decl] -> m ([T.FuncDecl], [Located T.Decl])
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
                        -- tmp: | level st > 0 -> Writer.lift $ throwTypError sp "Variables should be declared in the top level"
                        _ -> return ()
        fields <- execWriterT $
                forM decs $ \case
                        L sp (P.FuncD x args e)
                                | unLoc x `elem` ftns -> do
                                        e' <- Writer.lift $ transExpr $ if null args then e else cLnL args e $ P.LamE args e
                                        tell [(x, e')]
                                | otherwise -> do
                                        Writer.lift $ throwLocatedErr sp "lacks type signature"
                        _ -> return ()
        when (length fields /= length fieldtys) $ throwUnexpectedErr "number of function bodies and signatures are not match"
        return ([T.FD var1 exp ty | (var1, exp) <- fields, (var2, ty) <- fieldtys, unLoc var1 == unLoc var2], vardecls)

transFuncTyDecls :: MonadThrow m => [Located P.Decl] -> m [Located T.Decl]
transFuncTyDecls decs = do
        let xs = [unLoc x | (unLoc -> P.FuncD x _ _) <- decs]
        execWriterT $
                forM decs $ \case
                        (L sp (P.FuncTyD x ty)) | unLoc x `notElem` xs -> do
                                ty' <- Writer.lift $ transType ty
                                tell [L sp $ T.VarD x ty']
                        _ -> return ()

transTopDecl :: MonadThrow m => Located P.TopDecl -> WriterT ([Located T.Decl], [Located T.FuncDecl]) m ()
transTopDecl (L sp (P.DataD name params fields)) = do
        fields' <- Writer.lift $
                forM fields $ \(l, tys) -> do
                        tys' <- mapM transType tys
                        return (l, tys')
        let fieldty = cLLn (fst $ head fields) (snd $ last fields) $ T.SumT fields'
        tell ([L sp (T.TypeD name (foldr (\x ty -> cLL x ty $ T.AbsT x Nothing ty) fieldty params))], [])
        forM_ fields' $ \(l, field) -> do
                let res_ty = foldl (\ty1 ty2 -> cLL ty1 ty2 $ T.AppT ty1 ty2) (cL name $ T.ConT name) (map (\x -> cL x $ varType x) params)
                    rho_ty = foldr (\ty1 ty2 -> cLL ty1 ty2 $ T.ArrT ty1 ty2) res_ty field
                    sigma_ty =
                        if null params
                                then rho_ty
                                else cLnL params rho_ty $ T.AllT (map (\tv -> (BoundTv <$> tv, Nothing)) params) rho_ty
                    tyargs = params
                    args = map (noLoc . str2varName . show) [length params + 1 .. length params + length field]
                    tag = cLLn l args $ T.TagE l (map (\x -> cL x $ T.VarE x) args) (Just $ unLoc res_ty)
                    exp = foldr (\(x, ty) e -> cLL x e $ T.AbsE x (Just $ unLoc ty) e) tag (zip args field)
                    exp' = cLnL tyargs exp $ T.TAbsE tyargs exp
                tell ([], [L sp $ T.FD l exp sigma_ty])
transTopDecl (L sp (P.TypeD name params ty1)) = do
        ty1' <- Writer.lift $ transType ty1
        tell ([L sp (T.TypeD name (foldr (\x ty -> cLL x ty $ T.AbsT x Nothing ty) ty1' params))], [])
transTopDecl _ = return ()

getDecls :: [Located P.TopDecl] -> [Located P.Decl]
getDecls tds = execWriter $
        forM tds $ \case
                L _ (P.Decl d) -> tell [d]
                _ -> return ()

processDecls :: (MonadIO m, MonadThrow m) => [T.FuncDecl] -> [T.FuncDecl] -> m [T.FuncDecl]
processDecls decs binds = do
        let env = map (\(T.FD var body ty) -> (unLoc var, unLoc ty)) (decs ++ binds)
        forM binds $ \fd -> typeRecon env fd

ps2typ :: (MonadIO m, MonadThrow m) => P.Program -> m T.Decls
ps2typ (P.Program modn ids tds) = do
        let imps = map (\(L _ (P.ImpDecl mn)) -> mn) ids
        (tydecs, condecs) <- execWriterT $ mapM_ transTopDecl tds
        (fundecs, vardecs) <- transDecls (getDecls tds)
        fundecs' <- processDecls (map unLoc condecs) fundecs
        return $ T.Decls modn imps (tydecs ++ map (T.FuncD <$>) condecs ++ vardecs) fundecs'
