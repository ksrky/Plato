{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ViewPatterns #-}

module Plato.Translation.PsToTyp where

import Plato.Common.Error
import Plato.Common.Name
import Plato.Common.SrcLoc
import qualified Plato.Syntax.Parsing as P
import qualified Plato.Syntax.Typing as T
import Plato.Typing.Rename
import Plato.Typing.Types

import Control.Exception.Safe
import Control.Monad.Writer as Writer
import Data.List ((\\))
import qualified Data.Text as T
import Plato.Typing.Monad
import Plato.Typing.TypeCheck

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
                fds <- transDecls ds
                e' <- transExpr e
                return $ L sp (T.LetE fds e')
        traexpr (L sp (P.CaseE e alts)) = undefined
        traexpr (L _ (P.FactorE _)) = unreachable "fixity resolution failed"

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
        return $ L sp $ T.AllT (map (BoundTv <$>) xs) ty1'

transDecls :: MonadThrow m => [Located P.Decl] -> m [T.FuncDecl]
transDecls decs = do
        ns <- getFuncNames decs
        fieldtys <- execWriterT $
                forM decs $ \case
                        (L sp (P.FuncTyD x ty))
                                | unLoc x `elem` ns -> do
                                        ty' <- Writer.lift $ transType ty
                                        tell [(x, ty')]
                        -- level st > 0 -> Writer.lift $ throwTypError sp "Variables should be declared in the top level"
                        _ -> return ()
        fields <- execWriterT $
                forM decs $ \case
                        L sp (P.FuncD x xs e) | unLoc x `notElem` ns -> do
                                Writer.lift $ throwLocatedErr sp "lacks type signature"
                        L sp (P.FuncD x xs e) -> do
                                e' <- Writer.lift $ transExpr (cLnL xs e $ P.LamE xs e)
                                tell [(x, e')]
                        _ -> return ()
        when (length fields /= length fieldtys) $ throwUnexpectedErr "number of function bodies and signatures are not match"
        return [T.FD var1 exp ty | (var1, exp) <- fields, (var2, ty) <- fieldtys, var1 == var2]

transFuncTyDecls :: MonadThrow m => [Located P.Decl] -> m [Located T.Decl]
transFuncTyDecls decs = do
        xs <- getFuncNames decs
        execWriterT $
                forM decs $ \case
                        (L sp (P.FuncTyD x ty)) | unLoc x `notElem` xs -> do
                                ty' <- Writer.lift $ transType ty
                                tell [L sp $ T.VarD x ty']
                        _ -> return ()

getFuncNames :: MonadThrow m => [Located P.Decl] -> m [Name]
getFuncNames decs = execWriterT $
        forM decs $ \case
                (unLoc -> P.FuncD x _ _) -> tell [unLoc x]
                _ -> return ()

transTopDecl :: MonadThrow m => Located P.TopDecl -> WriterT ([Located T.Decl], [Located T.Decl]) m ()
transTopDecl (L sp (P.DataD name params fields)) = do
        fields' <- Writer.lift $
                forM fields $ \(l, tys) -> do
                        tys' <- mapM transType tys
                        return (l, tys')
        let fieldty = cLLn (fst $ head fields) (snd $ last fields) $ T.SumT fields'
        tell ([L sp (T.TypeD name (foldr (\x ty -> cLL x ty $ T.AbsT x ty) fieldty params))], [])
        forM_ fields' $ \(l, field) -> do
                let sumty = foldl (\ty1 ty2 -> cLL ty1 ty2 $ T.AppT ty1 ty2) (cL name $ T.ConT name) (map (\x -> cL x $ varType x) params)
                    bodyty = foldr (\ty1 ty2 -> cLL ty1 ty2 $ T.ArrT ty1 ty2) bodyty field
                    ty = cLnL params bodyty $ T.AllT (map (BoundTv <$>) params) bodyty
                    tyargs = map (noLoc . str2tyvarName . show) [1 .. length params]
                    args = map (noLoc . varName . T.pack . show) [length params + 1 .. length params + length field]
                    tag = cLLn l args $ T.TagE l (map (\x -> cL x $ T.VarE x) args) Nothing
                    exp = foldr (\x e -> cLL x e $ T.AbsE x Nothing e) tag (tyargs ++ args)
                tell ([], [L sp $ T.FuncD $ T.FD l exp ty])
transTopDecl (L sp (P.TypeD name params ty1)) = do
        ty1' <- Writer.lift $ transType ty1
        tell ([L sp (T.TypeD name (foldr (\x ty -> cLL x ty $ T.AbsT x ty) ty1' params))], [])
transTopDecl _ = return ()

getDecls :: [Located P.TopDecl] -> [Located P.Decl]
getDecls tds = execWriter $
        forM tds $ \case
                L _ (P.Decl d) -> tell [d]
                _ -> return ()

ps2typ :: MonadThrow m => P.Program -> m T.Decls
ps2typ (P.Program _ ids tds) = do
        let modns = map (\(L _ (P.ImpDecl mn)) -> mn) ids
        (tydecls, condecls) <- execWriterT $ mapM_ transTopDecl tds
        vardecls <- transFuncTyDecls (getDecls tds)
        binds <- transDecls (getDecls tds)
        return $ T.Decls modns (tydecls ++ condecls ++ vardecls) binds

tyrecon env fd = typeRecon env fd